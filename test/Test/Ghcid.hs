{-# LANGUAGE CPP, RecordWildCards #-}

-- | Test behavior of the executable, polling files for changes
module Test.Ghcid(ghcidTest, ghcidChild) where

import Control.Concurrent.Async
import Control.Concurrent.Extra
import Control.Exception.Extra
import Control.Monad.Extra
import Data.Char
import Data.List.Extra
import Data.IORef
import Data.Maybe
import qualified Data.ByteString as BS
import System.Directory.Extra
import System.IO.Extra
import System.Time.Extra
import Data.Version.Extra
import System.Environment
import System.Process.Extra
import System.FilePath
import System.Exit
#if !defined(mingw32_HOST_OS)
import Text.Read
import System.IO.Error
import System.Posix.Files (createLink)
import System.Posix.Signals
import System.Posix.Types
#endif

import Test.Tasty
import Test.Tasty.HUnit

import Ghcid (TermSize(..), mainWithTerminal, shouldWatchLoadConfig)
import Language.Haskell.Ghcid.Escape
import Language.Haskell.Ghcid.Util
import Test.Common
import Test.Util
import Data.Functor
import Prelude


ghcidTest :: TestTree
ghcidTest = testGroup "Ghcid test"
    [basicTest
    ,cdTest
    ,dotGhciTest
    ,loadConfigRestartFilterTest
    ,cabalTest
    ,cabalRestartTest
    ,noModulesTest
    ,missingWatchPathTest
    ,brokenStartupTest
    ,brokenStartupOutputTest
    ,outputDirectoryTest
    ,generationTest
#if !defined(mingw32_HOST_OS)
    ,sigtermTest
    ,missedChangeTest
#endif
    ]


-- | A test's private directory, where its ghcid runs.
data Sandbox = Sandbox
    {sandboxDir :: FilePath
    ,stagingDir :: FilePath -- ^ Outside the sandbox on the same file system, where 'write' prepares files
    }

-- | A path inside the sandbox.
path :: Sandbox -> FilePath -> FilePath
path sb x = sandboxDir sb </> x

freshDir :: (Sandbox -> IO a) -> IO a
freshDir act = withTempDir $ \tmp -> do
    let sb = Sandbox (tmp </> "sandbox") (tmp </> "staging")
    mapM_ createDirectory [sandboxDir sb, stagingDir sb]
    act sb

copyDir :: FilePath -> (Sandbox -> IO a) -> IO ()
copyDir dir act = do
    b <- doesDirectoryExist dir
    if not b then putStrLn $ "Couldn't run test because test source is missing, " ++ dir else void $
        freshDir $ \sb -> do
            xs <- map (makeRelative dir) <$> listFilesRecursive dir
            forM_ xs $ \x -> do
                createDirectoryIfMissing True $ takeDirectory $ path sb x
                copyFile (dir </> x) (path sb x)
            act sb


whenExecutable :: String -> IO a -> IO ()
whenExecutable exe act = do
    v <- findExecutable exe
    case v of
        Nothing -> putStrLn $ "Couldn't run test because " ++ exe ++ " is missing"
        Just _ -> void act


-- Note [Ghcid under test]
-- ~~~~~~~~~~~~~~~~~~~~~~~
-- Each test runs ghcid as a child process -- this test executable with
-- GHCID_TEST_CHILD set -- inside the test's sandbox, so the process-wide state
-- ghcid owns (current directory, arguments, signal handlers) is the test's alone
-- and tests can run in parallel. The child writes every screen it draws to stdout,
-- each line tagged with 'screenLine' and the screen closed by 'screenEnd'; all
-- other text is ghcid's own logging. Closing the child's stdin stops it, letting
-- its cleanup run.

-- | Tags a line of a screen the child drew. See Note [Ghcid under test]
screenLine :: String
screenLine = "\0|"

-- | Closes a screen the child drew. See Note [Ghcid under test]
screenEnd :: String
screenEnd = "\0."

-- | Run as the ghcid under test. See Note [Ghcid under test]
ghcidChild :: IO ()
ghcidChild = do
    hSetEncoding stdout utf8
    main <- myThreadId
    withAsync (BS.hGetContents stdin >> throwTo main ExitSuccess) $ \_ ->
        mainWithTerminal (pure $ TermSize 100 (Just 50) WrapHard) $ \screen ->
            outStr $ unlines $ map (screenLine ++) (concatMap lines screen) ++ [screenEnd]

-- | A running ghcid under test.
data Child = Child
    {childProcess :: ProcessHandle
    ,childScreens :: Chan (Maybe [String]) -- ^ Each screen drawn, then Nothing once it stops writing
    ,childLog :: IORef [String] -- ^ Everything else it wrote, newest first
    }

-- | Run ghcid with the given arguments in a sandbox, stopping it at the end.
--   See Note [Ghcid under test]
withChild :: Sandbox -> [String] -> (Child -> IO a) -> IO a
withChild sb args act = do
    exe <- getExecutablePath
    env <- getEnvironment
    let child = (proc exe args)
            {cwd = Just $ sandboxDir sb
            ,env = Just $ ("GHCID_TEST_CHILD","1") : env
            ,std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe}
    withCreateProcess child $ \(Just inp) (Just out) (Just err) childProcess -> do
        mapM_ (`hSetEncoding` utf8) [out, err]
        childScreens <- newChan
        childLog <- newIORef []
        let logged x = do
                whenVerbose $ putStrLn $ "%LOG: " ++ x
                atomicModifyIORef' childLog $ \xs -> (x:xs, ())
        let screens acc = ifM (hIsEOF out) (writeChan childScreens Nothing) $ do
                -- logging written without a newline can run into a tagged line
                (other, tagged) <- breakOn "\0" <$> hGetLine out
                unless (null other) $ logged other
                case stripPrefix screenLine tagged of
                    Just line -> screens $ line : acc
                    Nothing
                        | tagged == screenEnd -> do
                            let screen = filter (/= "") $ reverse acc
                            whenVerbose $ putStr $ unlines $ map ("%PRINT: "++) screen
                            writeChan childScreens $ Just screen
                            screens []
                        | otherwise -> screens acc
        withAsync (screens []) $ \_ -> withAsync (mapM_ logged . lines =<< hGetContents err) $ \_ -> do
            res <- act Child{..}
            hClose inp
            void $ awaiting "ghcid to stop" $ waitForProcess childProcess
            pure res

-- | Run ghcid on a sandbox, given a way to require its next screen contain some text.
withGhcid :: Sandbox -> [String] -> (([String] -> IO ()) -> IO a) -> IO a
withGhcid sb args script = withChild sb (["--quiet","--no-title","--no-status"] ++ args) $ \Child{..} -> do
    let require want = do
            screen <- awaiting ("a screen containing " ++ show want) $ readChan childScreens
            logs <- reverse <$> readIORef childLog
            let context = "\nghcid logged:\n" ++ unlines logs
            case screen of
                Nothing -> assertFailure $ "ghcid stopped before showing " ++ show want ++ context
                Just got -> assertApproxInfix want got context
            -- Ensure the next write gets a distinguishable modification time. Sleeping
            -- for exactly the measured resolution can still leave both writes in the
            -- same timestamp bucket when the sleep straddles its boundary imprecisely.
            resolution <- getModTimeResolution
            sleep $ max 0.05 $ resolution * 2
    script require


-- | Since different versions of GHCi give different messages, we only try to find what
--   we require anywhere in the obtained messages, ignoring weird characters.
assertApproxInfix :: [String] -> [String] -> String -> IO ()
assertApproxInfix want got context = do
    -- Spacing and quotes tend to be different on different GHCi versions
    let simple = lower . filter (\x -> isLetter x || isDigit x || x == ':') . unescape
        got2 = simple $ unwords got
    all ((`isInfixOf` got2) . simple) want @?
        "Expected " ++ show want ++ ", got " ++ show got ++ context

-- | Replace a file's contents in one step, as a careful editor saves.
write :: Sandbox -> FilePath -> String -> IO ()
write sb file x = do
    whenVerbose $ print ("writeFile",file,x)
    let dest = path sb file
        new = stagingDir sb </> "new"
    createDirectoryIfMissing True $ takeDirectory dest
    writeFile new x
    whenM (doesFileExist dest) $ copyPermissions dest new
    renameFile new dest

append :: Sandbox -> FilePath -> String -> IO ()
append sb file x = do
    whenVerbose $ print ("appendFile",file,x)
    write sb file . (++ x) =<< readFile' (path sb file)

rename :: Sandbox -> FilePath -> FilePath -> IO ()
rename sb from to = do
    whenVerbose $ print ("renameFile",from,to)
    renameFile (path sb from) (path sb to)



---------------------------------------------------------------------
-- ACTUAL TEST SUITE

basicTest :: TestTree
basicTest = disable19650 $ testCase "Ghcid basic" $ freshDir $ \sb -> do
    write sb "Main.hs" "main = print 1"
    withGhcid sb ["-cghci -fwarn-unused-binds Main.hs"] $ \require -> do
        require [allGoodMessage]
        write sb "Main.hs" "x"
        require ["Main.hs:1:1"," Parse error:"]

        -- Github issue 275
        write sb "Main.hs" "{-# LINE 42 \"foo.bar\" #-}\nx"
        require ["foo.bar:42:1", "Parse error:"]

        write sb "Util.hs" "module Util where"
        write sb "Main.hs" "import Util\nmain = print 1"
        require [allGoodMessage]
        write sb "Util.hs" "module Util where\nx"
        require ["Util.hs:2:1","Parse error:"]
        write sb "Util.hs" "module Util() where\nx = 1"
        require ["Util.hs:2:1","Warning:","Defined but not used: `x'"]

        -- check recursive modules work
        write sb "Util.hs" "module Util where\nimport Main"
        require ["cycle","Main.hs","Util.hs"]
        write sb "Util.hs" "module Util where"
        require [allGoodMessage]

        ghcVer <- readVersion <$> systemOutput_ "ghc --numeric-version"

        -- check renaming files works
        when (ghcVer < makeVersion [8]) $ do
            -- note that due to GHC bug #9648 and #11596 this doesn't work with newer GHC
            -- see https://ghc.haskell.org/trac/ghc/ticket/11596
            rename sb "Util.hs" "Util2.hs"
            require ["Main.hs:1:8","Could not find module `Util'"]
            rename sb "Util2.hs" "Util.hs"
            require [allGoodMessage]

        -- after this point GHC bugs mean nothing really works too much


cdTest :: TestTree
cdTest = disable19650 $ testCase "Cd basic" $ freshDir $ \sb -> do
    write sb "foo/Main.hs" "main = print 1"
    write sb "foo/Util.hs" "import Bob"
    write sb "foo/.ghci" ":load Main"
    ignore $ callProcess "chmod" ["go-w", path sb "foo", path sb "foo/.ghci"]
    ghcVer <- readVersion <$> systemOutput_ "ghc --numeric-version"
    -- GHC 8.0 and lower don't emit the LoadConfig messages
    withGhcid sb ("-ccd foo && ghci" : ["--restart=foo/.ghci" | ghcVer < makeVersion [8,2]]) $ \require -> do
        require [allGoodMessage]
        write sb "foo/Main.hs" "x"
        require ["Main.hs:1:1"," Parse error:"]
        write sb "foo/.ghci" ":load Util"
        require ["Util.hs:1:","`Bob'"]


dotGhciTest :: TestTree
dotGhciTest = testCase "Ghcid .ghci" $ copyDir "test/foo" $ \sb -> do
    write sb "test.txt" ""
    ignore $ callProcess "chmod" ["go-w", path sb ".ghci"]
    withGhcid sb ["--test=:test"] $ \require -> do
        let tested n = awaitFile (path sb "test.txt") ((>= n) . length)
        require [allGoodMessage]
        tested 1 >>= (@?= "X") -- the test writes out X
        append sb "Test.hs" "\n"
        require [allGoodMessage]
        tested 2 >>= (@?= "XX")
        let dotGhci warn = write sb ".ghci" $ unlines
                [":set -f" ++ warn ++ "warn-unused-imports", ":load Root Paths.hs Test", ":def test \\_ -> return \"Test.main\""]
        dotGhci ""
        require ["The import of Paths_foo is redundant"]
        write sb "Test.hs" "module Test(main) where\nmain = appendFile \"test.txt\" \"Y\""
        require ["The import of Paths_foo is redundant"]
        dotGhci "no-"
        require [allGoodMessage]
        -- neither load with a warning ran the test
        awaitFile (path sb "test.txt") ("Y" `isSuffixOf`) >>= (@?= "XXY")


loadConfigRestartFilterTest :: TestTree
loadConfigRestartFilterTest = testCase "Ignore generated cabal script setcwd.ghci" $ do
    shouldWatchLoadConfig "/Users/test/.cabal/script-builds/hash/setcwd.ghci" @?= False
    shouldWatchLoadConfig "/Users/test/project/.ghci" @?= True
    shouldWatchLoadConfig "foo/.ghci" @?= True


-- | Run in a copy of test/bar with cabal configured for the ambient package-db.
inCabalSandbox :: (Sandbox -> IO a) -> IO ()
inCabalSandbox act = copyDir "test/bar" $ \sb -> whenExecutable "cabal" $ do
    env <- getEnvironment
    let db = ["--package-db=" ++ x | x <- maybe [] splitSearchPath $ lookup "GHC_PACKAGE_PATH" env]
    (_, _, _, pid) <- createProcess $
         (proc "cabal" $ "configure":db){cwd = Just $ sandboxDir sb, env = Just $ filter ((/=) "GHC_PACKAGE_PATH" . fst) env}
    ExitSuccess <- awaiting "cabal configure" $ waitForProcess pid
    void $ act sb

cabalTest :: TestTree
cabalTest = testCase "Ghcid Cabal" $ inCabalSandbox $ \sb ->
    withGhcid sb [] $ \require -> do
        require [allGoodMessage]
        orig <- readFile' $ path sb "src/Literate.lhs"
        append sb "src/Literate.lhs" "> x"
        require ["src/Literate.lhs:5:3","Parse error:"]
        write sb "src/Literate.lhs" orig
        require [allGoodMessage]

-- | A .cabal change must restart ghcid even under a user-supplied --command.
cabalRestartTest :: TestTree
cabalRestartTest = testCase "Ghcid Cabal restart with custom command" $ inCabalSandbox $ \sb ->
    withGhcid sb ["-ccabal repl"] $ \require -> do
        require [allGoodMessage]
        append sb "bar.cabal" "\n-- touched by test\n"
        require [allGoodMessage]

-- | Zero loaded modules must wait for a change, not exit.
noModulesTest :: TestTree
noModulesTest = disable19650 $ testCase "Ghcid waits when nothing is loaded" $ freshDir $ \sb ->
    withGhcid sb ["-cghci Main.hs", "--restart=."] $ \require -> do
        require ["can't find"]
        write sb "Main.hs" "main = print 1"
        require [allGoodMessage]

missingWatchPathTest :: TestTree
missingWatchPathTest = testCase "Ghcid refuses a --restart or --reload path that does not exist" $ freshDir $ \sb -> do
    write sb "Main.hs" "main = print 1"
    forM_ ["--restart=typo.cabal", "--reload=typo.txt"] $ \flag ->
        withChild sb ["--quiet", "--no-title", "-cghci Main.hs", flag] $ \Child{..} ->
            awaiting ("ghcid to refuse " ++ flag) (waitForProcess childProcess) >>= (@?= ExitFailure 1)

#if !defined(mingw32_HOST_OS)
sigtermTest :: TestTree
sigtermTest = testCase "Ghcid SIGTERM takes a busy GHCi down with it" $ freshDir $ \sb -> do
    write sb "Main.hs" $ unlines
        ["import Control.Concurrent"
        ,"import System.Posix.Process"
        ,"main = do"
        ,"    writeFile \"ghci.pid\" . show =<< getProcessID"
        ,"    threadDelay 300000000"
        ]
    withChild sb ["--command=ghci Main.hs", "--test=main", "--no-title", "--no-status"] $ \Child{..} -> do
        let pollUntil what cond = awaiting what $ whileM $ do
                b <- cond
                unless b $ sleep 0.1
                pure $ not b
        let ghciPid :: IO (Maybe ProcessID)
            ghciPid = do
                b <- doesFileExist $ path sb "ghci.pid"
                if not b then pure Nothing else
                    fmap fromInteger . readMaybe <$> readFile' (path sb "ghci.pid")
        pollUntil "GHCi to report its pid" $ isJust <$> ghciPid
        Just pid <- ghciPid
        let ghciAlive = (signalProcess nullSignal pid >> pure True)
                `catchIOError` const (pure False)
        flip finally (ignored $ signalProcess sigKILL pid) $ do
            terminateProcess childProcess
            void $ awaiting "ghcid to exit" $ waitForProcess childProcess
            pollUntil "ghcid to take its GHCi child down" $ not <$> ghciAlive
#endif

-- | Wait for a file to satisfy a predicate.
awaitFile :: FilePath -> (String -> Bool) -> IO String
awaitFile file ok = do
    res <- timeout hangGuard $ flip loopM () $ \() -> do
        src <- ifM (doesFileExist file) (readFile' file) (pure "")
        if ok src then pure $ Right src else do
            sleep 0.05
            pure $ Left ()
    case res of
        Just src -> pure src
        Nothing -> do
            src <- ifM (doesFileExist file) (readFile' file) (pure "<missing>")
            assertFailure $ "Hung waiting on " ++ file ++ ", last contents:\n" ++ src

#if !defined(mingw32_HOST_OS)
missedChangeTest :: TestTree
missedChangeTest = disable19650 $ testCase "Ghcid reloads on a change its file watcher missed" $ freshDir $ \sb -> do
    write sb "Main.hs" "main = print 1"
    createDirectory $ path sb "elsewhere"
    -- a write through an unwatched hard link raises no notification where Main.hs is watched
    createLink (path sb "Main.hs") (path sb "elsewhere/Main.hs")
    withGhcid sb ["-cghci Main.hs"] $ \require -> do
        require [allGoodMessage]
        overwrite (path sb "elsewhere/Main.hs") "x"
        require ["Main.hs:1:1"," Parse error:"]
#endif

#if !defined(mingw32_HOST_OS)
-- | Change a file's contents in a single write, keeping the file (and so its other
--   links), padding with spaces to cover the old contents without truncating them.
overwrite :: FilePath -> String -> IO ()
overwrite file x = do
    old <- length <$> readFile' file
    withFile file ReadWriteMode $ \h -> hPutStr h $ x ++ replicate (old - length x) ' '
#endif

-- | A command that dies at startup must wait for a change, not exit.
brokenStartupTest :: TestTree
brokenStartupTest = testCase "Ghcid Cabal waits when the command fails at startup" $ inCabalSandbox $ \sb -> do
    orig <- readFile' $ path sb "bar.cabal"
    write sb "bar.cabal" "garbage that is not a cabal file"
    withGhcid sb [] $ \require -> do
        require ["exited unexpectedly"]
        write sb "bar.cabal" orig
        require [allGoodMessage]

brokenStartupOutputTest :: TestTree
brokenStartupOutputTest = testCase "Output files carry the command's own error when it fails at startup" $ inCabalSandbox $ \sb -> do
    write sb "bar.cabal" "garbage that is not a cabal file"
    withGhcid sb ["-oerrors.err", "-oerrors.json"] $ \_ -> do
        let cabalError = "field missing"
        void $ awaitFile (path sb "errors.err") (cabalError `isInfixOf`)
        void $ awaitFile (path sb "errors.json") (cabalError `isInfixOf`)

outputDirectoryTest :: TestTree
outputDirectoryTest = disable19650 $ testCase "Output files stay under --directory when ghcid stops" $ freshDir $ \sb -> do
    write sb "foo/Main.hs" "main = print 1"
    withGhcid sb ["-Cfoo", "-cghci Main.hs", "-oerrors.err"] $ \require ->
        require [allGoodMessage]
    stopped <- readFile' $ path sb "foo/errors.err"
    assertBool ("Expected a stopped report, got " ++ stopped) $ "Ghcid has stopped." `isInfixOf` stopped
    doesFileExist (path sb "errors.err") >>= assertBool "errors.err written outside --directory" . not

generationTest :: TestTree
generationTest = disable19650 $ testCase "Output files count load attempts across reloads and restarts" $ freshDir $ \sb -> do
    write sb "Main.hs" "main = print 1"
    write sb "restart.txt" ""
    let generation n = void $ awaitFile (path sb "errors.err") (("ghcid: generation=" ++ show n ++ " state=done ") `isPrefixOf`)
    withGhcid sb ["-cghci Main.hs", "--restart=restart.txt", "-oerrors.err"] $ \require -> do
        require [allGoodMessage]
        generation 1
        write sb "Main.hs" "main = print 2"
        require [allGoodMessage]
        generation 2
        write sb "restart.txt" "x"
        require [allGoodMessage]
        generation 3
