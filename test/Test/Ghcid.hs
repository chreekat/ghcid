{-# LANGUAGE CPP #-}

-- | Test behavior of the executable, polling files for changes
module Test.Ghcid(ghcidTest) where

import Control.Concurrent.Extra
import Control.Exception.Extra
import Control.Monad.Extra
import Data.Char
import Data.List.Extra
import Data.Maybe
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
ghcidTest = localOption (mkTimeout 30000000) $ testGroup "Ghcid test"
    [basicTest
    ,cdTest
    ,dotGhciTest
    ,loadConfigRestartFilterTest
    ,cabalTest
    ,cabalRestartTest
    ,noModulesTest
    ,brokenStartupTest
#if !defined(mingw32_HOST_OS)
    ,sigtermTest
#endif
    ]


freshDir :: IO a -> IO a
freshDir act = withTempDir $ \tdir -> withCurrentDirectory tdir act

copyDir :: FilePath -> IO a -> IO ()
copyDir dir act = do
    b <- doesDirectoryExist dir
    if not b then putStrLn $ "Couldn't run test because test source is missing, " ++ dir else void $
        withTempDir $ \tdir -> do
            xs <- withCurrentDirectory dir $ listFilesRecursive "."
            forM_ xs $ \x -> do
                createDirectoryIfMissing True $ takeDirectory $ tdir </> x
                copyFile (dir </> x) (tdir </> x)
            withCurrentDirectory tdir act


whenExecutable :: String -> IO a -> IO ()
whenExecutable exe act = do
    v <- findExecutable exe
    case v of
        Nothing -> putStrLn $ "Couldn't run test because " ++ exe ++ " is missing"
        Just _ -> void act


withGhcid :: [String] -> (([String] -> IO ()) -> IO a) -> IO a
withGhcid args script = do
    chan <- newChan
    verbose <- isJust <$> lookupEnv "GHCID_TEST_VERBOSE"
    let require want = do
            t <- timeout 30 $ readChan chan
            case t of
                Nothing -> fail $ "Require failed to produce results in time, expected: " ++ show want
                Just got -> assertApproxInfix want got
            -- Ensure the next write gets a distinguishable modification time. Sleeping
            -- for exactly the measured resolution can still leave both writes in the
            -- same timestamp bucket when the sleep straddles its boundary imprecisely.
            resolution <- getModTimeResolution
            sleep $ max 0.05 $ resolution * 2

    let output msg = do
            let msg2 = filter (/= "") msg
            when verbose $
                putStr $ unlines $ map ("%PRINT: "++) msg2
            writeChan chan msg2
    done <- newBarrier
    res <- bracket
        (flip forkFinally (const $ signalBarrier done ()) $
            withArgs (["--quiet","--no-title","--no-status"]++args) $
                mainWithTerminal (pure $ TermSize 100 (Just 50) WrapHard) output)
        killThread $ \_ -> script require
    waitBarrier done
    pure res


-- | Since different versions of GHCi give different messages, we only try to find what
--   we require anywhere in the obtained messages, ignoring weird characters.
assertApproxInfix :: [String] -> [String] -> IO ()
assertApproxInfix want got = do
    -- Spacing and quotes tend to be different on different GHCi versions
    let simple = lower . filter (\x -> isLetter x || isDigit x || x == ':') . unescape
        got2 = simple $ unwords got
    all ((`isInfixOf` got2) . simple) want @?
        "Expected " ++ show want ++ ", got " ++ show got

write :: FilePath -> String -> IO ()
write file x = do
    whenVerbose $ print ("writeFile",file,x)
    createDirectoryIfMissing True $ takeDirectory file
    writeFile file x

append :: FilePath -> String -> IO ()
append file x = do
    whenVerbose $ print ("appendFile",file,x)
    appendFile file x

rename :: FilePath -> FilePath -> IO ()
rename from to = do
    whenVerbose $ print ("renameFile",from,to)
    renameFile from to



---------------------------------------------------------------------
-- ACTUAL TEST SUITE

basicTest :: TestTree
basicTest = disable19650 $ testCase "Ghcid basic" $ freshDir $ do
    write "Main.hs" "main = print 1"
    withGhcid ["-cghci -fwarn-unused-binds Main.hs"] $ \require -> do
        require [allGoodMessage]
        write "Main.hs" "x"
        require ["Main.hs:1:1"," Parse error:"]

        -- Github issue 275
        write "Main.hs" "{-# LINE 42 \"foo.bar\" #-}\nx"
        require ["foo.bar:42:1", "Parse error:"]

        write "Util.hs" "module Util where"
        write "Main.hs" "import Util\nmain = print 1"
        require [allGoodMessage]
        write "Util.hs" "module Util where\nx"
        require ["Util.hs:2:1","Parse error:"]
        write "Util.hs" "module Util() where\nx = 1"
        require ["Util.hs:2:1","Warning:","Defined but not used: `x'"]

        -- check recursive modules work
        write "Util.hs" "module Util where\nimport Main"
        require ["cycle","Main.hs","Util.hs"]
        write "Util.hs" "module Util where"
        require [allGoodMessage]

        ghcVer <- readVersion <$> systemOutput_ "ghc --numeric-version"

        -- check renaming files works
        when (ghcVer < makeVersion [8]) $ do
            -- note that due to GHC bug #9648 and #11596 this doesn't work with newer GHC
            -- see https://ghc.haskell.org/trac/ghc/ticket/11596
            rename "Util.hs" "Util2.hs"
            require ["Main.hs:1:8","Could not find module `Util'"]
            rename "Util2.hs" "Util.hs"
            require [allGoodMessage]

        -- after this point GHC bugs mean nothing really works too much


cdTest :: TestTree
cdTest = disable19650 $ testCase "Cd basic" $ freshDir $ do
    write "foo/Main.hs" "main = print 1"
    write "foo/Util.hs" "import Bob"
    write "foo/.ghci" ":load Main"
    ignore $ void $ system "chmod go-w foo foo/.ghci"
    ghcVer <- readVersion <$> systemOutput_ "ghc --numeric-version"
    -- GHC 8.0 and lower don't emit the LoadConfig messages
    withGhcid ("-ccd foo && ghci" : ["--restart=foo/.ghci" | ghcVer < makeVersion [8,2]]) $ \require -> do
        require [allGoodMessage]
        write "foo/Main.hs" "x"
        require ["Main.hs:1:1"," Parse error:"]
        write "foo/.ghci" ":load Util"
        require ["Util.hs:1:","`Bob'"]


dotGhciTest :: TestTree
dotGhciTest = testCase "Ghcid .ghci" $ copyDir "test/foo" $ do
    write "test.txt" ""
    ignore $ void $ system "chmod go-w .ghci"
    withGhcid ["--test=:test"] $ \require -> do
        require [allGoodMessage]
        sleep 1 -- time to write out the test
        readFile "test.txt" >>= (@?= "X") -- the test writes out X
        append "Test.hs" "\n"
        require [allGoodMessage]
        sleep 1 -- time to write out the test
        readFile "test.txt" >>= (@?= "XX")
        print =<< readFile ".ghci"
        write ".ghci" ":set -fwarn-unused-imports\n:load Root Paths.hs Test"
        require ["The import of Paths_foo is redundant"]
        sleep 1 -- time to write out the test
        readFile "test.txt" >>= (@?= "XX") -- but shouldn't run on warning


loadConfigRestartFilterTest :: TestTree
loadConfigRestartFilterTest = testCase "Ignore generated cabal script setcwd.ghci" $ do
    shouldWatchLoadConfig "/Users/test/.cabal/script-builds/hash/setcwd.ghci" @?= False
    shouldWatchLoadConfig "/Users/test/project/.ghci" @?= True
    shouldWatchLoadConfig "foo/.ghci" @?= True


-- | Run in a copy of test/bar with cabal configured for the ambient package-db.
inCabalSandbox :: IO a -> IO ()
inCabalSandbox act = copyDir "test/bar" $ whenExecutable "cabal" $ do
    env <- getEnvironment
    let db = ["--package-db=" ++ x | x <- maybe [] splitSearchPath $ lookup "GHC_PACKAGE_PATH" env]
    (_, _, _, pid) <- createProcess $
         (proc "cabal" $ "configure":db){env = Just $ filter ((/=) "GHC_PACKAGE_PATH" . fst) env}
    ExitSuccess <- waitForProcess pid
    void act

cabalTest :: TestTree
cabalTest = testCase "Ghcid Cabal" $ inCabalSandbox $
    withGhcid [] $ \require -> do
        require [allGoodMessage]
        orig <- readFile' "src/Literate.lhs"
        append "src/Literate.lhs" "> x"
        require ["src/Literate.lhs:5:3","Parse error:"]
        write "src/Literate.lhs" orig
        require [allGoodMessage]

-- | A .cabal change must restart ghcid even under a user-supplied --command.
cabalRestartTest :: TestTree
cabalRestartTest = testCase "Ghcid Cabal restart with custom command" $ inCabalSandbox $
    withGhcid ["-ccabal repl"] $ \require -> do
        require [allGoodMessage]
        append "bar.cabal" "\n-- touched by test\n"
        require [allGoodMessage]

-- | Zero loaded modules must wait for a change, not exit.
noModulesTest :: TestTree
noModulesTest = disable19650 $ testCase "Ghcid waits when nothing is loaded" $ freshDir $
    withGhcid ["-cghci Main.hs", "--restart=Main.hs"] $ \require -> do
        require ["can't find"]
        write "Main.hs" "main = print 1"
        require [allGoodMessage]

#if !defined(mingw32_HOST_OS)
sigtermTest :: TestTree
sigtermTest = testCase "Ghcid SIGTERM takes a busy GHCi down with it" $ freshDir $ do
    write "Main.hs" $ unlines
        ["import Control.Concurrent"
        ,"import System.Posix.Process"
        ,"main = do"
        ,"    writeFile \"ghci.pid\" . show =<< getProcessID"
        ,"    threadDelay 300000000"
        ]
    exe <- getExecutablePath
    env <- getEnvironment
    withFile "ghcid.out" WriteMode $ \h -> do
        (_, _, _, ghcid) <- createProcess
            (proc exe ["--command=ghci Main.hs", "--test=main", "--no-title", "--no-status"])
                {env = Just $ ("GHCID_TEST_CHILD","1") : env
                ,std_out = UseHandle h, std_err = UseHandle h}
        let pollUntil secs msg cond = do
                res <- timeout secs $ whileM $ do
                    b <- cond
                    unless b $ sleep 0.1
                    pure $ not b
                when (isNothing res) $ assertFailure msg
        let ghciPid :: IO (Maybe ProcessID)
            ghciPid = do
                b <- doesFileExist "ghci.pid"
                if not b then pure Nothing else
                    fmap fromInteger . readMaybe <$> readFile' "ghci.pid"
        flip finally (ignored $ do
            pid <- getPid ghcid
            whenJust pid $ signalProcess sigKILL
            void $ waitForProcess ghcid) $ do
            pollUntil 20 "GHCi never reported its pid" $ isJust <$> ghciPid
            Just pid <- ghciPid
            let ghciAlive = (signalProcess nullSignal pid >> pure True)
                    `catchIOError` const (pure False)
            flip finally (ignored $ signalProcess sigKILL pid) $ do
                terminateProcess ghcid
                void $ waitForProcess ghcid
                pollUntil 10 "ghcid leaked its GHCi child" $ not <$> ghciAlive
#endif

-- | A command that dies at startup must wait for a change, not exit.
brokenStartupTest :: TestTree
brokenStartupTest = testCase "Ghcid Cabal waits when the command fails at startup" $ inCabalSandbox $ do
    orig <- readFile' "bar.cabal"
    write "bar.cabal" "garbage that is not a cabal file"
    withGhcid [] $ \require -> do
        require ["exited unexpectedly"]
        write "bar.cabal" orig
        require [allGoodMessage]
