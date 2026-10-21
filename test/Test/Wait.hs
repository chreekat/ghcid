{-# LANGUAGE CPP #-}

-- | Test the file waiter against a real file system
module Test.Wait(waitTests) where

import Control.Concurrent.Async
import Data.Time.Clock.POSIX
import System.Directory
import System.FilePath
import System.IO.Extra
import System.Time.Extra
#if !defined(mingw32_HOST_OS)
import System.Posix.Files
#endif

import Test.Tasty
import Test.Tasty.HUnit

import Wait


waitTests :: TestTree
waitTests = localOption (mkTimeout 30000000) $ testGroup "Wait"
    [testCase "A replaced directory is watched again" $ withTempDir $ \tmp -> withWaiterNotify 0.5 $ \waiter -> do
        let file = tmp </> "sub" </> "a.txt"
        write file "1"
        waitNudging waiter file (repeat $ appendFile file "x") >>= (@?= Just (Right (Reported, [(file, ())])))
        removeDirectoryRecursive $ takeDirectory file
        write file "2"
        waitNudging waiter file (repeat $ appendFile file "x") >>= (@?= Just (Right (Reported, [(file, ())])))

    ,testCase "A file is noticed as its missing directories appear" $ withTempDir $ \tmp -> withWaiterNotify 0.5 $ \waiter -> do
        let file = tmp </> "new" </> "sub" </> "a.txt"
        let nudges = [createDirectory $ tmp </> "new", createDirectory $ tmp </> "new" </> "sub"] ++ repeat (appendFile file "x")
        waitNudging waiter file nudges >>= (@?= Just (Right (Reported, [(file, ())])))

#if !defined(mingw32_HOST_OS)
    ,testCase "A change the watcher misses is found by the audit, and watching resumes" $ withTempDir $ \tmp -> withWaiterNotify 0.5 $ \waiter -> do
        let file = tmp </> "watched" </> "a.txt"
        let link = tmp </> "unwatched" </> "a.txt"
        write file "1"
        createDirectory $ takeDirectory link
        createLink file link
        -- through an unwatched hard link, with mtimes too old to count as changed before the wait began
        let viaLink i = setModificationTime link $ posixSecondsToUTCTime $ fromIntegral (i :: Int)
        waitNudging waiter file (map viaLink [1..]) >>= (@?= Just (Right (Missed, [(file, ())])))
        waitNudging waiter file (repeat $ appendFile file "x") >>= (@?= Just (Right (Reported, [(file, ())])))
#endif
    ]

write :: FilePath -> String -> IO ()
write file x = do
    createDirectoryIfMissing True $ takeDirectory file
    writeFile file x

-- | Wait on a file, nudging it one step at a time until the wait ends or a 10s deadline passes.
waitNudging :: Waiter -> FilePath -> [IO ()] -> IO (Maybe (Either String (Detection, [(FilePath, ())])))
waitNudging waiter file nudges = do
    nextWait <- waitFiles waiter
    withAsync (nextWait [(file, ())]) $ \pending ->
        let go [] = pure Nothing
            go (nudge:rest) = do
                nudge
                maybe (go rest) (pure . Just) =<< timeout 0.2 (wait pending)
        in go $ take 50 nudges
