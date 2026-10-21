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
import Test.Common


waitTests :: TestTree
waitTests = testGroup "Wait"
    [testCase "A replaced directory is watched again" $ withTempDir $ \tmp -> withWaiterNotify 0.5 $ \waiter -> do
        let file = tmp </> "sub" </> "a.txt"
        write file "1"
        waitNudging waiter file (stamps file) >>= (@?= Right (Reported, [(file, ())]))
        removeDirectoryRecursive $ takeDirectory file
        write file "2"
        waitNudging waiter file (stamps file) >>= (@?= Right (Reported, [(file, ())]))

    ,testCase "A file renamed over a watched one is noticed" $ withTempDir $ \tmp -> withWaiterNotify 0.5 $ \waiter -> do
        let file = tmp </> "a.txt"
        write file "1"
        let saveAtomically stamp = write (file <.> "new") "2" >> stamp >> renameFile (file <.> "new") file
        waitNudging waiter file (map saveAtomically $ stamps $ file <.> "new") >>= (@?= Right (Reported, [(file, ())]))

    ,testCase "A file is noticed as its missing directories appear" $ withTempDir $ \tmp -> withWaiterNotify 0.5 $ \waiter -> do
        let file = tmp </> "new" </> "sub" </> "a.txt"
        let nudges = [createDirectory $ tmp </> "new", createDirectory $ tmp </> "new" </> "sub", write file "1"] ++ stamps file
        waitNudging waiter file nudges >>= (@?= Right (Reported, [(file, ())]))

#if !defined(mingw32_HOST_OS)
    ,testCase "A change the watcher misses is found by the audit, and watching resumes" $ withTempDir $ \tmp -> withWaiterNotify 0.5 $ \waiter -> do
        let file = tmp </> "watched" </> "a.txt"
        let link = tmp </> "unwatched" </> "a.txt"
        write file "1"
        createDirectory $ takeDirectory link
        createLink file link
        -- changes through an unwatched hard link raise no notification for the watched name
        waitNudging waiter file (stamps link) >>= (@?= Right (Missed, [(file, ())]))
        waitNudging waiter file (stamps file) >>= (@?= Right (Reported, [(file, ())]))
#endif
    ]

write :: FilePath -> String -> IO ()
write file x = do
    createDirectoryIfMissing True $ takeDirectory file
    writeFile file x

-- | Successive modification times for a file, all too old to count as changed
--   before a wait began, so only a notification or the audit can find them.
stamps :: FilePath -> [IO ()]
stamps file = [setModificationTime file $ posixSecondsToUTCTime $ fromIntegral i | i <- [1 :: Int ..]]

-- | Wait on a file, nudging it one step at a time until the wait ends.
waitNudging :: Waiter -> FilePath -> [IO ()] -> IO (Either String (Detection, [(FilePath, ())]))
waitNudging waiter file nudges = do
    nextWait <- waitFiles waiter
    withAsync (nextWait [(file, ())]) $ \pending ->
        let go [] = wait pending
            go (nudge:rest) = do
                nudge
                maybe (go rest) pure =<< timeout 0.2 (wait pending)
        in awaiting ("a wait on " ++ file) $ go nudges
