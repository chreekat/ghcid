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
        waitNudging waiter file (stamps file) >>= (@?= Just (Right (Reported, [(file, ())])))
        removeDirectoryRecursive $ takeDirectory file
        write file "2"
        waitNudging waiter file (stamps file) >>= (@?= Just (Right (Reported, [(file, ())])))

    ,testCase "A file renamed over a watched one is noticed" $ withTempDir $ \tmp -> withWaiterNotify 0.5 $ \waiter -> do
        let file = tmp </> "a.txt"
        write file "1"
        let saveAtomically stamp = write (file <.> "new") "2" >> stamp >> renameFile (file <.> "new") file
        waitNudging waiter file (map saveAtomically $ stamps $ file <.> "new") >>= (@?= Just (Right (Reported, [(file, ())])))

    ,testCase "A file is noticed as its missing directories appear" $ withTempDir $ \tmp -> withWaiterNotify 0.5 $ \waiter -> do
        let file = tmp </> "new" </> "sub" </> "a.txt"
        let nudges = [createDirectory $ tmp </> "new", createDirectory $ tmp </> "new" </> "sub", write file "1"] ++ stamps file
        waitNudging waiter file nudges >>= (@?= Just (Right (Reported, [(file, ())])))

#if !defined(mingw32_HOST_OS)
    ,testCase "A change the watcher misses is found by the audit, and watching resumes" $ withTempDir $ \tmp -> withWaiterNotify 0.5 $ \waiter -> do
        let file = tmp </> "watched" </> "a.txt"
        let link = tmp </> "unwatched" </> "a.txt"
        write file "1"
        createDirectory $ takeDirectory link
        createLink file link
        -- changes through an unwatched hard link raise no notification for the watched name
        waitNudging waiter file (stamps link) >>= (@?= Just (Right (Missed, [(file, ())])))
        -- past the stamps the shared inode already carries
        waitNudging waiter file (drop 100 $ stamps file) >>= (@?= Just (Right (Reported, [(file, ())])))
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
