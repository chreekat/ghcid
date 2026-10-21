-- | Test the file waiter against a real file system
module Test.Wait(waitTests) where

import Control.Concurrent.Async
import System.Directory
import System.FilePath
import System.IO.Extra
import System.Time.Extra

import Test.Tasty
import Test.Tasty.HUnit

import Wait


waitTests :: TestTree
waitTests = localOption (mkTimeout 30000000) $ testGroup "Wait"
    [testCase "A replaced directory is watched again" $ withTempDir $ \tmp -> withWaiterNotify $ \waiter -> do
        let file = tmp </> "sub" </> "a.txt"
        write file "1"
        waitNudging waiter file (appendFile file "x") >>= (@?= Just (Right [(file, ())]))
        removeDirectoryRecursive $ takeDirectory file
        write file "2"
        waitNudging waiter file (appendFile file "x") >>= (@?= Just (Right [(file, ())]))
    ]

write :: FilePath -> String -> IO ()
write file x = do
    createDirectoryIfMissing True $ takeDirectory file
    writeFile file x

-- | Wait on a file, repeating a nudge until the wait ends or a 10s deadline passes.
waitNudging :: Waiter -> FilePath -> IO () -> IO (Maybe (Either String [(FilePath, ())]))
waitNudging waiter file nudge = do
    nextWait <- waitFiles waiter
    withAsync (nextWait [(file, ())]) $ \pending ->
        let go 0 = pure Nothing
            go n = do
                nudge
                maybe (go (n - 1 :: Int)) (pure . Just) =<< timeout 0.2 (wait pending)
        in go 50
