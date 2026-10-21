{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- | Use 'withWaiterPoll' or 'withWaiterNotify' to create a 'Waiter' object,
--   then access it (single-threaded) by using 'waitFiles'.
module Wait(Waiter, Detection(..), withWaiterPoll, withWaiterNotify, waitFiles) where

import Control.Concurrent.Extra
import Control.Concurrent.Async
import qualified Data.Map as Map
import Control.Monad.Extra
import Data.List.Extra
import Data.Maybe
import System.FilePath
import Control.Exception.Extra
import System.Directory.Extra
import Data.Time.Clock
import Data.String
import System.Time.Extra
import System.FSNotify
import Language.Haskell.Ghcid.Util
#if !defined(mingw32_HOST_OS)
import System.Posix.Files
import System.Posix.Types
#endif


data Waiter
  = WaiterPoll Seconds
  | WaiterNotify Notifier

-- | File-system notifications, audited for changes they fail to report.
data Notifier = Notifier
    {audit :: IO () -- ^ Blocks until the files are due to be checked even without a notification
    ,kick :: MVar () -- ^ Filled by an event on any watch
    ,watcher :: Var Watcher
    }

-- | A watch manager and the directories it watches.
data Watcher = Watcher
    {manager :: WatchManager
    ,watches :: Map.Map FilePath Watch -- ^ Keyed by canonical path
    }

-- | A watch on a directory.
data Watch = Watch
    {watchedId :: Maybe DirId -- ^ The directory that was at that path just before the watch was placed
    ,stopWatch :: StopListening
    }

-- | What makes a directory the same directory, even when another takes over its path.
#if defined(mingw32_HOST_OS)
type DirId = ()
#else
type DirId = (DeviceID, FileID)
#endif

-- | The identity of the directory at a path, if there is one.
dirId :: FilePath -> IO (Maybe DirId)
#if defined(mingw32_HOST_OS)
dirId dir = ifM (doesDirectoryExist dir) (pure $ Just ()) (pure Nothing)
#else
dirId dir = handle (\(_ :: IOError) -> pure Nothing) $ do
    st <- getFileStatus dir
    pure $ if isDirectory st then Just (deviceID st, fileID st) else Nothing
#endif

-- | How a change came to light.
data Detection
    = Reported -- ^ By a notification or a poll, or already there when the wait began
    | Missed -- ^ Only by the audit: no notification ever reported it
    deriving (Eq, Show)

withWaiterPoll :: Seconds -> (Waiter -> IO a) -> IO a
withWaiterPoll x f = f $ WaiterPoll x

-- | A waiter driven by file-system notifications, which also checks the files
--   whenever the given action returns before one arrives.
withWaiterNotify :: IO () -> (Waiter -> IO a) -> IO a
withWaiterNotify audit f = do
    kick <- newEmptyMVar
    bracket (newVar . (`Watcher` Map.empty) =<< startManager) (stopManager . manager <=< readVar) $ \watcher ->
        f $ WaiterNotify Notifier{..}

-- `listContentsInside test dir` will list files and directories inside `dir`,
-- recursing into those subdirectories which pass `test`.
-- Note that `dir` and files it directly contains are always listed, regardless of `test`.
-- Subdirectories will have a trailing path separator, and are only listed if we recurse into them.
listContentsInside :: (FilePath -> IO Bool) -> FilePath -> IO [FilePath]
listContentsInside test dir = do
    (dirs,files) <- partitionM doesDirectoryExist =<< listContents dir
    recurse <- filterM test dirs
    rest <- concatMapM (listContentsInside test) recurse
    pure $ addTrailingPathSeparator dir : files ++ rest

-- | Given the pattern:
--
-- > wait <- waitFiles waiter
-- > ...
-- > wait ["File1.hs","File2.hs"]
--
--   This continues as soon as either @File1.hs@ or @File2.hs@ changes,
--   starting from when 'waitFiles' was initially called.
--
--   returns a message about why you are continuing (usually a file name).
waitFiles :: forall a.  Ord a => Waiter -> IO ([(FilePath, a)] -> IO (Either String (Detection, [(FilePath, a)])))
waitFiles waiter = do
    base <- getCurrentTime
    pure $ \files -> handle onError (go base files)
 where
    onError :: IOError -> IO (Either String (Detection, [(FilePath, a)]))
    onError e = sleep 1.0 >> pure (Left (show e))

    go :: UTCTime -> [(FilePath, a)] -> IO (Either String (Detection, [(FilePath, a)]))
    go base files = do
        logDebug $ "WAITING: " ++ unwords (map fst files)
        -- As listContentsInside returns directories, we are waiting on them explicitly and so
        -- will pick up new files, as creating a new file changes the containing directory's modtime.
        files <- concatForM files $ \(file, a) ->
            ifM (doesDirectoryExist file) (fmap (,a) <$> listContentsInside (pure . not . isPrefixOf "." . takeFileName) file) (pure [(file, a)])
        case waiter of
            WaiterPoll t -> pure ()
            WaiterNotify n -> do
                syncWatches n $ map fst files
                void $ tryTakeMVar $ kick n
        new <- mapM (getModTime . fst) files
        case [x | (x,Just t) <- zip files new, t > base] of
            [] -> Right <$> recheck files new
            xs -> pure (Right (Reported, xs))

    recheck :: [(FilePath, a)] -> [Maybe UTCTime] -> IO (Detection, [(String, a)])
    recheck files old = do
            sleep 0.1
            -- how to account for any change found on this wake
            detect <- case waiter of
                WaiterPoll t -> do
                    sleep $ max 0 $ t - 0.1 -- subtract the initial 0.1 sleep from above
                    pure $ pure Reported
                WaiterNotify n@Notifier{..} -> do
                    -- read, not taken, so the kick outlives the race whichever side wins
                    woke <- race (readMVar kick) audit
                    logDebug $ "WAITING: " ++ either (const "Notify signaled") (const "Auditing") woke
                    -- Any event may have reshaped the directories, e.g. created one on the way to a missing file
                    syncWatches n $ map fst files
                    since <- tryTakeMVar kick
                    pure $ if isJust since then pure Reported else confirmMissed n
            new <- mapM (getModTime . fst) files
            case [x | (x,t1,t2) <- zip3 files old new, t1 /= t2] of
                [] -> recheck files new
                xs -> do
                    let disappeared = [x | (x, Just _, Nothing) <- zip3 files old new]
                    unless (null disappeared) $ do
                        -- if someone is deleting a needed file, give them some space to put the file back
                        -- typically caused by VIM
                        -- but try not to
                        logDebug $ "WAITING: Waiting max of 1s due to file removal, " ++ unwords (nubOrd (map fst disappeared))
                        -- at most 20 iterations, but stop as soon as the file returns
                        void $ flip firstJustM (replicate 20 ()) $ \_ -> do
                            sleep 0.05
                            new <- mapM (getModTime . fst) files
                            pure $ if null [x | (x, Just _, Nothing) <- zip3 files old new] then Just () else Nothing
                    (,xs) <$> detect


-- | Settle whether the audit found a change that no notification reported, allowing a
--   second for one still in flight. Watches that missed a change cannot be trusted, so
--   a confirmed miss replaces them all.
confirmMissed :: Notifier -> IO Detection
confirmMissed Notifier{..} = do
    late <- timeout 1 $ readMVar kick
    if isJust late then pure Reported else do
        logDebug "WAITING: Notifications missed a change, rebuilding the watcher"
        modifyVar_ watcher $ \w -> do
            handle (\(_ :: IOError) -> pure ()) $ stopManager $ manager w
            (`Watcher` Map.empty) <$> startManager
        pure Missed

-- | Watch exactly the directories holding the given paths, kicking on any event.
--   A path whose directory is missing is covered by its nearest existing ancestor.
syncWatches :: Notifier -> [FilePath] -> IO ()
syncWatches Notifier{..} files = do
    dirs <- fmap Map.fromList $ forM (nubOrd $ map takeDirectory files) $ \dir -> do
        dir <- nearestDir dir
        (dir,) <$> dirId dir
    modifyVar_ watcher $ \Watcher{..} -> do
        -- A watch follows its directory, not the path, so rewatch a path whose directory was replaced
        let (keep,del) = Map.partitionWithKey (\k v -> Map.lookup k dirs == Just (watchedId v)) watches
        mapM_ (handle (\(_ :: IOError) -> pure ()) . stopWatch) $ Map.elems del
        new <- forM (Map.toList $ dirs `Map.difference` keep) $ \(dir, i) -> do
            res <- try $ watchDir manager (fromString dir) (const True) $ \event -> do
                logDebug $ "NOTIFY: " ++ show event
                void $ tryPutMVar kick ()
            case res of
                Right stop -> pure (dir, Watch i stop)
                Left (e :: IOError) -> do
                    -- recorded anyway, so the failure is retried only once the directory changes
                    logErr $ "Cannot watch " ++ dir ++ " for changes: " ++ show e
                    pure (dir, Watch i $ pure ())
        let watches = keep `Map.union` Map.fromList new
        logDebug $ "WAITING: " ++ unwords (Map.keys watches)
        pure Watcher{..}

-- | The canonical form of the closest existing directory at or above the given one.
nearestDir :: FilePath -> IO FilePath
nearestDir dir = do
    dir <- canonicalizePathSafe dir
    let up = takeDirectory dir
    ifM (doesDirectoryExist dir ||^ pure (up == dir)) (pure dir) (nearestDir up)

canonicalizePathSafe :: FilePath -> IO FilePath
canonicalizePathSafe x = canonicalizePath x `catch` \(_ :: IOError) -> pure x
