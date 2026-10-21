{-# LANGUAGE RecordWildCards #-}

-- | The files written by @--outputfile@.
module OutputFile(
    Outcome(..), Status(..), Report(..), Format(..), formatOf, renderReport,
    OutputFiles, newOutputFiles, outputLoading, outputFinished, outputStopped
    ) where

import Data.IORef
import Data.List.Extra
import Data.Time
import System.FilePath

import Language.Haskell.Ghcid.Escape
import Language.Haskell.Ghcid.Parser
import Language.Haskell.Ghcid.Types


-- | What one attempt to load the project produced.
data Outcome
    = -- | The command gave a session.
      Loaded
        {outcomeLoaded :: [FilePath] -- ^ The modules in the session
        ,outcomeMessages :: [Load] -- ^ The diagnostics, for the JSON form
        ,outcomeText :: [String] -- ^ The body of the text form
        }
    | -- | The command exited, leaving no session.
      CommandFailed
        {outcomeCommand :: String
        ,outcomeOutput :: [String] -- ^ What the command printed, oldest first, possibly escaped
        }

-- | How an output file is written.
data Format = TextFormat | JsonFormat
    deriving (Eq, Show)

-- | The format is chosen by the extension.
formatOf :: FilePath -> Format
formatOf file
    | takeExtension file == ".json" = JsonFormat
    | otherwise = TextFormat

-- Note [Output file header]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~
-- Every write leads with a header whose meaning for pollers is documented in
-- README.md, under "How do I read the output file from a script?". Its start
-- time must be taken before GHCi reads the sources, or an edit made in between
-- would look included when it is not; taking it just before keeps pollers
-- from waiting on edits that were in fact included.
--
-- While an attempt runs, the files keep the previous outcome's body under a
-- loading header, so editors keep their diagnostics until new ones arrive.
-- The text header is one line that no GHC-style parser takes for a
-- diagnostic, so readers of the body can ignore it.

-- | Where the latest load attempt stands.
data Status
    = Running (Maybe Outcome) -- ^ Underway, holding the previous outcome, if any
    | Finished Outcome
    | Stopped [String] -- ^ ghcid has exited, for the reason given in these lines, if known

-- | Everything one rewrite of the output files shows.
data Report = Report
    {reportGeneration :: Int -- ^ Counts load attempts, from 1
    ,reportStarted :: UTCTime -- ^ When the attempt began
    ,reportStatus :: Status
    }

stateName :: Status -> String
stateName Running{} = "loading"
stateName (Finished Loaded{}) = "done"
stateName (Finished CommandFailed{}) = "failed"
stateName Stopped{} = "stopped"

-- | The contents of an output file. See Note [Output file header]
renderReport :: Format -> Report -> String
renderReport TextFormat Report{..} = unlines $
    unwords
        ["ghcid:"
        ,"generation=" ++ show reportGeneration
        ,"state=" ++ stateName reportStatus
        -- ISO 8601 basic format, since quickfix reads a colon and digits as a location
        ,"started=" ++ formatTime defaultTimeLocale "%Y%m%dT%H%M%S%9QZ" reportStarted
        ] :
    case reportStatus of
        Running outcome -> maybe [] textBody outcome
        Finished outcome -> textBody outcome
        Stopped why -> "Ghcid has stopped." : why
renderReport JsonFormat Report{..} = showJSON $
    [("generation", JScalar $ show reportGeneration)
    ,("state", JScalar $ jString $ stateName reportStatus)
    ,("started", JScalar $ jString $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%9QZ" reportStarted)
    ] ++
    case reportStatus of
        Running outcome -> maybe nothingLoaded jsonFields outcome
        Finished outcome -> jsonFields outcome
        Stopped why -> [("reason", JArray $ map jString why) | not $ null why] ++ nothingLoaded
    where nothingLoaded = [("loaded", JArray []), ("messages", JArray [])]

textBody :: Outcome -> [String]
textBody Loaded{..} = outcomeText
textBody CommandFailed{..} =
    ["Command \"" ++ outcomeCommand ++ "\" exited unexpectedly"
    ,"Waiting for changes that will restart ghcid."
    ] ++ map unescape outcomeOutput

jsonFields :: Outcome -> [(String, JValue)]
jsonFields Loaded{..} =
    [("loaded", JArray $ map jString outcomeLoaded)
    ,("messages", JArray $ jMessages outcomeMessages)
    ]
jsonFields CommandFailed{..} =
    [("command", JScalar $ jString outcomeCommand)
    ,("output", JArray $ map (jString . unescape) outcomeOutput)
    ,("loaded", JArray [])
    -- the command may have printed GHC diagnostics before exiting
    ,("messages", JArray $ jMessages $ parseLoad outcomeOutput)
    ]


-- | The output files, and the report they show.
data OutputFiles = OutputFiles [FilePath] (IORef Report)

-- | Before the first load attempt; nothing is written yet.
newOutputFiles :: [FilePath] -> IO OutputFiles
newOutputFiles files = do
    now <- getCurrentTime
    OutputFiles files <$> newIORef (Report 0 now $ Stopped [])

-- | Start the next load attempt. See Note [Output file header]
outputLoading :: OutputFiles -> IO ()
outputLoading outputs = do
    now <- getCurrentTime
    update outputs $ \Report{..} -> Report (reportGeneration + 1) now $ Running $ case reportStatus of
        Running outcome -> outcome
        Finished outcome -> Just outcome
        Stopped _ -> Nothing

outputFinished :: OutputFiles -> Outcome -> IO ()
outputFinished outputs outcome = update outputs $ \r -> r{reportStatus = Finished outcome}

-- | ghcid is exiting; stopping again keeps the reason already given.
outputStopped :: OutputFiles -> [String] -> IO ()
outputStopped outputs why = update outputs $ \r -> r{reportStatus = Stopped $ case reportStatus r of
    Stopped earlier -> earlier ++ why
    _ -> why}

update :: OutputFiles -> (Report -> Report) -> IO ()
update (OutputFiles files ref) f = do
    report <- atomicModifyIORef' ref $ \r -> let r' = f r in (r', r')
    mapM_ (\file -> writeFile file $ renderReport (formatOf file) report) files


-- | An already rendered JSON value.
data JValue = JScalar String | JArray [String]

-- | One array element per line, so the file stays greppable.
showJSON :: [(String, JValue)] -> String
showJSON xs = unlines $ concat $
    [ field (if i == 0 then "{" else ",") a b | (i,(a,b)) <- zipFrom (0 :: Int) xs] ++
    [["}"]]
    where
        field sep a (JScalar b) = [sep ++ jString a ++ ":" ++ b]
        field sep a (JArray bs) =
            (sep ++ jString a ++ ":") :
            ["  " ++ (if j == 0 then "[" else ",") ++ b | (j,b) <- zipFrom (0 :: Int) bs] ++
            [if null bs then "  []" else "  ]"]

jString :: String -> String
jString x = "\"" ++ escapeJSON x ++ "\""

-- | The 'Message's, skipping any other 'Load'.
jMessages :: [Load] -> [String]
jMessages xs =
    [ jDict $
        [("severity",jString $ show loadSeverity)
        ,("file",jString loadFile)] ++
        [("start",pair loadFilePos) | loadFilePos /= (0,0)] ++
        [("end", pair loadFilePosEnd) | loadFilePos /= loadFilePosEnd] ++
        [("message", jString $ intercalate "\n" loadMessage)]
    | Message{..} <- xs]
    where pair (a,b) = "[" ++ show a ++ "," ++ show b ++ "]"

jDict :: [(String, String)] -> String
jDict xs = "{" ++ intercalate ", " [jString a ++ ":" ++ b | (a,b) <- xs] ++ "}"
