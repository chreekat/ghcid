{-# LANGUAGE RecordWildCards #-}

-- | The files written by @--outputfile@.
module OutputFile(
    Outcome(..), Format(..), formatOf, renderOutcome, writeOutputFiles
    ) where

import Data.List.Extra
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

renderOutcome :: Format -> Outcome -> String
renderOutcome TextFormat Loaded{..} = unlines outcomeText
renderOutcome TextFormat CommandFailed{..} = unlines $
    ["Command \"" ++ outcomeCommand ++ "\" exited unexpectedly"
    ,"Waiting for changes that will restart ghcid."
    ] ++ map unescape outcomeOutput
renderOutcome JsonFormat Loaded{..} = showJSON
    [("loaded", JArray $ map jString outcomeLoaded)
    ,("messages", JArray $ jMessages outcomeMessages)
    ]
renderOutcome JsonFormat CommandFailed{..} = showJSON
    [("command", JScalar $ jString outcomeCommand)
    ,("output", JArray $ map (jString . unescape) outcomeOutput)
    ,("loaded", JArray [])
    -- the command may have printed GHC diagnostics before exiting
    ,("messages", JArray $ jMessages $ parseLoad outcomeOutput)
    ]

writeOutputFiles :: [FilePath] -> Outcome -> IO ()
writeOutputFiles files outcome =
    mapM_ (\file -> writeFile file $ renderOutcome (formatOf file) outcome) files


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
