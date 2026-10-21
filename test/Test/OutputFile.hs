-- | Test the contents of the output files
module Test.OutputFile(outputFileTests) where

import Data.Char
import Data.Time
import System.FilePath
import System.IO.Extra
import Test.Tasty
import Test.Tasty.HUnit

import Language.Haskell.Ghcid.Parser
import OutputFile


outputFileTests :: TestTree
outputFileTests = testGroup "Output file"
    [testCase "The text form puts a generation header above the unchanged body" $
        renderReport TextFormat (Report 7 started $ Finished allGood) @?= unlines
            ["ghcid: generation=7 state=done started=20261021T094312.345000000Z"
            ,"All good (1 module, at 09:43:12)"
            ]
    ,testCase "The JSON form carries generation, state and start time" $
        renderReport JsonFormat (Report 7 started $ Finished allGood) @?= unlines
            ["{\"generation\":7"
            ,",\"state\":\"done\""
            ,",\"started\":\"2026-10-21T09:43:12.345000000Z\""
            ,",\"loaded\":"
            ,"  [\"/p/Main.hs\""
            ,"  ]"
            ,",\"messages\":"
            ,"  []"
            ,"}"
            ]
    ,testCase "A command that exited is reported as state failed" $
        take 1 (lines $ renderReport TextFormat $ Report 2 started $ Finished $ CommandFailed "cabal repl" ["oops"])
            @?= ["ghcid: generation=2 state=failed started=20261021T094312.345000000Z"]
    ,testCase "The text header is no diagnostic to GHC-style parsers" $ do
        let header = takeWhile (/= '\n') $ renderReport TextFormat $ Report 12 started $ Running Nothing
        parseLoad [header] @?= []
        -- quickfix's %f:%l:%m would read any colon followed by a digit as a location
        assertBool header $ not $ any (\(a,b) -> a == ':' && isDigit b) $ zip header (drop 1 header)
    ,testCase "Each load attempt gets the next generation and shows the previous outcome while it runs" $
        withTempDir $ \dir -> do
            let file = dir </> "errors.err"
                -- the generation and state, then the body
                shown = do
                    src <- readFile' file
                    pure (take 2 $ drop 1 $ words src, drop 1 $ lines src)
            outputs <- newOutputFiles [file]
            outputLoading outputs
            shown >>= (@?= (["generation=1", "state=loading"], []))
            outputFinished outputs allGood
            shown >>= (@?= (["generation=1", "state=done"], outcomeText allGood))
            outputLoading outputs
            shown >>= (@?= (["generation=2", "state=loading"], outcomeText allGood))
            outputStopped outputs
            shown >>= (@?= (["generation=2", "state=stopped"], ["Ghcid has stopped."]))
    ]
    where
        started = UTCTime (fromGregorian 2026 10 21) (picosecondsToDiffTime 34992345000000000)
        allGood = Loaded ["/p/Main.hs"] [] ["All good (1 module, at 09:43:12)"]
