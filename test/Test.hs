
module Test(main) where

import Data.Maybe
import Test.Tasty
import Test.Tasty.Runners (NumThreads(..))
import System.Environment
import System.IO

import Test.Util
import Test.Parser
import Test.Auto
import Test.OutputFile
import Test.API
import Test.Ghcid
import Test.Wait

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    -- See Note [Ghcid under test]
    child <- lookupEnv "GHCID_TEST_CHILD"
    if isJust child then ghcidChild else defaultMain tests

tests :: TestTree
-- Several integration tests temporarily change the process-wide current
-- directory, so they must not overlap with each other.
tests = localOption (NumThreads 1) $ testGroup "Tests"
    [utilsTests
    ,parserTests
    ,autoTests
    ,outputFileTests
    ,apiTests
    ,waitTests
    ,ghcidTest
    ]
