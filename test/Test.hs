
module Test(main) where

import Data.Maybe
import Test.Tasty
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
tests = testGroup "Tests"
    [utilsTests
    ,parserTests
    ,autoTests
    ,outputFileTests
    ,apiTests
    ,waitTests
    ,ghcidTest
    ]
