
module Test(main) where

import Data.Maybe
import Test.Tasty
import Test.Tasty.Runners (NumThreads(..))
import System.Environment
import System.IO

import qualified Ghcid
import Test.Util
import Test.Parser
import Test.Auto
import Test.API
import Test.Ghcid

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    -- With GHCID_TEST_CHILD set, act as a real ghcid process, for tests
    -- that need to signal one
    child <- lookupEnv "GHCID_TEST_CHILD"
    if isJust child then Ghcid.main else defaultMain tests

tests :: TestTree
-- Several integration tests temporarily change the process-wide current
-- directory, so they must not overlap with each other.
tests = localOption (NumThreads 1) $ testGroup "Tests"
    [utilsTests
    ,parserTests
    ,autoTests
    ,apiTests
    ,ghcidTest
    ]
