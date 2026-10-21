
module Test.Common(disable19650, hangGuard, awaiting) where

import Test.Tasty
import Test.Tasty.HUnit
import System.Info
import System.Time.Extra
import Data.Version

-- Tests which are disabled due to https://gitlab.haskell.org/ghc/ghc/-/issues/19650
-- readily resetting which packages are loaded
disable19650 :: TestTree -> TestTree
disable19650 x
    | compilerVersion >= makeVersion [9,0] && compilerVersion < makeVersion [9,2] = testGroup "Disabled" []
    | otherwise = x

-- | How long a test waits on anything before calling it hung: far beyond what any real
--   work takes even on a saturated machine, so reaching it never means slow, only stuck.
hangGuard :: Seconds
hangGuard = 300

-- | Wait for an action, failing the test if it hangs.
awaiting :: String -> IO a -> IO a
awaiting what act = maybe (assertFailure $ "Hung waiting for " ++ what) pure =<< timeout hangGuard act
