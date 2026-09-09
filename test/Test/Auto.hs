
-- | Test the choice of default command and restart set
module Test.Auto(autoTests) where

import Data.List.Extra

import Test.Tasty
import Test.Tasty.HUnit

import Ghcid

import Prelude


defaultOptions :: Options
defaultOptions = Options
    {command = "", arguments = [], test = [], test_message = "", run = []
    ,warnings = False, lint = Nothing, no_status = False, clear = False
    ,reverse_errors = False, no_height_limit = False, height = Nothing
    ,width = Nothing, topmost = False, no_title = False, project = ""
    ,reload = [], restart = [], directory = ".", outputfile = []
    ,ignoreLoaded = False, poll = Nothing, max_messages = Nothing
    ,color = Never, setup = [], no_cabal_repl_rtsopts = False
    ,allow_eval = False, server = False, target = []}

bareProbe :: ProjectProbe
bareProbe = ProjectProbe
    {probeCurdir = "/proj"
    ,probeStackYaml = Nothing
    ,probeCabalProject = []
    ,probeCabalFiles = []
    ,probeDotGhci = False}

auto :: Options -> ProjectProbe -> Options
auto o = autoCommand o "/tmp/ghcid/.ghci"

infix 1 `commandHas`, `commandLacks`

commandHas :: Options -> String -> Assertion
commandHas o x = x `isInfixOf` command o @?
    "Expected " ++ show x ++ " within command " ++ show (command o)

commandLacks :: Options -> String -> Assertion
commandLacks o x = not (x `isInfixOf` command o) @?
    "Expected no " ++ show x ++ " within command " ++ show (command o)

autoTests :: TestTree
autoTests = testGroup "Auto command"
    [testCase "Cabal project" $ do
        let o = auto defaultOptions bareProbe{probeCabalFiles = ["/proj/foo.cabal"]}
        o `commandHas` "cabal repl"
        o `commandHas` "-fno-code"
        restart o @?= ["/proj/foo.cabal"]
    ,testCase "Cabal project with a test" $ do
        let o = auto defaultOptions{test = ["main"]} bareProbe{probeCabalFiles = ["/proj/foo.cabal"]}
        o `commandHas` "cabal repl"
        o `commandLacks` "-fno-code"
    ,testCase "Stack project" $ do
        let o = auto defaultOptions bareProbe{probeStackYaml = Just "/proj/stack.yaml", probeCabalFiles = ["/proj/foo.cabal"]}
        o `commandHas` "stack ghci"
        restart o @?= ["/proj/stack.yaml", "/proj/foo.cabal"]
    ,testCase ".ghci project" $ do
        let o = auto defaultOptions bareProbe{probeDotGhci = True}
        o `commandHas` "ghci"
        restart o @?= ["/proj/.ghci"]
    ,testCase "Bare directory" $ do
        let o = auto defaultOptions bareProbe
        o `commandHas` "ghci"
        restart o @?= []
    ,testCase "Explicit command wins" $ do
        let o = auto defaultOptions{command = "my repl"} bareProbe{probeCabalFiles = ["/proj/foo.cabal"]}
        command o @?= "my repl"
    ,testCase "Explicit command still restarts on project files" $ do
        let probe = bareProbe
                {probeStackYaml = Just "/proj/stack.yaml"
                ,probeCabalProject = ["/proj/cabal.project"]
                ,probeCabalFiles = ["/proj/pkg/foo.cabal"]}
        let o = auto defaultOptions{command = "my repl"} probe
        restart o @?= ["/proj/stack.yaml", "/proj/cabal.project", "/proj/pkg/foo.cabal"]
    ,testCase "cabal.project without top-level .cabal still picks cabal" $ do
        let o = auto defaultOptions bareProbe{probeCabalProject = ["/proj/cabal.project"], probeCabalFiles = ["/proj/pkg/foo.cabal"]}
        o `commandHas` "cabal repl"
        restart o @?= ["/proj/cabal.project", "/proj/pkg/foo.cabal"]
    ]
