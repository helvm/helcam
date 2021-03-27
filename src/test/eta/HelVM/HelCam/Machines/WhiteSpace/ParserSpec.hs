module HelVM.HelCam.Machines.WhiteSpace.ParserSpec (spec) where

import HelVM.HelCam.Machines.WhiteSpace.EvaluatorSpecData

import HelVM.HelCam.Machines.WhiteSpace.Parser

import Test.Hspec
import Test.Hspec.Contrib.HUnit (fromHUnitTest)
import Test.HUnit

spec :: Spec
spec = fromHUnitTest testsOfWSParser

testsOfWSParser :: Test
testsOfWSParser = TestList
  [ "testOfParseCat"          ~: "cat"                     ~: catIL          ~=? parseTL catTL          False
  , "testOfParseHelloWorld"   ~: "helloWorld"              ~: helloWorldIL   ~=? parseTL helloWorldTL   False
  , "testOfParseTruthMachine" ~: "testOfParseTruthMachine" ~: truthMachineIL ~=? parseTL truthMachineTL False
  ]
