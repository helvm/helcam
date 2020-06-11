module HelVM.HelMA.Automata.WhiteSpace.ParserSpec (spec) where

import HelVM.HelMA.Automata.WhiteSpace.EvaluatorSpecData

import HelVM.HelMA.Automata.WhiteSpace.Parser

import Test.Hspec

spec :: Spec
spec = do
  describe "parseTL" $ do
    it "cat"          $ do parseTL catTL          False `shouldBe` catIL
    it "helloWorld"   $ do parseTL helloWorldTL   False `shouldBe` helloWorldIL
    it "truthMachine" $ do parseTL truthMachineTL False `shouldBe` truthMachineIL
