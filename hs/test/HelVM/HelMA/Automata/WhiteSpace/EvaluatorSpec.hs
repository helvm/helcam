module HelVM.HelMA.Automata.WhiteSpace.EvaluatorSpec (spec) where

import HelVM.HelMA.Automata.WhiteSpace.Evaluator

import HelVM.HelMA.Automata.WhiteSpace.EvaluatorSpecData

import HelVM.HelMA.Common.IO.FreeIO
import HelVM.HelMA.Common.IO.MockIO

import Test.Hspec

spec :: Spec
spec = do
  describe "interact" $ do
    it "Test evalCountTL"        $ do batchSimpleEvalTL              countTL        `shouldBe` countO
    it "Test evalHelloWorldTL"   $ do batchSimpleEvalTL              helloWorldTL   `shouldBe` helloWorldO
    it "Test evalHWorldTL"       $ do batchSimpleEvalTL              hWorldTL       `shouldBe` hWorldO
    it "Test evalCalcTL"         $ do flipSimpleEvalTL "-1\n"        calcTL         `shouldBe` calcO
    it "Test evalFactTL"         $ do flipSimpleEvalTL "10\n"        factTL         `shouldBe` factO
    it "Test evalHanoiTL"        $ do flipSimpleEvalTL "1\n"         hanoiTL        `shouldBe` hanoiO
    it "Test evalLocTestTL"      $ do flipSimpleEvalTL "1\n2\n"      locTestTL      `shouldBe` locTestO
    it "Test evalNameTL"         $ do flipSimpleEvalTL "WriteOnly\n" nameTL         `shouldBe` nameO
    it "Test evalTruthMachineTL" $ do flipSimpleEvalTL "0\n"         truthMachineTL `shouldBe` zeroO

  describe "monadic" $ do
    it "Test evalCountTL"        $ do (batchExecMockIO              . simpleEvalTL) countTL        `shouldBe` countO
    it "Test evalHelloWorldTL"   $ do (batchExecMockIO              . simpleEvalTL) helloWorldTL   `shouldBe` helloWorldO
    it "Test evalHWorldTL"       $ do (batchExecMockIO              . simpleEvalTL) hWorldTL       `shouldBe` hWorldO
    it "Test evalCalcTL"         $ do (flipExecMockIO "-1\n"        . simpleEvalTL) calcTL         `shouldBe` calcO
    it "Test evalFactTL"         $ do (flipExecMockIO "10\n"        . simpleEvalTL) factTL         `shouldBe` factO
    it "Test evalHanoiTL"        $ do (flipExecMockIO "1\n"         . simpleEvalTL) hanoiTL        `shouldBe` hanoiO
    it "Test evalLocTestTL"      $ do (flipExecMockIO "1\n2\n"      . simpleEvalTL) locTestTL      `shouldBe` locTestO
    it "Test evalNameTL"         $ do (flipExecMockIO "WriteOnly\n" . simpleEvalTL) nameTL         `shouldBe` nameO
    it "Test evalTruthMachineTL" $ do (flipExecMockIO "0\n"         . simpleEvalTL) truthMachineTL `shouldBe` zeroO

  describe "freedom" $ do
    it "Test evalCountTL"        $ do (batchExecMockIO              . interpretFreeIOToWrapperIO . simpleEvalTL) countTL        `shouldBe` countO
    it "Test evalHelloWorldTL"   $ do (batchExecMockIO              . interpretFreeIOToWrapperIO . simpleEvalTL) helloWorldTL   `shouldBe` helloWorldO
    it "Test evalHWorldTL"       $ do (batchExecMockIO              . interpretFreeIOToWrapperIO . simpleEvalTL) hWorldTL       `shouldBe` hWorldO
    it "Test evalCalcTL"         $ do (flipExecMockIO "-1\n"        . interpretFreeIOToWrapperIO . simpleEvalTL) calcTL         `shouldBe` calcO
    it "Test evalFactTL"         $ do (flipExecMockIO "10\n"        . interpretFreeIOToWrapperIO . simpleEvalTL) factTL         `shouldBe` factO
    it "Test evalHanoiTL"        $ do (flipExecMockIO "1\n"         . interpretFreeIOToWrapperIO . simpleEvalTL) hanoiTL        `shouldBe` hanoiO
    it "Test evalLocTestTL"      $ do (flipExecMockIO "1\n2\n"      . interpretFreeIOToWrapperIO . simpleEvalTL) locTestTL      `shouldBe` locTestO
    it "Test evalNameTL"         $ do (flipExecMockIO "WriteOnly\n" . interpretFreeIOToWrapperIO . simpleEvalTL) nameTL         `shouldBe` nameO
    it "Test evalTruthMachineTL" $ do (flipExecMockIO "0\n"         . interpretFreeIOToWrapperIO . simpleEvalTL) truthMachineTL `shouldBe` zeroO

  describe "logging" $ do

    describe "truthMachineIL" $ do
      it "Test evalMockIO"           $ do (flipEvalMockIO "0\n" . interpretFreeIOToWrapperIO .             simpleEvalTL) truthMachineTL `shouldBe` ""
      it "Test evalMockIO logInput"  $ do (flipEvalMockIO "0\n" . interpretFreeIOToWrapperIO . logInput  . simpleEvalTL) truthMachineTL `shouldBe` zeroO
      it "Test evalMockIO logOutput" $ do (flipEvalMockIO "0\n" . interpretFreeIOToWrapperIO . logOutput . simpleEvalTL) truthMachineTL `shouldBe` zeroO

    describe "calcTL" $ do
      it "Test evalMockIO"                     $ do (flipEvalMockIO "-1\n" . interpretFreeIOToWrapperIO .                         simpleEvalTL) calcTL `shouldBe` ""
      it "Test evalMockIO logInput"            $ do (flipEvalMockIO "-1\n" . interpretFreeIOToWrapperIO . logInput              . simpleEvalTL) calcTL `shouldBe` "-1"
      it "Test evalMockIO logOutput"           $ do (flipEvalMockIO "-1\n" . interpretFreeIOToWrapperIO . logOutput             . simpleEvalTL) calcTL `shouldBe` calcO
      it "Test evalMockIO logInput logOutput"  $ do (flipEvalMockIO "-1\n" . interpretFreeIOToWrapperIO . logInput  . logOutput . simpleEvalTL) calcTL `shouldBe` calcOLog
      it "Test evalMockIO logOutput logInput"  $ do (flipEvalMockIO "-1\n" . interpretFreeIOToWrapperIO . logOutput . logInput  . simpleEvalTL) calcTL `shouldBe` calcOLog
