module HelVM.HelMA.Automata.Rev.EvaluatorSpec (spec) where

import HelVM.HelMA.Automata.Rev.Evaluator

import HelVM.HelMA.Common.IO.FreeIO
import HelVM.HelMA.Common.IO.MockIO
import HelVM.HelMA.Common.Util

import Test.Hspec

spec :: Spec
spec = do
  describe "interact" $ do
    it "Hello, world!" $ do batchEval hw `shouldBe` hwo

  describe "monadic" $ do
    it "Hello, world!" $ do (batchExecMockIO . eval) hw `shouldBe` hwo

  describe "freedom" $ do
    it "Hello, world!" $ do (batchExecMockIO . interpretFreeIOToWrapperIO . eval) hw `shouldBe` hwo

hw :: Source
hw = "#!/usr/bin/rev\n!dlrow ,olleH\n"

hwo :: Output
hwo = "ver/nib/rsu/!#\nHello, world!\n"