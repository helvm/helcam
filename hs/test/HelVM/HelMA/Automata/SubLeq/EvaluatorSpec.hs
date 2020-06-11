module HelVM.HelMA.Automata.SubLeq.EvaluatorSpec (spec) where

import HelVM.HelMA.Automata.SubLeq.Evaluator
import HelVM.HelMA.Automata.SubLeq.EvaluatorSpecData
import HelVM.HelMA.Automata.SubLeq.FileUtil

import HelVM.HelMA.Common.IO.FreeIO
import HelVM.HelMA.Common.IO.MockIO

import Test.Hspec

spec :: Spec
spec = do
  describe "interact" $ do
    it "Test hello." $ do batchSimpleEval <$> readSqFile "hello" `shouldReturn` hello
    it "Test hello." $ do batchSimpleEvalIL helloSQIL `shouldBe` hello

  describe "monadic" $ do
    it "Test hello." $ do (batchExecMockIO . simpleEval) <$> readSqFile "hello" `shouldReturn` hello
    it "Test hello." $ do (batchExecMockIO . simpleEvalIL) helloSQIL `shouldBe` hello

  describe "freedom" $ do
    it "Test hello." $ do (batchExecMockIO . interpretFreeIOToWrapperIO . simpleEval) <$> readSqFile "hello" `shouldReturn` hello
    it "Test hello." $ do (batchExecMockIO . interpretFreeIOToWrapperIO . simpleEvalIL) helloSQIL `shouldBe` hello
