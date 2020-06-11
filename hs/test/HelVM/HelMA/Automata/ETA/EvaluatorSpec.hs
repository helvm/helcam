module HelVM.HelMA.Automata.ETA.EvaluatorSpec (spec) where

import HelVM.HelMA.Automata.ETA.Evaluator

import HelVM.HelMA.Automata.ETA.EvaluatorSpecData
import HelVM.HelMA.Automata.ETA.FileUtil

import HelVM.HelMA.Automata.Expectations

import HelVM.HelMA.Common.IO.FreeIO
import HelVM.HelMA.Common.IO.MockIO

import Test.Hspec

spec :: Spec
spec = do
  describe "interact" $ do
    it "hello"      $ do batchSimpleEval <$> readEtaFile "source/hello"      `shouldReturn` hello
    it "hello2"     $ do batchSimpleEval <$> readEtaFile "source/hello2"     `shouldReturn` hello
    it "raw/hello2" $ do batchSimpleEval <$> readEtaFile "raw/hello2" `shouldReturn` hello
    it "crlf"       $ do batchSimpleEval <$> readEtaFile "source/crlf"       `shouldReturn` crlf
    it "bottles"    $ do batchSimpleEval <$> readEtaFile "source/bottles"    `goldenShouldBe` readOutFile "bottles"

  describe "monadic" $ do
    it "hello"      $ do batchExecMockIO . simpleEval <$> readEtaFile "source/hello"      `shouldReturn` hello
    it "hello2"     $ do batchExecMockIO . simpleEval <$> readEtaFile "source/hello2"     `shouldReturn` hello
    it "raw/hello2" $ do batchExecMockIO . simpleEval <$> readEtaFile "raw/hello2" `shouldReturn` hello
    it "crlf"       $ do batchExecMockIO . simpleEval <$> readEtaFile "source/crlf"       `shouldReturn` crlf
    it "bottles"    $ do batchExecMockIO . simpleEval <$> readEtaFile "source/bottles"    `goldenShouldBe` readOutFile "bottles"

  describe "freedom" $ do
    it "hello"      $ do batchExecMockIO . interpretFreeIOToWrapperIO . simpleEval <$> readEtaFile "source/hello"      `shouldReturn` hello
    it "hello2"     $ do batchExecMockIO . interpretFreeIOToWrapperIO . simpleEval <$> readEtaFile "source/hello2"     `shouldReturn` hello
    it "raw/hello2" $ do batchExecMockIO . interpretFreeIOToWrapperIO . simpleEval <$> readEtaFile "raw/hello2" `shouldReturn` hello
    it "crlf"       $ do batchExecMockIO . interpretFreeIOToWrapperIO . simpleEval <$> readEtaFile "source/crlf"       `shouldReturn` crlf
    it "bottles"    $ do batchExecMockIO . interpretFreeIOToWrapperIO . simpleEval <$> readEtaFile "source/bottles"    `goldenShouldBe` readOutFile "bottles"

  describe "from eas" $ do
    it "eas/true"     $ do batchSimpleEval      <$> readEtaFile "eas/true"     `shouldReturn` ""
    it "eas/hello"    $ do batchSimpleEval      <$> readEtaFile "eas/hello"    `shouldReturn` hello
    it "eas/hello2"   $ do batchSimpleEval      <$> readEtaFile "eas/hello2"   `shouldReturn` hello
    it "eas/hello3"   $ do batchSimpleEval      <$> readEtaFile "eas/hello3"   `shouldReturn` hello
    it "eas/hello4"   $ do batchSimpleEval      <$> readEtaFile "eas/hello4"   `shouldReturn` hello
    it "eas/readnum"  $ do flipSimpleEval "0\n" <$> readEtaFile "eas/readnum"  `shouldReturn` ""
    it "eas/readnum"  $ do flipSimpleEval "1\n" <$> readEtaFile "eas/readnum"  `shouldReturn` ""
    it "eas/fact"     $ do flipSimpleEval "0\n" <$> readEtaFile "eas/fact"     `shouldReturn` ""
    it "eas/fact"     $ do flipSimpleEval "1\n" <$> readEtaFile "eas/fact"     `shouldReturn` ""
    it "eas/bottles"  $ do batchSimpleEval      <$> readEtaFile "eas/bottles"  `goldenShouldBe` readOutFile "eas/bottles"
