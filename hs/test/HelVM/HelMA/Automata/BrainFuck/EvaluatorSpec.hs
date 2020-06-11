module HelVM.HelMA.Automata.BrainFuck.EvaluatorSpec (spec) where

import HelVM.HelMA.Automata.BrainFuck.Evaluator
import HelVM.HelMA.Automata.BrainFuck.EvaluatorSpecData
import HelVM.HelMA.Automata.BrainFuck.FileUtil

import HelVM.HelMA.Automata.Expectations

import HelVM.HelMA.Common.IO.FreeIO
import HelVM.HelMA.Common.IO.MockIO

import Test.Hspec

spec :: Spec
spec = do
  describe "interact" $ do
    it "value256 for Int8"     $ do batchEvalInt8  <$> readBfFile "source/value256"              `shouldReturn` occtet
    it "value256 for Word8"    $ do batchEvalWord8 <$> readBfFile "source/value256"              `shouldReturn` occtet
    it "helloWorld"            $ do batchEvalWord8 <$> readBfFile "source/helloWorld"            `shouldReturn` helloWorldExpected
    it "fascistHelloWorld"     $ do batchEvalWord8 <$> readBfFile "source/fascistHelloWorld"     `shouldReturn` helloWorldExpected
    it "padHelloWorld"         $ do batchEvalWord8 <$> readBfFile "source/padHelloWorld"         `shouldReturn` hello_WorldExpected
    it "theShortestHelloWorld" $ do batchEvalWord8 <$> readBfFile "source/theShortestHelloWorld" `shouldReturn` hello_WorldExpected
    it "99botles"              $ do batchEvalWord8 <$> readBfFile "source/99botles"              `goldenShouldBe` (toWindows <$> readOutFile "99botles")
    it "triangle"              $ do batchEvalWord8 <$> readBfFile "source/triangle"              `goldenShouldBe` (toNR      <$> readOutFile "triangle")
    it "fibonacci"             $ do flipEvalWord8 "0\r\n" <$> readBfFile "source/fibonacci"      `goldenShouldBe` (toMac     <$> readOutFile "fibonacci")

  describe "monadic" $ do
    it "value256 for Int8"     $ do batchExecMockIO . evalInt8  <$> readBfFile "source/value256"              `shouldReturn` occtet
    it "value256 for Word8"    $ do batchExecMockIO . evalWord8 <$> readBfFile "source/value256"              `shouldReturn` occtet
    it "helloWorld"            $ do batchExecMockIO . evalWord8 <$> readBfFile "source/helloWorld"            `shouldReturn` helloWorldExpected
    it "fascistHelloWorld"     $ do batchExecMockIO . evalWord8 <$> readBfFile "source/fascistHelloWorld"     `shouldReturn` helloWorldExpected
    it "padHelloWorld"         $ do batchExecMockIO . evalWord8 <$> readBfFile "source/padHelloWorld"         `shouldReturn` hello_WorldExpected
    it "theShortestHelloWorld" $ do batchExecMockIO . evalWord8 <$> readBfFile "source/theShortestHelloWorld" `shouldReturn` hello_WorldExpected

  describe "freedom" $ do
    it "value256 for Int8"     $ do batchExecMockIO . interpretFreeIOToWrapperIO . evalInt8  <$> readBfFile "source/value256"              `shouldReturn` occtet
    it "value256 for Word8"    $ do batchExecMockIO . interpretFreeIOToWrapperIO . evalWord8 <$> readBfFile "source/value256"              `shouldReturn` occtet
    it "helloWorld"            $ do batchExecMockIO . interpretFreeIOToWrapperIO . evalWord8 <$> readBfFile "source/helloWorld"            `shouldReturn` helloWorldExpected
    it "fascistHelloWorld"     $ do batchExecMockIO . interpretFreeIOToWrapperIO . evalWord8 <$> readBfFile "source/fascistHelloWorld"     `shouldReturn` helloWorldExpected
    it "padHelloWorld"         $ do batchExecMockIO . interpretFreeIOToWrapperIO . evalWord8 <$> readBfFile "source/padHelloWorld"         `shouldReturn` hello_WorldExpected
    it "theShortestHelloWorld" $ do batchExecMockIO . interpretFreeIOToWrapperIO . evalWord8 <$> readBfFile "source/theShortestHelloWorld" `shouldReturn` hello_WorldExpected

  describe "FreeIO" $ do
    it "value256 for Int8"     $ do batchEvalMockIO . interpretFreeIOToWrapperIO . evalInt8              <$> readBfFile "source/value256" `shouldReturn` ""
    it "value256 for Int8"     $ do batchEvalMockIO . interpretFreeIOToWrapperIO . logOutput . evalInt8  <$> readBfFile "source/value256" `shouldReturn` occtet


toMac :: String -> String
toMac s = charNToR <$> s

charNToR :: Char -> Char
charNToR '\n' = '\r'
charNToR c    = c

toWindows :: String -> String
toWindows s = charNToRN =<< s

charNToRN :: Char -> String
charNToRN '\n' = "\r\n"
charNToRN c    = one c

toNR :: String -> String
toNR s = charNToNR =<< s

charNToNR :: Char -> String
charNToNR '\n' = "\n\r"
charNToNR c    = one c
