module HelVM.HelMA.Automata.ETA.LexerSpec (spec) where

import HelVM.HelMA.Automata.ETA.EvaluatorSpecData
import HelVM.HelMA.Automata.ETA.FileUtil

import HelVM.HelMA.Automata.ETA.Lexer

import HelVM.HelMA.Automata.Expectations

import Test.Hspec

spec :: Spec
spec = do
  describe "raw" $ do
    forM_ [ "hello"
          , "hello2"
          , "pip"
          , "pip2"
          , "fact"
          , "bottles"
          , "crlf"
          ] $ \filename -> do
       it filename $ do (show . readTokens <$> readEtaFile ("source/" <> filename)) `goldenShouldBe` readEtaFile ("raw/" <> filename)

  describe "tokenize" $ do

    describe "original ETA" $ do
      it "hello"   $ do tokenize <$> readEtaFile "source/hello"   `shouldReturn` helloTL
      it "hello2"  $ do tokenize <$> readEtaFile "source/hello2"  `shouldReturn` hello2TL
      it "pip"     $ do tokenize <$> readEtaFile "source/pip"     `shouldReturn` pipTL
      it "pip2"    $ do tokenize <$> readEtaFile "source/pip2"    `shouldReturn` pip2TL
      it "fact"    $ do tokenize <$> readEtaFile "source/fact"    `shouldReturn` factTL
      it "bottles" $ do tokenize <$> readEtaFile "source/bottles" `shouldReturn` bottlesTL
      it "crlf"    $ do tokenize <$> readEtaFile "source/crlf"    `shouldReturn` crlfTL

    describe "from EAS" $ do
      it "eas/true"     $ do tokenize <$> readEtaFile "eas/true"     `shouldReturn` trueEASTL
      it "eas/hello"    $ do tokenize <$> readEtaFile "eas/hello"    `shouldReturn` helloEASTL
      it "eas/pip"      $ do tokenize <$> readEtaFile "eas/pip"      `shouldReturn` pipEASTL
      it "eas/pip2"     $ do tokenize <$> readEtaFile "eas/pip2"     `shouldReturn` pip2EASTL
      it "eas/reverse"  $ do tokenize <$> readEtaFile "eas/reverse"  `shouldReturn` reverseEASTL
      it "eas/function" $ do tokenize <$> readEtaFile "eas/function" `shouldReturn` functionEASTL
      it "eas/writestr" $ do tokenize <$> readEtaFile "eas/writestr" `shouldReturn` writeStrEASTL
      it "eas/hello2"   $ do tokenize <$> readEtaFile "eas/hello2"   `shouldReturn` hello2EASTL
      it "eas/hello3"   $ do tokenize <$> readEtaFile "eas/hello3"   `shouldReturn` hello3EASTL
      it "eas/hello4"   $ do tokenize <$> readEtaFile "eas/hello4"   `shouldReturn` hello4EASTL
      it "eas/writenum" $ do tokenize <$> readEtaFile "eas/writenum" `shouldReturn` writeNumEASTL
      it "eas/multiply" $ do tokenize <$> readEtaFile "eas/multiply" `shouldReturn` multiplyEASTL
      it "eas/readnum"  $ do tokenize <$> readEtaFile "eas/readnum"  `shouldReturn` readNumEASTL
      it "eas/fact"     $ do tokenize <$> readEtaFile "eas/fact"     `shouldReturn` factEASTL
      it "eas/bottles"  $ do tokenize <$> readEtaFile "eas/bottles"  `shouldReturn` bottlesEASTL
      it "eas/euclid"   $ do tokenize <$> readEtaFile "eas/euclid"   `shouldReturn` euclidEASTL
