module HelVM.HelMA.Automata.BrainFuck.TokensSpec (spec) where

import HelVM.HelMA.Automata.BrainFuck.Lexer
import HelVM.HelMA.Automata.BrainFuck.FileUtil

import HelVM.HelMA.Automata.Expectations

import Test.Hspec

spec :: Spec
spec = do
  describe "tokenize" $ do
    it "helloWorld"             $ do (show . readTokens)             <$> readBfFile "source/helloWorld"             `goldenShouldBe` readBfFile "source/helloWorld"
    it "helloWorldWithComments" $ do (show . readTokens)             <$> readBfFile "source/helloWorldWithComments" `goldenShouldBe` readBfFile "source/helloWorld"
    it "helloWorldAsList"       $ do (show . tokenList . readTokens) <$> readBfFile "source/helloWorldWithComments" `shouldReturn`   helloWorldAsList

helloWorldAsList :: String
helloWorldAsList = "[+,+,+,+,+,+,+,+,[,>,+,+,+,+,[,>,+,+,>,+,+,+,>,+,+,+,>,+,<,<,<,<,-,],>,+,>,+,>,-,>,>,+,[,<,],<,-,],>,>,.,>,-,-,-,.,+,+,+,+,+,+,+,.,.,+,+,+,.,>,>,.,<,-,.,<,.,+,+,+,.,-,-,-,-,-,-,.,-,-,-,-,-,-,-,-,.,>,>,+,.,>,+,+,.]"
