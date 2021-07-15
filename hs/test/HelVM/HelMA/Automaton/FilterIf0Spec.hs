module HelVM.HelMA.Automaton.FilterIf0Spec (spec) where

import HelVM.HelMA.Automaton.IO.BusinessIO
import HelVM.HelMA.Automaton.IO.MockIO

import Test.Hspec

wFilterIf0 :: BusinessIO m => m ()
wFilterIf0 = do
  char <- wGetChar
  if char == '0'
    then do
      wLogStrLn ""
      wPutChar '\n'
    else do
      wPutChar char
      wFilterIf0

spec :: Spec
spec = do
  describe "Test WFilter0" $ do
    it "Test WFilterIf0 with outputMockIO" $ do outputMockIO wFilterIf0 "qwerty0uiop" `shouldBe` "qwerty\n"
    it "Test WFilterIf0 with outputMockIO" $ do loggedMockIO wFilterIf0 "qwerty0uiop" `shouldBe` "\n"
