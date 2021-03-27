module HelVM.HelCam.Machines.BrainFuck.TokensSpec (spec) where

import HelVM.HelCam.Machines.BrainFuck.EvaluatorSpecData

import HelVM.HelCam.Machines.BrainFuck.Lexer

import Test.Hspec
import Test.Hspec.Contrib.HUnit (fromHUnitTest)
import Test.HUnit

spec :: Spec
spec = fromHUnitTest testsOfBFTokens

testsOfBFTokens :: Test
testsOfBFTokens = test
  [ "tokenizeHelloWorld"             ~: "tokenize helloWorld"             ~: helloWorld       ~=? show (readTokens helloWorld)
  , "tokenizeHelloWorldWithComments" ~: "tokenize helloWorldWithComments" ~: helloWorld       ~=? show (readTokens helloWorldWithComments)
  , "testTokenAsList"                ~: "testTokenAsList"                 ~: helloWorldAsList ~=? show (tokenList $ readTokens helloWorldWithComments)
  ]

helloWorldAsList :: String
helloWorldAsList = "[+,+,+,+,+,+,+,+,[,>,+,+,+,+,[,>,+,+,>,+,+,+,>,+,+,+,>,+,<,<,<,<,-,],>,+,>,+,>,-,>,>,+,[,<,],<,-,],>,>,.,>,-,-,-,.,+,+,+,+,+,+,+,.,.,+,+,+,.,>,>,.,<,-,.,<,.,+,+,+,.,-,-,-,-,-,-,.,-,-,-,-,-,-,-,-,.,>,>,+,.,>,+,+,.]"
