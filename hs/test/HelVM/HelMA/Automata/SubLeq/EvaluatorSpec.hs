module HelVM.HelMA.Automata.SubLeq.EvaluatorSpec (spec) where

import HelVM.HelMA.Automata.SubLeq.Evaluator
import HelVM.HelMA.Automata.SubLeq.EvaluatorSpecData
import HelVM.HelMA.Automata.SubLeq.FileUtil

import HelVM.WrappedGoldenIO

import HelVM.HelMA.Automaton.IO.MockIO

import HelVM.Common.SafeExceptT

import System.FilePath.Posix

import Test.Hspec

spec :: Spec
spec = do

  describe "simpleEvalIL" $ do
    forM_ [ ("helloSQIL" , helloSQIL)
          ] $ \(fileName , il)  -> do
      let exec f = (f . unsafeRunExceptT . simpleEvalIL) il 
      describe fileName $ do
        it "monadic"  $ do exec batchOutputMockIO `goldenShouldBe` buildAbsoluteOutFileName ("simpleEvalIL" </> "monadic" </> fileName)
        it "monadic"  $ do exec batchLoggedMockIO `goldenShouldBe` buildAbsoluteOutFileName ("simpleEvalIL" </> "logging" </> fileName)

  describe "simpleEval" $ do
    forM_ [ ("hello"     , "" )
          , ("longHello" , "" )
          ] $ \(fileName , input)  -> do
      let exec f = f input . unsafeRunExceptT . simpleEval <$> (readSqFile fileName) 
      describe fileName $ do
        it "monadic"  $ do exec flipOutputMockIO `goldenShouldReturn` buildAbsoluteOutFileName ("simpleEval" </> "monadic" </> fileName)
        it "logging"  $ do exec flipLoggedMockIO `goldenShouldReturn` buildAbsoluteOutFileName ("simpleEval" </> "logging" </> fileName)
