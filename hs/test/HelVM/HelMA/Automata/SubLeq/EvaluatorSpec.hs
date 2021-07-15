module HelVM.HelMA.Automata.SubLeq.EvaluatorSpec (spec) where

import HelVM.HelMA.Automata.SubLeq.Evaluator
import HelVM.HelMA.Automata.SubLeq.EvaluatorSpecData
import HelVM.HelMA.Automata.SubLeq.FileUtil

import HelVM.WrappedGoldenIO

import HelVM.HelMA.Automaton.IO.MockIO

import System.FilePath.Posix

import Test.Hspec

spec :: Spec
spec = do

  describe "simpleEvalIL" $ do
    forM_ [ ("helloSQIL" , helloSQIL)
          ] $ \(fileName , il)  -> do
      let exec f = (f . runExceptT . simpleEvalIL) il
      describe fileName $ do
        it "monadic" $ do
          exec batchOutputSafeMockIO `goldenShouldSafe` buildAbsoluteOutFileName ("simpleEvalIL" </> "monadic" </> fileName)
        it "logging" $ do
          exec batchLoggedSafeMockIO `goldenShouldSafe` buildAbsoluteOutFileName ("simpleEvalIL" </> "logging" </> fileName)

  describe "simpleEval" $ do
    forM_ [ ("hello"     , "" )
          , ("longHello" , "" )
          ] $ \(fileName , input)  -> do
      let exec f = f input . runExceptT . simpleEval <$> readSqFile fileName
      describe fileName $ do
        it "monadic" $ do
          exec flipOutputSafeMockIO `goldenShouldSafeIO` buildAbsoluteOutFileName ("simpleEval" </> "monadic" </> fileName)
        it "logging" $ do
          exec flipLoggedSafeMockIO `goldenShouldSafeIO` buildAbsoluteOutFileName ("simpleEval" </> "logging" </> fileName)
