module HelVM.HelMA.Automata.SubLeq.EvaluatorSpec (spec) where

import HelVM.HelMA.Automata.SubLeq.Evaluator
import HelVM.HelMA.Automata.SubLeq.EvaluatorSpecData
import HelVM.HelMA.Automata.SubLeq.FileUtil

import HelVM.WrappedGoldenIO

import HelVM.HelMA.Automaton.IO.MockIO

import HelVM.Common.Safe

import System.FilePath.Posix

import Test.Hspec

spec :: Spec
spec = do

  describe "simpleEvalIL" $ do
    forM_ [ ("helloSQIL" , helloSQIL)
          ] $ \(fileName , il)  -> do
      let exec f = safeToIO $ (f . runExceptT . simpleEvalIL) il
      describe fileName $ do
        it "monadic" $ do
          exec batchOutputSafeMockIO `goldenShouldReturn` buildAbsoluteOutFileName ("simpleEvalIL" </> "monadic" </> fileName)
        it "logging" $ do
          exec batchLoggedSafeMockIO `goldenShouldReturn` buildAbsoluteOutFileName ("simpleEvalIL" </> "logging" </> fileName)

  describe "simpleEval" $ do
    forM_ [ ("hello"     , "" )
          , ("longHello" , "" )
          ] $ \(fileName , input)  -> do
      let exec f = safeIOToIO $ f input . runExceptT . simpleEval <$> readSqFile fileName
      describe fileName $ do
        it "monadic" $ do
          exec flipOutputSafeMockIO `goldenShouldReturn` buildAbsoluteOutFileName ("simpleEval" </> "monadic" </> fileName)
        it "logging" $ do
          exec flipLoggedSafeMockIO `goldenShouldReturn` buildAbsoluteOutFileName ("simpleEval" </> "logging" </> fileName)
