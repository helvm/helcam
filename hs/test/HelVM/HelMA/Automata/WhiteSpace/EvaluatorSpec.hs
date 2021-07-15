module HelVM.HelMA.Automata.WhiteSpace.EvaluatorSpec (spec) where

import HelVM.HelMA.Automata.WhiteSpace.Evaluator
import HelVM.HelMA.Automata.WhiteSpace.EvaluatorSpecData
import HelVM.HelMA.Automata.WhiteSpace.FileUtil
import HelVM.HelMA.Automata.WhiteSpace.Instruction

import HelVM.CartesianProduct
import HelVM.WrappedGoldenIO

import HelVM.HelMA.Automaton.IO.MockIO
import HelVM.HelMA.Automaton.Types.RAMType
import HelVM.HelMA.Automaton.Types.StackType
import HelVM.HelMA.Automaton.Types.TokenType

import HelVM.Common.Safe

import System.FilePath.Posix

import Test.Hspec

spec :: Spec
spec = do
  describe "simpleEval" $ do
    let options =  [True , False] |><< stackTypes |><| ramTypes
    describe "ws" $ do
      describe "original" $ do
        let majorPath = "simpleEval" </> "original" </> "ws"
        forM_ ([ ("count"        , ""           )
               , ("hworld"       , ""           )
               , ("calc"         , "-1\n"       )
               , ("fact"         , "10\n"       )
               , ("hanoi"        , "1\n"        )
               , ("loctest"      , "1\n2\n"     )
               , ("name"         , "WriteOnly\n")
               ] >><<< options) $ \(fileName , input , ascii , stackType , ramType) -> do
          let params = (WhiteTokenType ,  , ascii , stackType , ramType) <$> readWsFile ("original" </> fileName)
          let exec f = safeIOToIO $ f input . runExceptT . simpleEval <$> params
          let minorPath = show ascii <-> show stackType <-> show ramType </> fileName
          describe minorPath $ do
            it ("monadic" </> minorPath) $ do
              exec flipOutputSafeMockIO `goldenShouldReturn` buildAbsoluteOutFileName (majorPath </> "monadic" </> minorPath)
            it ("logging" </> minorPath) $ do
              exec flipLoggedSafeMockIO `goldenShouldReturn` buildAbsoluteOutFileName (majorPath </> "logging" </> minorPath)

    describe "stn" $ do

      describe "from-wsa" $ do
        let majorPath = "simpleEval" </> "from-wsa" </> "stn"
        forM_ ([ ("true"        , ""           )
               , ("hello"       , ""           )
               , ("hello2"      , ""           )
               , ("hello4"      , ""           )
               , ("bottles"     , ""           )
               , ("prim"        , ""           )
               ] >><<< options) $ \(fileName , input , ascii , stackType , ramType) -> do
          let params = (VisibleTokenType ,  , ascii , stackType , ramType) <$> readStnFile ("from-wsa" </> fileName)
          let exec f = safeIOToIO $ f input . runExceptT . simpleEval <$> params
          let minorPath = show ascii <-> show stackType <-> show ramType </> fileName
          describe minorPath $ do
            it ("monadic" </> minorPath) $ do
              exec flipOutputSafeMockIO `goldenShouldReturn` buildAbsoluteOutFileName (majorPath </> "monadic" </> minorPath)
            it ("logging" </> minorPath) $ do
              exec flipLoggedSafeMockIO `goldenShouldReturn` buildAbsoluteOutFileName (majorPath </> "logging" </> minorPath)

      describe "original" $ do
        let majorPath = "simpleEval" </> "original" </> "stn"
        forM_ ([ ("count"        , ""           )
               , ("helloWorld"   , ""           )
               , ("hWorld"       , ""           )
               , ("calc"         , "-1\n"       )
               , ("fact"         , "10\n"       )
               , ("hanoi"        , "1\n"        )
               , ("locTest"      , "1\n2\n"     )
               , ("name"         , "WriteOnly\n")
               , ("truthMachine" , "0\n"        )
               ] >><<< options) $ \(fileName , input , ascii , stackType , ramType) -> do
          let params = (VisibleTokenType ,  , ascii , stackType , ramType) <$> readStnFile ("original" </> fileName)
          let exec f = safeIOToIO $ f input . runExceptT . simpleEval <$> params
          let minorPath = show ascii <-> show stackType <-> show ramType </> fileName
          describe minorPath $ do
            it ("monadic" </> minorPath) $ do
              exec flipOutputSafeMockIO `goldenShouldReturn` buildAbsoluteOutFileName (majorPath </> "monadic" </> minorPath)
            it ("logging" </> minorPath) $ do
              exec flipLoggedSafeMockIO `goldenShouldReturn` buildAbsoluteOutFileName (majorPath </> "logging" </> minorPath)

  describe "simpleEvalTL" $ do
    forM_ [ ("countTL"        , countTL        , ""           )
          , ("helloWorldTL"   , helloWorldTL   , ""           )
          , ("hWorldTL"       , hWorldTL       , ""           )
          , ("calcTL"         , calcTL         , "-1\n"       )
          , ("factTL"         , factTL         , "10\n"       )
          , ("hanoiTL"        , hanoiTL        , "1\n"        )
          , ("locTestTL"      , locTestTL      , "1\n2\n"     )
          , ("nameTL"         , nameTL         , "WriteOnly\n")
          , ("truthMachineTL" , truthMachineTL , "0\n"        )
          ] $ \(fileName , tl , input) -> do
      let exec f = safeToIO ((f input . runExceptT . simpleEvalTL) tl)
      describe fileName $ do
        it ("monadic" </> fileName) $ do
          exec flipOutputSafeMockIO `goldenShouldReturn` buildAbsoluteOutFileName ("simpleEvalTL" </> "monadic" </> fileName)
        it ("logging" </> fileName) $ do
          exec flipLoggedSafeMockIO `goldenShouldReturn` buildAbsoluteOutFileName ("simpleEvalTL" </> "logging" </> fileName)

  describe "simpleEvalIL" $ do
    forM_ [ ("call"     , [Call "A", End , Mark "A", Return] , "")
          , ("push-pop" , [Liter 0 , Discard , End]          , "")
          ] $ \(fileName , il , input) -> do
      let exec = safeToIO $ flipLoggedSafeMockIO input (runExceptT $ evalIL il SeqStackType IntMapRAMType)
      it fileName $ do
        exec `goldenShouldReturn` buildAbsoluteOutFileName ("simpleEvalIL" </> "logging" </> fileName)
