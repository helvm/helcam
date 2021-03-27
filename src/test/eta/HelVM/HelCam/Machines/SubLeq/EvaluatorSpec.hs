module HelVM.HelCam.Machines.SubLeq.EvaluatorSpec (spec) where

import HelVM.HelCam.Machines.SubLeq.Evaluator
import HelVM.HelCam.Machines.SubLeq.EvaluatorSpecData

import HelVM.HelCam.Common.MockIO

import Test.Hspec
import Test.Hspec.Contrib.HUnit (fromHUnitTest)
import Test.HUnit

spec :: Spec
spec = fromHUnitTest testsOfSQEvaluator

testsOfSQEvaluator :: Test
testsOfSQEvaluator = TestList
  [ "eval_hello"   ~: "Test hello."     ~: hello     ~=? batchSimpleEvalIL helloSQIL
--  , "eval_hello2"  ~: "Test hello2."  ~: hello     ~=? batchExecMockIO (eval hello2ETA)

  , "eval_hello"   ~: "Test hello."     ~: hello     ~=? batchExecMockIO (simpleEvalIL helloSQIL)
--  , "eval_hello2"  ~: "Test hello2."  ~: hello     ~=? batchExecMockIO (eval hello2ETA)
  ]
