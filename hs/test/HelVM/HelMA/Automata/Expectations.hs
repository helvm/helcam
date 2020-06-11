module HelVM.HelMA.Automata.Expectations where

import Test.Hspec

infix 1 `shouldBeDo`
shouldBeDo :: (HasCallStack, Show a, Eq a) => a -> IO a -> Expectation
shouldBeDo action expected = shouldBe action =<< expected

infix 1 `goldenShouldBe`
goldenShouldBe :: (HasCallStack, Show a, Eq a) => IO a -> IO a -> Expectation
goldenShouldBe action expected = join $ liftA2 shouldBe action expected
