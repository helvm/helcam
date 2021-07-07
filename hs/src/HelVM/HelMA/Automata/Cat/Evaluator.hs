module HelVM.HelMA.Automata.Cat.Evaluator (
  evalParams,
  eval
) where

import HelVM.HelMA.Automaton.API.EvalParams
import HelVM.HelMA.Automaton.API.IOTypes
import HelVM.HelMA.Automaton.IO.BusinessIO

import HelVM.Common.SafeMonadT

evalParams :: BusinessIO m => EvalParams -> SafeMonadT m ()
evalParams = hoistMonad . eval . source

eval :: BusinessIO m => Source -> m ()
eval = wPutStr
