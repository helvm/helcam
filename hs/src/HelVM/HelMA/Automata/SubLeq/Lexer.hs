module HelVM.HelMA.Automata.SubLeq.Lexer where

import HelVM.HelMA.Automata.SubLeq.Symbol
import HelVM.HelMA.Common.Util

import Data.List.Split

tokenize :: Source -> SymbolList
tokenize source = (maybeToList . readMaybe) =<< splitOneOf " \t\n" source
