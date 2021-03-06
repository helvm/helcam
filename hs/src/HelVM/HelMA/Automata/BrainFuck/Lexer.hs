module HelVM.HelMA.Automata.BrainFuck.Lexer where

import HelVM.HelMA.Automata.BrainFuck.Token

import HelVM.HelMA.Automaton.API.IOTypes
import HelVM.Common.ReadText
import HelVM.HelMA.Automaton.WrapTokenList

-- Lexer
tokenize :: Source -> TokenList
tokenize =  unWrapTokenList . readTokens

readTokens :: Source -> Tokens
readTokens source = readText source :: Tokens

type Tokens = WrapTokenList TokenList
