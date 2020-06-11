module HelVM.HelMA.Automata.ETA.Lexer where

import HelVM.HelMA.Automata.ETA.Token
import HelVM.HelMA.Common.Util

import qualified Text.Read as Read
import qualified Text.Show

-- Lexer

tokenize :: String -> TokenList
tokenize =  toTokenList . readTokens

readTokens :: String -> WhiteTokens
readTokens source = (Read.read . toUppers) source :: WhiteTokens

toTokenList :: WhiteTokens -> TokenList
toTokenList (WhiteTokens tokens) = whiteTokenToToken <$> tokens

-- WhiteTokens

newtype WhiteTokens = WhiteTokens WhiteTokenList deriving (Eq)

instance Show WhiteTokens where
  show (WhiteTokens tokens) = show =<< tokens

instance Read WhiteTokens where
  readsPrec _ source = [( WhiteTokens $ maybeToList . readMaybe . one =<< source, "")]
