module HelVM.HelMA.Automata.WhiteSpace.Lexer where

import HelVM.HelMA.Automata.WhiteSpace.Token

import qualified Text.Read as Read
import qualified Text.Show

-- Lexer

tokenize :: String -> TokenList
tokenize =  tokenList . readTokens

readTokens :: String -> WhiteTokens
readTokens source = Read.read source :: WhiteTokens

tokenList :: WhiteTokens -> TokenList
tokenList (WhiteTokens tokens) = whiteTokenToToken <$> tokens

-- WhiteTokens

newtype WhiteTokens = WhiteTokens WhiteTokenList deriving (Eq)

instance Show WhiteTokens where
  show (WhiteTokens tokens) = show =<< tokens

instance Read WhiteTokens where
  readsPrec _ source = [( WhiteTokens $ maybeToList . readMaybe . one =<< source, "")]
