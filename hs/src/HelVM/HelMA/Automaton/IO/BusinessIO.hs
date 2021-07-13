module HelVM.HelMA.Automaton.IO.BusinessIO (
  SREvaluator,
  REvaluator,
  SEvaluator,
  Evaluator,
  Element,
  BIO,
  BusinessIO,
  wGetChar,
  wPutChar,
  wGetLine,
  wPutStr,
  wPutStrLn,
  wFlush,
  wPutInt,
  wPutIntegral,
  wLogStr,
  wLogStrLn,
  wLogShow,
) where

import HelVM.Common.Safe

import HelVM.HelMA.Automaton.Memories.RAMConst   as RAM
import HelVM.HelMA.Automaton.Memories.StackConst as Stack
  
import Data.Default as Default
  
import qualified System.IO as IO

type SREvaluator e s r m = (Stack e s , RAM e r , Evaluator e m)
type REvaluator e r m = (RAM e r , Evaluator e m)
type SEvaluator e s m = (Stack e s , Evaluator e m)
type Evaluator e m = (Element e , BIO m)
type Element e  = (Default e , Read e , Show e , Integral e)

type BIO m = (MonadSafeError m , BusinessIO m)

class Monad m => BusinessIO m where
  wGetChar     :: m Char
  wPutChar     :: Char -> m ()
  wGetLine     :: m Text
  wPutStr      :: Text -> m ()
  wPutStrLn    :: Text -> m ()
  wFlush       :: m ()
  wPutInt      :: Int -> m ()
  wPutIntegral :: Integral v => v -> m ()
  wLogStr      :: Text -> m ()
  wLogStrLn    :: Text -> m ()
  wLogShow     :: Show s => s -> m ()
  wPutStrLn s  = wPutStr $ s <> "\n"
  wFlush       = pass
  wPutInt      = wPutChar . chr
  wPutIntegral = wPutInt . fromIntegral
  wLogStrLn s  = wLogStr $ s <> "\n"
  wLogShow     = wLogStrLn . show

instance BusinessIO IO where
  wGetChar  = IO.getChar
  wPutChar  = IO.putChar
  wGetLine  = getLine
  wPutStr   = putText
  wPutStrLn = putTextLn
  wFlush    = hFlush stdout
  wLogStr   = IO.hPutStr stderr . toString

instance BusinessIO (SafeExceptT IO) where
  wGetChar  = hoistMonad   IO.getChar
  wPutChar  = hoistMonad . IO.putChar
  wGetLine  = hoistMonad   getLine
  wPutStr   = hoistMonad . putText
  wPutStrLn = hoistMonad . putTextLn
  wFlush    = hoistMonad $ hFlush stdout
  wLogStr   = hoistMonad . IO.hPutStr stderr . toString
