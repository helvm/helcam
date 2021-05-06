module HelVM.HelMA.Common.IO.WrapperIO (
  WrapperIO,
  wGetChar,
  wPutChar,
  wGetLine,
  wPutStr,
  wPutStrLn,
  wFlush,
  wGetInt,
  wPutInt,
  wLogStr,
  wLogStrLn
) where

import qualified System.IO as IO

class Monad m => WrapperIO m where
  wGetChar  :: m Char
  wPutChar  :: Char -> m ()
  wGetLine  :: m String
  wPutStr   :: String -> m ()
  wPutStrLn :: String -> m ()
  wFlush    :: m ()
  wGetInt   :: m Int
  wPutInt   :: Int -> m ()
  wLogStr   :: String -> m ()
  wLogStrLn :: String -> m ()
  wPutStrLn s = wPutStr $ s <> "\n"
  wFlush      = pass
  wPutInt     = wPutChar . chr
  wGetInt     = do ord <$> wGetChar
  wLogStrLn s = wLogStr $ s <> "\n"

instance WrapperIO IO where
  wGetChar  = IO.getChar
  wPutChar  = IO.putChar
  wGetLine  = IO.getLine
  wPutStr   = putStr
  wPutStrLn = putStrLn
  wFlush    = IO.hFlush stdout
  wLogStr   = IO.hPutStr stderr
