module HelVM.HelMA.Common.IO.FreeIO (
  logOutput,
  logInput,
  interpretFreeIOToWrapperIO,
  FreeIO
) where

import HelVM.HelMA.Common.IO.WrapperIO

import Control.Monad.Free
import Control.Natural

logInput :: FreeIO ~> FreeIO
logInput = foldFree logInputF

logOutput :: FreeIO ~> FreeIO
logOutput = foldFree logOutputF

interpretFreeIOToWrapperIO :: WrapperIO m => FreeIO a -> m a
interpretFreeIOToWrapperIO = foldFree interpretFreeIOFToWrapperIO

type FreeIO = Free FreeIOF

----

logInputF :: FreeIOF a -> FreeIO a
logInputF (GetChar cd) = do
  c <- fGetChar
  liftF $ LogStr (one c) (cd c)
logInputF (GetLine cd) = do
  l <- fGetLine
  liftF $ LogStr l (cd l)
logInputF (GetInt cd) = do
  i <- fGetInt
  liftF $ LogStr (show i) (cd i)
logInputF other = liftF other

logOutputF :: FreeIOF a -> FreeIO a
logOutputF f@(PutChar c v) = do
  _ <- liftF $ LogStr (one c) v
  liftF f
logOutputF f@(PutStr s v) = do
  _ <- liftF $ LogStr s v
  liftF f
logOutputF other = liftF other

----

interpretFreeIOFToWrapperIO :: WrapperIO m => FreeIOF a -> m a
interpretFreeIOFToWrapperIO (GetChar cd) = do cd <$> wGetChar
interpretFreeIOFToWrapperIO (PutChar c v) = do
  wPutChar c
  pure v
interpretFreeIOFToWrapperIO (GetLine cd) = do cd <$> wGetLine
interpretFreeIOFToWrapperIO (PutStr s v) = do
  wPutStr s
  pure v
interpretFreeIOFToWrapperIO (PutStrLn s v) = do
  wPutStrLn s
  pure v
interpretFreeIOFToWrapperIO (Flush v) = do
  wFlush
  pure v
interpretFreeIOFToWrapperIO (GetInt cd) = do cd <$> wGetInt
interpretFreeIOFToWrapperIO (PutInt i v) = do
  wPutInt i
  pure v
interpretFreeIOFToWrapperIO (LogStr s v) = do
  wLogStr s
  pure v
interpretFreeIOFToWrapperIO (LogStrLn s v) = do
  wLogStrLn s
  pure v

instance WrapperIO FreeIO where
  wGetChar  = fGetChar
  wPutChar  = fPutChar
  wGetLine  = fGetLine
  wPutStr   = fPutStr
  wPutStrLn = fPutStrLn
  wFlush    = fFlush
  wGetInt   = fGetInt
  wPutInt   = fPutInt
  wLogStr   = fLogStr
  wLogStrLn = fLogStrLn

----

fGetChar :: FreeIO Char
fGetChar = liftF $ GetChar id

fPutChar :: Char -> FreeIO ()
fPutChar c = liftF $ PutChar c ()

fGetLine :: FreeIO String
fGetLine  = liftF $ GetLine id

fPutStr :: String -> FreeIO ()
fPutStr  s = liftF $ PutStr s ()

fPutStrLn :: String -> FreeIO ()
fPutStrLn s = liftF $ PutStrLn s ()

fFlush :: FreeIO ()
fFlush = liftF $ Flush ()

fGetInt :: FreeIO Int
fGetInt = liftF $ GetInt id

fPutInt :: Int -> FreeIO ()
fPutInt i = liftF $ PutInt i ()

fLogStr :: String -> FreeIO ()
fLogStr s = liftF $ LogStr s ()

fLogStrLn :: String -> FreeIO ()
fLogStrLn s = liftF $ LogStrLn s ()

----

data FreeIOF a
 = GetChar (Char -> a)
 | PutChar Char a
 | GetLine (String -> a)
 | PutStr String a
 | PutStrLn String a
 | Flush a
 | GetInt (Int -> a)
 | PutInt Int a
 | LogStr String a
 | LogStrLn String a
 deriving (Functor)
