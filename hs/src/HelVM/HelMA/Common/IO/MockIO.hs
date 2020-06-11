module HelVM.HelMA.Common.IO.MockIO (
  batchExecMockIO,
  flipExecMockIO,
  execMockIO,
batchEvalMockIO,
  flipEvalMockIO,
  evalMockIO,
  getLogged,
  MockIO
) where

import HelVM.HelMA.Common.IO.WrapperIO
import HelVM.HelMA.Common.Util

import qualified Relude.Unsafe as Unsafe

batchExecMockIO :: MockIO () -> Output
batchExecMockIO = flipExecMockIO []

flipExecMockIO :: Input -> MockIO () -> Output
flipExecMockIO = flip execMockIO

execMockIO :: MockIO () -> Interact
execMockIO mockIO  = getOutput . execState mockIO . createMockIO

batchEvalMockIO :: MockIO () -> Output
batchEvalMockIO = flipEvalMockIO []

flipEvalMockIO :: Input -> MockIO () -> Output
flipEvalMockIO = flip evalMockIO

evalMockIO :: MockIO () -> Interact
evalMockIO mockIO = getLogged . execState mockIO . createMockIO

----

instance WrapperIO MockIO where
  wGetChar = mockGetChar
  wGetInt  = mockGetInt
  wGetLine = mockGetLine
  wPutChar = mockPutChar
  wPutInt  = mockPutInt
  wPutStr  = mockPutStr
  wLogStr  = mockLogStr

mockGetChar :: MockIO Char
mockGetChar = do
  mockIO <- get
  let char = headOrError mockIO $ input mockIO
  put mockIO { input = Unsafe.tail $ input mockIO }
  pure char

mockGetInt :: MockIO Int
mockGetInt = do ord <$> mockGetChar

mockGetLine :: MockIO String
mockGetLine = do
  mockIO <- get
  let pair = splitStringByEndLine (input mockIO)
  put mockIO { input = snd pair }
  pure $ fst pair

mockPutChar :: Char -> MockIO ()
mockPutChar char = do
  mockIO <- get
  put mockIO { output = char : output mockIO }

mockPutInt :: Int -> MockIO ()
mockPutInt value = do
  mockIO <- get
  put $ mockIO { output = chr  value : output mockIO }


mockPutStr :: String -> MockIO ()
mockPutStr string = do
  mockIO <- get
  put $ mockIO { output = reverse string <> output mockIO }


mockLogStr :: String -> MockIO ()
mockLogStr string = do
  mockIO <- get
  put $ mockIO { logged = reverse string <> logged mockIO }

----

type MockIO = State MockIOData

createMockIO :: String -> MockIOData
createMockIO i = MockIOData i [] []

getOutput :: MockIOData -> String
getOutput (MockIOData _ o _) = reverse o

getLogged :: MockIOData -> String
getLogged (MockIOData _ _ e) = reverse e

data MockIOData = MockIOData
  { input  :: String
  , output :: String
  , logged  :: String
  }
  deriving (Eq, Show, Read)

----

headOrError :: Show e => e -> [a] -> a
headOrError _ (x:_) =  x
headOrError e []    =  error $ show e
