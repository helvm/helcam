module HelVM.HelMA.Automaton.IO.MockIO (
  batchExecSafeMockIO,
  batchEvalSafeMockIO,
  flipExecSafeMockIO,
  flipEvalSafeMockIO,
  execSafeMockIO,
  evalSafeMockIO,

  batchExecMockIO,
  batchEvalMockIO,
  flipExecMockIO,
  flipEvalMockIO,
  execMockIO,
  evalMockIO,


  getOutput,
  getLogged,
  createMockIO,
  MockIO,
  MockIOData,
) where

import HelVM.HelMA.Automaton.API.IOTypes
import HelVM.HelMA.Automaton.IO.BusinessIO

import HelVM.Common.Containers.SplitAt
import HelVM.Common.Safe

import qualified Relude.Unsafe as Unsafe


batchExecSafeMockIO :: MockIO (Safe ()) -> Safe Output
batchExecSafeMockIO = flipExecSafeMockIO ""

batchEvalSafeMockIO :: MockIO (Safe ()) -> Safe Output
batchEvalSafeMockIO = flipEvalSafeMockIO ""

flipExecSafeMockIO :: Input -> MockIO (Safe ()) -> Safe Output
flipExecSafeMockIO = flip execSafeMockIO

flipEvalSafeMockIO :: Input -> MockIO (Safe ()) -> Safe Output
flipEvalSafeMockIO = flip evalSafeMockIO

--execSafeMockIO :: MockIO (Safe ()) -> Input -> Safe Output
--execSafeMockIO mockIO = Right . execMockIO mockIO
--
--evalSafeMockIO :: MockIO (Safe ()) -> Input -> Safe Output
--evalSafeMockIO mockIO = Right . evalMockIO mockIO

execSafeMockIO :: MockIO (Safe ()) -> Input -> Safe Output
execSafeMockIO mockIO input = Right $ execMockIO mockIO input

evalSafeMockIO :: MockIO (Safe ()) -> Input -> Safe Output
evalSafeMockIO mockIO input = Right $ evalMockIO mockIO input

--getSafeMockIO :: State MockIOData a -> Input -> MockIOData
--getSafeMockIO mockIO input = execState mockIO (createMockIO input)

----

batchExecMockIO :: MockIO () -> Output
batchExecMockIO = flipExecMockIO ""

batchEvalMockIO :: MockIO () -> Output
batchEvalMockIO = flipEvalMockIO ""

flipExecMockIO :: Input -> MockIO () -> Output
flipExecMockIO = flip execMockIO

flipEvalMockIO :: Input -> MockIO () -> Output
flipEvalMockIO = flip evalMockIO

execMockIO :: MockIO a -> Input -> Output
execMockIO mockIO  = getOutput . getMockIO mockIO

evalMockIO :: MockIO a -> Input -> Output
evalMockIO mockIO = getLogged . getMockIO mockIO

getMockIO :: MockIO a -> Input -> MockIOData
getMockIO mockIO = execState mockIO . createMockIO

----

instance BusinessIO (SafeExceptT MockIO) where
  wGetChar = safeExceptT   mockGetChar
  wGetLine = safeExceptT   mockGetLine
  wPutChar = safeExceptT . mockPutChar
  wPutInt  = safeExceptT . mockPutInt
  wPutStr  = safeExceptT . mockPutStr
  wLogStr  = safeExceptT . mockLogStr

instance BusinessIO MockIO where
  wGetChar = mockGetChar
  wGetLine = mockGetLine
  wPutChar = mockPutChar
  wPutInt  = mockPutInt
  wPutStr  = mockPutStr
  wLogStr  = mockLogStr

mockGetChar :: MockIO Char
mockGetChar = mockGetChar' =<< get where
  mockGetChar' :: MonadState MockIOData f => MockIOData -> f Char
  mockGetChar' mockIO = headOrError mockIO (input mockIO) <$ put mockIO { input = Unsafe.tail $ input mockIO }

mockGetLine :: MockIO Text
mockGetLine = mockGetLine' =<< get where
  mockGetLine' :: MonadState MockIOData f => MockIOData -> f Text
  mockGetLine' mockIO = toText line <$ put mockIO { input = input' } where (line , input') = splitStringByLn $ input mockIO

mockPutChar :: Char -> MockIO ()
mockPutChar char = mockPutChar' =<< get where
  mockPutChar' :: MonadState MockIOData f => MockIOData -> f ()
  mockPutChar' mockIO = put mockIO { output = char : output mockIO }

mockPutInt :: Int -> MockIO ()
mockPutInt value = mockPutInt' =<< get where
  mockPutInt' :: MonadState MockIOData f => MockIOData -> f ()
  mockPutInt' mockIO = put $ mockIO { output = chr  value : output mockIO }

mockPutStr :: Text -> MockIO ()
mockPutStr text = mockPutStr' =<< get where
  mockPutStr' :: MonadState MockIOData f => MockIOData -> f ()
  mockPutStr' mockIO = put $ mockIO { output = reverse (toString text) <> output mockIO }

mockLogStr :: Text -> MockIO ()
mockLogStr text = mockLogStr' =<< get where
  mockLogStr' :: MonadState MockIOData f => MockIOData -> f ()
  mockLogStr' mockIO = put $ mockIO { logged = reverse (toString text) <> logged mockIO }

----

type MockIO = State MockIOData

createMockIO :: Input -> MockIOData
createMockIO i = MockIOData (toString i) "" ""

getOutput :: MockIOData -> Output
getOutput mockIO = toText $ reverse $ output mockIO

getLogged :: MockIOData -> Output
getLogged mockIO = toText $ reverse $ logged mockIO

data MockIOData = MockIOData
  { input  :: String
  , output :: String
  , logged  :: String
--  , saved :: Text
  }
  deriving stock (Eq , Show , Read)

----

headOrError :: Show e => e -> [a] -> a
headOrError _ (x:_) =  x
headOrError e []    =  error $ show e

splitStringByLn :: String -> (String , String)
splitStringByLn = splitBy '\n'
