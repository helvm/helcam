module HelVM.HelMA.Automaton.IO.MockIO (
  batchExecSafeMockIO,
  flipExecSafeMockIO,
  execSafeMockIO,
  batchEvalSafeMockIO,
  flipEvalSafeMockIO,
  evalSafeMockIO,
--  createSafeMockIO,
--  SafeMockIO,

  batchExecMockIO,
  flipExecMockIO,
  execMockIO,
  batchEvalMockIO,
  flipEvalMockIO,
  evalMockIO,
  createMockIO,
  MockIO,

  getOutput,
  getLogged,
  MockIOData,
) where

import HelVM.HelMA.Automaton.API.IOTypes
import HelVM.HelMA.Automaton.IO.BusinessIO

import HelVM.Common.Containers.SplitAt
import HelVM.Common.Safe

import qualified Relude.Unsafe as Unsafe


batchExecSafeMockIO :: MockIO (Safe ()) -> Safe Output
batchExecSafeMockIO = flipExecSafeMockIO ""

flipExecSafeMockIO :: Input -> MockIO (Safe ()) -> Safe Output
flipExecSafeMockIO = flip execSafeMockIO

execSafeMockIO :: MockIO (Safe ()) -> Input -> Safe Output
execSafeMockIO safeMockIO input = Right $ getOutput $ execState safeMockIO (createMockIO input)

batchEvalSafeMockIO :: MockIO (Safe ()) -> Safe Output
batchEvalSafeMockIO = flipEvalSafeMockIO ""

flipEvalSafeMockIO :: Input -> MockIO (Safe ()) -> Safe Output
flipEvalSafeMockIO = flip evalSafeMockIO

evalSafeMockIO :: MockIO (Safe ()) -> Input -> Safe Output
evalSafeMockIO safeMockIO input = Right $ getLogged $ execState safeMockIO (createMockIO input)

------
--
--instance BusinessIO SafeMockIO where
--  wGetChar = safeMockGetChar
--  wGetLine = safeMockGetLine
--  wPutChar = safeMockPutChar
--  wPutInt  = safeMockPutInt
--  wPutStr  = safeMockPutStr
--  wLogStr  = safeMockLogStr
--
--instance BusinessIO (SafeExceptT SafeMockIO) where
--  wGetChar = safeExceptT   safeMockGetChar
--  wGetLine = safeExceptT   safeMockGetLine
--  wPutChar = safeExceptT . safeMockPutChar
--  wPutInt  = safeExceptT . safeMockPutInt
--  wPutStr  = safeExceptT . safeMockPutStr
--  wLogStr  = safeExceptT . safeMockLogStr
--
--safeMockGetChar :: SafeMockIO Char
----safeMockGetChar = error ""
--safeMockGetChar = safeMockGetChar' <$> get --where
----  safeMockGetChar' = fmap safeMockGetChar'' where
----    safeMockGetChar'' mockIO = headOrError mockIO (input mockIO) <$ put mockIO { input = Unsafe.tail $ input mockIO }
--
----safeMockGetChar' :: SafeMockIOData -> SafeMockIOData
----safeMockGetChar' = id
--
--safeMockGetChar' :: MonadState SafeMockIOData f => SafeMockIOData -> Safe (f Char)
--safeMockGetChar' safeMockIO = safeMockGetChar'' <$> safeMockIO
--
--safeMockGetChar'' :: MonadState SafeMockIOData f => MockIOData -> f (Char)
--safeMockGetChar'' mockIO = headOrError mockIO (input mockIO) <$ put (Right mockIO { input = Unsafe.tail $ input mockIO })
--
--safeMockGetLine :: SafeMockIO Text
--safeMockGetLine = error ""
--
--safeMockPutChar :: Char -> SafeMockIO ()
--safeMockPutChar _ = error ""
--
--safeMockPutInt :: Int -> SafeMockIO ()
--safeMockPutInt _ = error ""
--
--
--safeMockPutStr :: Text -> SafeMockIO ()
--safeMockPutStr _ = error ""
--
--
--safeMockLogStr :: Text -> SafeMockIO ()
--safeMockLogStr _ = error ""
--
------
--
--type SafeMockIO = State SafeMockIOData
--
--createSafeMockIO :: Input -> Safe MockIOData
--createSafeMockIO = pure . createMockIO
--
--type SafeMockIOData = Safe MockIOData
--
----

batchExecMockIO :: MockIO () -> Output
batchExecMockIO = flipExecMockIO ""

flipExecMockIO :: Input -> MockIO () -> Output
flipExecMockIO = flip execMockIO

execMockIO :: MockIO () -> Interact
execMockIO mockIO  = getOutput . execState mockIO . createMockIO

batchEvalMockIO :: MockIO () -> Output
batchEvalMockIO = flipEvalMockIO ""

flipEvalMockIO :: Input -> MockIO () -> Output
flipEvalMockIO = flip evalMockIO

evalMockIO :: MockIO () -> Interact
evalMockIO mockIO = getLogged . execState mockIO . createMockIO

----

instance BusinessIO MockIO where
  wGetChar = mockGetChar
  wGetLine = mockGetLine
  wPutChar = mockPutChar
  wPutInt  = mockPutInt
  wPutStr  = mockPutStr
  wLogStr  = mockLogStr

instance BusinessIO (SafeExceptT MockIO) where
  wGetChar = safeExceptT   mockGetChar
  wGetLine = safeExceptT   mockGetLine
  wPutChar = safeExceptT . mockPutChar
  wPutInt  = safeExceptT . mockPutInt
  wPutStr  = safeExceptT . mockPutStr
  wLogStr  = safeExceptT . mockLogStr

mockGetChar :: MockIO Char
mockGetChar = mockGetChar' =<< get

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
