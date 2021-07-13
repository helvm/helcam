module HelVM.Common.Safe (
  exceptTToIO,
  liftError,
  liftSafe,
  userErrorText,

  safeFailToFail,
  safeToFail,
  safeLegacyToFail,

  safe,
  safeLegacyToSafe,
  safeToSafeLegacy,

  maybeToSafeOrErrorTupleList,
  maybeToSafeOrErrorTuple,
  maybeToSafeOrError,

  safeErrorTupleList,
  safeErrorTuple,
  safeError,

  appendErrorTupleList,
  appendErrorTuple,
  appendError,

  tupleListToError,
  tupleToError,

  unsafe,

  MonadSafeError,
  SafeExceptT,
  SafeFail,
  Safe,
  Error,
) where

import Control.Exception.Base
import Control.Monad.Except hiding (runExceptT)
import System.IO.Error

exceptTToIO :: SafeExceptT IO a -> IO a
exceptTToIO a = liftEither =<< runExceptT (withExceptT userErrorText a)

userErrorText :: Text -> IOException
userErrorText = userError . toString

liftError :: MonadSafeError m => Text -> m a
liftError = throwError

liftSafe :: MonadSafeError m => Safe a -> m a
liftSafe = liftEither

safeFailToFail ::  MonadFail m => SafeFail m a -> m a
safeFailToFail m = safeToFail =<< m

safeToFail ::  MonadFail m => Safe a -> m a
safeToFail = safeLegacyToFail . safeToSafeLegacy

safeLegacyToFail :: MonadFail m => SafeLegacy a -> m a
safeLegacyToFail (Right a) = pure a
safeLegacyToFail (Left a)  = fail a

-- Create Safe

safe :: a -> Safe a
safe = pure

safeLegacyToSafe :: SafeLegacy a -> Safe a
safeLegacyToSafe = first toText

safeToSafeLegacy :: Safe a -> SafeLegacy a
safeToSafeLegacy = first toString

---- Create from Maybe

maybeToSafeOrErrorTupleList :: [ErrorTuple] -> Maybe a -> Safe a
maybeToSafeOrErrorTupleList = maybeToSafeOrError . tupleListToError

maybeToSafeOrErrorTuple :: ErrorTuple -> Maybe a -> Safe a
maybeToSafeOrErrorTuple = maybeToSafeOrError . tupleToError

maybeToSafeOrError :: Error -> Maybe a -> Safe a
maybeToSafeOrError = maybeToRight

---- Create Error

safeErrorTupleList :: [ErrorTuple] -> Safe a
safeErrorTupleList = safeError . tupleListToError

safeErrorTuple :: ErrorTuple -> Safe a
safeErrorTuple = safeError . tupleToError

safeError :: Error -> Safe a
safeError = Left

---- Append Error

appendErrorTupleList :: [ErrorTuple] -> Safe a -> Safe a
appendErrorTupleList = appendError . tupleListToError

appendErrorTuple :: ErrorTuple -> Safe a -> Safe a
appendErrorTuple = appendError . tupleToError

appendError :: Error -> Safe a -> Safe a
appendError message = first (<> message)

----

tupleListToError :: [ErrorTuple] -> Error
tupleListToError xs = mconcat $ tupleToError <$> xs

tupleToError :: ErrorTuple -> Error
tupleToError (prefix , showed) = " [" <> format prefix <> showed <> "]" where
  format "" = ""
  format _  = prefix <> " "

----

unsafe :: Safe a -> a
unsafe (Right a) = a
unsafe (Left a) = error a

----

type MonadSafeError m = MonadError Error m

type SafeExceptT m a = ExceptT Error m a

type SafeFail m a = m (Safe a)

type SafeLegacy a = Either String a

type Safe a = Either Error a

type ErrorTuple = (Error , Error)

type Error = Text
