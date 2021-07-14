module HelVM.Common.Safe (
--  safeExceptTIOToIO,
  safeIOToIO,
  safeToIO,
  exceptTToIO,
  userErrorText,

  liftExceptT,
  liftSafe,
  liftLegacySafe,

  liftMaybeOrErrorTupleList,
  liftMaybeOrErrorTuple,
  liftMaybeOrError,

  liftErrorTupleList,
  liftErrorTuple,
  liftError,

  appendErrorTupleList,
  appendErrorTuple,
  appendError,

  safeExceptT,
  legacySafeToSafe,
  safeToLegacySafe,

  tupleListToError,
  tupleToError,

  unsafe,

  MonadSafeError,
  SafeExceptT,
  Safe,
  Error,
) where

import Control.Exception.Base
import Control.Monad.Except hiding (ExceptT , runExceptT)

import System.IO.Error

--safeExceptTIOToIO :: IO (SafeExceptT a) -> IO a
--safeExceptTIOToIO a = exceptTToIO =<< a

safeIOToIO :: IO (Safe a) -> IO a
safeIOToIO a = safeToIO =<< a

safeToIO :: Safe a -> IO a
safeToIO = exceptTToIO . liftSafe

exceptTToIO :: SafeExceptT IO a -> IO a
exceptTToIO = liftExceptT . withExceptT userErrorText

userErrorText :: Text -> IOException
userErrorText = userError . toString

-- Lift

liftExceptT :: MonadError e m => ExceptT e m a -> m a
liftExceptT m = liftEither =<< runExceptT m

liftSafe :: MonadSafeError m => Safe a -> m a
liftSafe = liftEither

liftLegacySafe :: MonadSafeError m => LegacySafe a -> m a
liftLegacySafe = liftSafe . legacySafeToSafe

-- Lift from Maybe

liftMaybeOrErrorTupleList :: MonadSafeError m => [ErrorTuple] -> Maybe a -> m a
liftMaybeOrErrorTupleList = liftMaybeOrError . tupleListToError

liftMaybeOrErrorTuple :: MonadSafeError m => ErrorTuple -> Maybe a -> m a
liftMaybeOrErrorTuple = liftMaybeOrError . tupleToError

liftMaybeOrError :: MonadSafeError m => Error -> Maybe a -> m a
liftMaybeOrError e = liftSafe . maybeToRight e

-- Lift from Error

liftErrorTupleList :: MonadSafeError m => [ErrorTuple] -> m a
liftErrorTupleList = liftError . tupleListToError

liftErrorTuple :: MonadSafeError m => ErrorTuple -> m a
liftErrorTuple = liftError . tupleToError

liftError :: MonadSafeError m => Error -> m a
liftError = throwError

-- Append Error

appendErrorTupleList :: MonadSafeError m => [ErrorTuple] -> m a -> m a
appendErrorTupleList = appendError . tupleListToError

appendErrorTuple :: MonadSafeError m => ErrorTuple -> m a -> m a
appendErrorTuple = appendError . tupleToError

appendError :: MonadSafeError m => Error -> m a -> m a
appendError message a = catchError a appendAndThrow where appendAndThrow e = throwError (e <> message)

-- Create Safe

safeExceptT :: Monad m => m a -> SafeExceptT m a
safeExceptT a = ExceptT $ pure <$> a

legacySafeToSafe :: LegacySafe a -> Safe a
legacySafeToSafe = first toText

safeToLegacySafe :: Safe a -> LegacySafe a
safeToLegacySafe = first toString

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

type SafeExceptT m = ExceptT Error m

type LegacySafe = Either String

type Safe = Either Error

type ErrorTuple = (Error , Error)

type Error = Text
