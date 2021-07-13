module HelVM.Common.SafeMonadT (
  liftMonad,
  hoistSafe,
  hoistMonad,
  hoistError,
  safeMonadToFail,
  unsafeRunExceptT,
  SafeMonadT,
) where

import HelVM.Common.Safe

import Control.Monad.Except hiding (ExceptT , runExceptT)

liftMonad :: MonadError e m => ExceptT e m a -> m a
liftMonad m = join $ liftEither <$> runExceptT m

--import Control.Monad.Except

hoistMonad :: Monad m => m a -> SafeMonadT m a
hoistMonad a = ExceptT $ safe <$> a

--except :: Monad m => Either e a -> ExceptT e m a
hoistSafe :: Monad m => Safe a -> SafeMonadT m a
hoistSafe = hoistEither
--hoistSafe = except

--hoistError :: Monad m => e -> ExceptT e m a
hoistError :: Monad m => Error -> SafeMonadT m a
hoistError = hoistSafe . safeError
--hoistError = throwE

----

safeMonadToFail :: MonadFail m => SafeMonadT m a -> m a
safeMonadToFail m = safeToFail =<< runExceptT m

unsafeRunExceptT :: Monad m => SafeMonadT m a -> m a
unsafeRunExceptT = fmap unsafe . runExceptT

----

type SafeMonadT m = ExceptT Error m
