module HelVM.Common.SafeExceptT (
  hoistSafe,
  hoistMonad,
  hoistError,
  safeMonadToFail,
  unsafeRunExceptT,
) where

import HelVM.Common.Safe

hoistMonad :: Monad m => m a -> SafeExceptT m a
hoistMonad a = ExceptT $ safe <$> a

--except :: Monad m => Either e a -> ExceptT e m a
hoistSafe :: Monad m => Safe a -> SafeExceptT m a
hoistSafe = hoistEither
--hoistSafe = except

--hoistError :: Monad m => e -> ExceptT e m a
hoistError :: Monad m => Error -> SafeExceptT m a
hoistError = hoistSafe . safeError
--hoistError = throwE

----

safeMonadToFail :: MonadFail m => SafeExceptT m a -> m a
safeMonadToFail m = safeToFail =<< runExceptT m

unsafeRunExceptT :: Monad m => SafeExceptT m a -> m a
unsafeRunExceptT = fmap unsafe . runExceptT
