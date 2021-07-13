module HelVM.Common.SafeExceptT (
  hoistSafe,
  hoistError,
  unsafeRunExceptT,
) where

import HelVM.Common.Safe

--except :: Monad m => Either e a -> ExceptT e m a
hoistSafe :: Monad m => Safe a -> SafeExceptT m a
hoistSafe = hoistEither
--hoistSafe = except

--hoistError :: Monad m => e -> ExceptT e m a
hoistError :: Monad m => Error -> SafeExceptT m a
hoistError = hoistSafe . safeError
--hoistError = throwE

----

unsafeRunExceptT :: Monad m => SafeExceptT m a -> m a
unsafeRunExceptT = fmap unsafe . runExceptT