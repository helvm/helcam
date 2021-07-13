module HelVM.Common.SafeExceptT (
  unsafeRunExceptT,
) where

import HelVM.Common.Safe

unsafeRunExceptT :: Monad m => SafeExceptT m a -> m a
unsafeRunExceptT = fmap unsafe . runExceptT
