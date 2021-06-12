module HelVM.HelMA.Common.Collections.Pop where

import HelVM.HelMA.Common.Util

import Data.Sequence (Seq(..))

discard :: Pop1 e c => c -> Result c
discard s = discard' <$> pop1 s where
  discard' (_ , s') = s'

class Pop1 e c | c -> e where
  pop1 :: c -> Result (e , c)

instance Show e => Pop1 e [e] where
  pop1 (e  : c) = Right (e , c)
  pop1       c  = Left $ "Empty " <> show c

instance Show e => Pop1 e (Seq e) where
  pop1 (e :<|  c) = Right (e , c)
  pop1         c  = Left $ "Empty " <> show c

class Pop2 e c | c -> e where
  pop2 :: c -> Result (e , e , c)

instance Show e => Pop2 e [e] where
  pop2 (e : e' : c) = Right (e , e', c)
  pop2           c  = Left $ "Empty " <> show c

instance Show e => Pop2 e (Seq e) where
  pop2 (e :<| e' :<| c) = Right (e , e', c)
  pop2               c  = Left $ "Empty  " <> show c

