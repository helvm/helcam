module HelVM.HelMA.Common.Collections.Lookup where

import HelVM.HelMA.Common.Util

import qualified Data.IntMap   as IntMap
import qualified Data.Sequence as Seq

indexEither :: (Show c , Lookup e c) => c -> Int -> Result e
indexEither c i = maybeToRight ("Empty " <> show c <> " index " <> show i) $ indexMaybe c i

indexMaybe :: Lookup e c => c -> Int -> Maybe e
indexMaybe = flip lookup

class Lookup e c | c -> e where
  lookup:: Int -> c -> Maybe e

instance Lookup e [e] where
  lookup = flip (!!?)

instance Lookup e (Seq e) where
  lookup = Seq.lookup

instance Lookup e (IntMap e) where
  lookup = IntMap.lookup
