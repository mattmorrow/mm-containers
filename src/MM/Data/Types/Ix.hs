{-# LANGUAGE CPP #-}
#include "mm_containers_compat.h"
#if MIN_VERSION_base(4,7,0)
#define __D_BITS Bits,FiniteBits
#else
#define __D_BITS Bits
#endif

module MM.Data.Types.Ix (
   Ix(..), unIx
  ,castIx, castIxArg, castIxArg2, castIntToIxArg
) where

import Data.Bits
import Unsafe.Coerce(unsafeCoerce)
import Data.Binary

-- | A newtype of @Int@ with a phantom type variable.
newtype Ix a = Ix Int deriving
  (Eq,Ord,Bounded,Enum,Binary,Num,Real,Integral,__D_BITS)

unIx :: Ix a -> Int
unIx (Ix i) = i

castIx :: Ix a -> Ix b
castIx (Ix i) = Ix i

castIxArg :: f (Ix a) -> f (Ix b)
castIxArg = unsafeCoerce

castIxArg2 :: f (Ix a) b -> f (Ix c) b
castIxArg2 = unsafeCoerce

castIntToIxArg :: f Int -> f (Ix a)
castIntToIxArg = unsafeCoerce

instance Show (Ix a) where
  showsPrec _ (Ix x) = shows x

instance Read (Ix a) where
  readsPrec p s = unsafeCoerce (readsPrec p s :: [(Int, String)])

-----------------------------------------------------------------------------
