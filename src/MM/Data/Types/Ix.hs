{-# LANGUAGE CPP #-}
#include "mm_containers_compat.h"
#define __D deriving(Eq,Ord,Read,Show)

module MM.Data.Types.Ix (
   Ix(..), unIx
  ,castIx, castIxArg, castIxArg2, castIntToIxArg
) where

import Data.Bits
import Unsafe.Coerce(unsafeCoerce)
import Data.Binary

-- | A newtype of @Int@ with a phantom type variable.
newtype Ix a = Ix Int __D

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

instance Binary (Ix a) where
  put = put . unIx
  get = do
    a <- get
    return (Ix a)

instance Enum (Ix a) where
  toEnum = Ix
  fromEnum = unIx

instance Bounded (Ix a) where
  minBound = Ix minBound
  maxBound = Ix maxBound

--instance Bin.Binary (Ix a) where
--  put = Bin.put . unIx
--  get = do
--    a <- Bin.get
--    return (Ix a)

instance Num (Ix a) where
  -- (Bangs to be sure that
  -- ghc doesn't get blinded
  -- by its coercions and fail
  -- to optimize something.)
  Ix a + Ix b | !c <- a + b = Ix c
  Ix a * Ix b | !c <- a * b = Ix c
  Ix a - Ix b | !c <- a - b = Ix c
  abs (Ix a) | !c <- abs a = Ix c
  negate (Ix a) | !c <- negate a = Ix c
  signum (Ix a) | !c <- signum a = Ix c
  fromInteger a | !c <- fromIntegral a = Ix c

instance Real (Ix a) where
  toRational (Ix a) = toRational a

instance Integral (Ix a) where
  -- (Bangs to be sure that
  -- ghc doesn't get blinded
  -- by its coercions and fail
  -- to optimize something.)
  quot (Ix a) (Ix b) | !c <- quot a b = Ix c
  rem  (Ix a) (Ix b) | !c <- rem a b = Ix c
  div  (Ix a) (Ix b) | !c <- rem a b = Ix c
  mod  (Ix a) (Ix b) | !c <- rem a b = Ix c
  quotRem (Ix a) (Ix b)
    | (!a,!b) <- quotRem a b
    = (Ix a, Ix b)
  divMod (Ix a) (Ix b)
    | (!a,!b) <- divMod a b
    = (Ix a, Ix b)
  toInteger (Ix a) = fromIntegral a

instance Bits (Ix a) where
  -- (Bangs to be sure that
  -- ghc doesn't get blinded
  -- by its coercions and fail
  -- to optimize something.)
  Ix a .&. Ix b | !c <- a .&. b = Ix c
  Ix a .|. Ix b | !c <- a .|. b = Ix c
  xor (Ix a) (Ix b) | !c <- xor a b = Ix c
  complement (Ix a) | !c <- complement a = Ix c
  shift (Ix a) m | !c <- shift a m = Ix c
  rotate (Ix a) m | !c <- rotate a m = Ix c
  setBit (Ix a) m | !c <- setBit a m = Ix c
  clearBit (Ix a) m | !c <- clearBit a m = Ix c
  complementBit (Ix a) m | !c <- complementBit a m = Ix c
  shiftL (Ix a) m | !c <- shiftL a m = Ix c
  shiftR (Ix a) m | !c <- shiftR a m = Ix c
  rotateL (Ix a) m | !c <- rotateL a m = Ix c
  rotateR (Ix a) m | !c <- rotateR a m = Ix c
  bit m | !c <- bit m = Ix c
  testBit (Ix a) m = testBit a m
  isSigned (Ix a) = isSigned a
  bitSize (Ix a) = bitSize a
#if MIN_VERSION_base(4,5,0)
  unsafeShiftL (Ix a) m | !c <- unsafeShiftL a m = Ix c
  unsafeShiftR (Ix a) m | !c <- unsafeShiftR a m = Ix c
  popCount (Ix a) = popCount a
#endif
#if MIN_VERSION_base(4,7,0)
  bitSizeMaybe (Ix a) = bitSizeMaybe a
  zeroBits = Ix zeroBits
#endif

#if MIN_VERSION_base(4,7,0)
instance FiniteBits (Ix a) where
  finiteBitSize (Ix a) = finiteBitSize a
  countLeadingZeros (Ix a) = countLeadingZeros a
  countTrailingZeros (Ix a) = countTrailingZeros a
#endif

-----------------------------------------------------------------------------
