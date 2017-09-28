{-# OPTIONS -pgmP cpp -optP-w #-}
{-# LANGUAGE CPP #-}

module MM.Data.Class.O (
   module MM.Data.Class.O
  ,module MM.Data.Class.Empty
  ,module MM.Data.Class.Lattice
) where

import Data.Int
import Data.Word
import Data.Bits
import MM.Data.Map.Ord(Map)
import MM.Data.Set.Ord(Set)
import MM.Data.Map.Int(IntMap)
import MM.Data.Set.Int(IntSet)
import MM.Data.Ix.Map(IxMap)
import MM.Data.Ix.Set(IxSet)
import qualified MM.Data.Map.Ord as M
import qualified MM.Data.Set.Ord as S
import qualified MM.Data.Map.Int as IM
import qualified MM.Data.Set.Int as IS
import qualified MM.Data.Ix.Map as Ix
import qualified MM.Data.Ix.Set as IxS
import Data.Monoid(Monoid(..))
import Data.List hiding((\\))
import MM.Data.Class.Lattice
import MM.Data.Class.Empty

-----------------------------------------------------------------------------

#define HASH #
#define __INLINE(x)   {-HASH INLINE x HASH-}

-----------------------------------------------------------------------------

-- | .
class (Empty a, Meet a, Join a, Diff a) => O a

-----------------------------------------------------------------------------

instance O ()
instance (O a, O b) => O (a,b)

-----------------------------------------------------------------------------

#define INSTANCES_O_NumBits(A_t)\
  instance O (A_t);\
  instance Meet (A_t) where\
    {(/\) a b = a .&. b\
    ;__INLINE((/\))};\
  instance Join (A_t) where\
    {(\/) a b = a .|. b\
    ;__INLINE((\/))};\
  instance Diff (A_t) where\
    {(\\) a b = a /\ complement b\
    ;__INLINE((\\))};

INSTANCES_O_NumBits(Int)
INSTANCES_O_NumBits(Int8)
INSTANCES_O_NumBits(Int16)
INSTANCES_O_NumBits(Int32)
INSTANCES_O_NumBits(Int64)
INSTANCES_O_NumBits(Word)
INSTANCES_O_NumBits(Word8)
INSTANCES_O_NumBits(Word16)
INSTANCES_O_NumBits(Word32)
INSTANCES_O_NumBits(Word64)

-----------------------------------------------------------------------------

#define INSTANCES_O_SET(PRE,A_t,...)\
  instance (__VA_ARGS__) => Meet (A_t) where\
    {(/\) a b = PRE.intersection a b\
    ;__INLINE((/\))};\
  instance (__VA_ARGS__) => Join (A_t) where\
    {(\/) a b = PRE.union a b\
    ;__INLINE((\/))};\
  instance (__VA_ARGS__) => Diff (A_t) where\
    {(\\) a b = PRE.difference a b\
    ;__INLINE((\\))};\
  instance (__VA_ARGS__) => O (A_t);\

INSTANCES_O_SET(IS,IntSet)
INSTANCES_O_SET(IxS,IxSet a)
INSTANCES_O_SET(S,Set a,Ord a)

-----------------------------------------------------------------------------

#define INSTANCES_O_MAP(PRE,A_t,...)\
  instance (__VA_ARGS__) => Meet (A_t) where\
    {(/\) a b = PRE.intersectionWith (/\) a b\
    ;__INLINE((/\))};\
  instance (__VA_ARGS__) => Join (A_t) where\
    {(\/) a b = PRE.unionWith (\/) a b\
    ;__INLINE((\/))};\
  instance (__VA_ARGS__) => Diff (A_t) where\
    {(\\) a b = PRE.differenceWith go a b\
      where {go x y\
              | !z <- x\\y\
              , False <- isEmpty z = Just z\
              | otherwise = Nothing};\
    ;__INLINE((\\))};\
  instance (__VA_ARGS__) => O (A_t);\

INSTANCES_O_MAP(IM,IntMap b,O b)
INSTANCES_O_MAP(Ix,IxMap a b,O b)
INSTANCES_O_MAP(M,Map a b,Ord a,O b)

-----------------------------------------------------------------------------

#if 0
class Monoid a => O a where
  isEmpty :: a -> Bool
  (\/) :: a -> a -> a
  (/\) :: a -> a -> a
  (\\) :: a -> a -> a
instance O () where
  isEmpty () = True
  (\/) () () = ()
  (/\) () () = ()
  (\\) () () = ()
-- NOTE: The orphan @Monoid@ instances.
#define HASH #
#define __INLINE(x)   {-HASH INLINE x HASH-}
#define INSTANCE_O_NumBits(T_t)\
  instance Monoid (T_t) where{\
    __INLINE(mempty);\
    __INLINE(mappend);\
    mempty = 0;\
    mappend = (\/)};\
  instance O (T_t) where{\
    __INLINE(isEmpty);\
    __INLINE((/\));\
    __INLINE((\/));\
    __INLINE((\\));\
    isEmpty a = a==0;\
    (/\) a b = a .&. b;\
    (\/) a b = a .|. b;\
    (\\) a b = a /\ complement b};
INSTANCE_O_NumBits(Int)
INSTANCE_O_NumBits(Int8)
INSTANCE_O_NumBits(Int16)
INSTANCE_O_NumBits(Int32)
INSTANCE_O_NumBits(Int64)
INSTANCE_O_NumBits(Word)
INSTANCE_O_NumBits(Word8)
INSTANCE_O_NumBits(Word16)
INSTANCE_O_NumBits(Word32)
INSTANCE_O_NumBits(Word64)
#define METHODS_O_SET(PRE)\
  isEmpty = PRE.null;\
  (\/) = PRE.union;\
  (/\) = PRE.intersection;\
  (\\) = PRE.difference;
instance O (IntSet) where {METHODS_O_SET(IS)}
instance O (IxSet a) where {METHODS_O_SET(IxS)}
instance (Ord a) => O (Set a) where {METHODS_O_SET(S)}
#define METHODS_O_MAP(PRE)\
  isEmpty = PRE.null;\
  (\/) = PRE.unionWith (\/);\
  (/\) = PRE.intersectionWith (/\);\
  (\\) = PRE.differenceWith\
          (\a b-> let {!c = a\\b}\
                  in case isEmpty c of{\
                      True-> Nothing;\
                      False-> Just c});
instance (O a) => O (IntMap a) where {METHODS_O_MAP(IM)}
instance (O b) => O (IxMap a b) where {METHODS_O_MAP(Ix)}
instance (Ord a, O b) => O (Map a b) where {METHODS_O_MAP(M)}
#endif

-----------------------------------------------------------------------------
