{-# OPTIONS -pgmP cpp -optP-w #-}
{-# LANGUAGE CPP #-}

module MM.Data.Class.Empty (
   module MM.Data.Class.Empty
) where

-- {{{
import Prelude hiding(null)
import Data.Int
import Data.Word
import MM.Data.Map.Ord(Map)
import MM.Data.Set.Ord(Set)
import MM.Data.Map.Int(IntMap)
import MM.Data.Set.Int(IntSet)
import MM.Data.Map.Ix(IxMap)
import MM.Data.Set.Ix(IxSet)
import qualified MM.Data.Map.Ord as M
import qualified MM.Data.Set.Ord as S
import qualified MM.Data.Map.Int as IM
import qualified MM.Data.Set.Int as IS
import qualified MM.Data.Map.Ix as Ix
import qualified MM.Data.Set.Ix as IxS
-- }}}

-----------------------------------------------------------------------------

#define HASH #
#define __INLINE(x)
-- {-HASH INLINE x HASH-}

-----------------------------------------------------------------------------

#define FOREACH_UNIT(MACRO,...)\
  MACRO(Bot,bot,##__VA_ARGS__);\
  MACRO(Top,top,##__VA_ARGS__);\
  MACRO(Empty,empty,##__VA_ARGS__);\
  MACRO(Null,null,##__VA_ARGS__);\
  MACRO(Zero,zero,##__VA_ARGS__);\
  MACRO(One,one,##__VA_ARGS__);

-----------------------------------------------------------------------------

#define IS(THIS)   is##THIS

-----------------------------------------------------------------------------

#define CLASS_UNIT(CLASS,METH)\
  class CLASS a where\
    {METH :: a\
    ;IS(CLASS) :: a -> Bool};

-- | .
FOREACH_UNIT(CLASS_UNIT)

-----------------------------------------------------------------------------

#define INSTANCE_UNIT_Unit(CLASS,METH)\
  instance CLASS () where\
    {METH = ()\
    ;IS(CLASS) () = True};

FOREACH_UNIT(INSTANCE_UNIT_Unit)

-----------------------------------------------------------------------------

#define INSTANCE_UNIT_Tup2(CLASS,METH)\
  instance (CLASS a, CLASS b) => CLASS (a,b) where\
    {METH = (METH,METH)\
    ;IS(CLASS) (a,b) = IS(CLASS) a && IS(CLASS) b};

#define INSTANCE_UNIT_Tup3(CLASS,METH)\
  instance (CLASS a, CLASS b, CLASS c) => CLASS (a,b,c) where\
    {METH = (METH,METH,METH)\
    ;IS(CLASS) (a,b,c) = IS(CLASS) a && IS(CLASS) b && IS(CLASS) c};

FOREACH_UNIT(INSTANCE_UNIT_Tup2)
FOREACH_UNIT(INSTANCE_UNIT_Tup3)

-----------------------------------------------------------------------------

#define INSTANCE_ZERO_Maybe(CLASS,METH)\
  instance CLASS (Maybe a) where\
    {METH = Nothing\
    ;IS(CLASS) Nothing = True\
    ;IS(CLASS) _ = False};

#define INSTANCE_ONE_Maybe(CLASS,METH)\
  instance (CLASS a) => CLASS (Maybe a) where\
    {METH = Just METH\
    ;IS(CLASS) (Just a) = IS(CLASS) a\
    ;IS(CLASS) Nothing = False};

INSTANCE_ZERO_Maybe(Bot,bot)
INSTANCE_ZERO_Maybe(Empty,empty)
INSTANCE_ZERO_Maybe(Null,null)
INSTANCE_ZERO_Maybe(Zero,zero)
INSTANCE_ONE_Maybe(Top,top)
INSTANCE_ONE_Maybe(One,one)

-----------------------------------------------------------------------------

#define INSTANCE_Empty_Zero(A_t,...)\
  instance (__VA_ARGS__) => Empty (A_t) where\
    {empty = zero\
    ;isEmpty a = isZero a};

#define INSTANCE_Null_Zero(A_t,...)\
  instance (__VA_ARGS__) => Null (A_t) where\
    {null = zero\
    ;isNull a = isZero a};

-----------------------------------------------------------------------------

#define INSTANCE_Zero_Num(A_t)\
  instance Zero (A_t) where\
    {zero = 0\
    ;isZero 0 = True\
    ;isZero _ = False};

#define INSTANCE_One_Num(A_t)\
  instance One (A_t) where\
    {one = 1\
    ;isOne 1 = True\
    ;isOne _ = False};

#define INSTANCES_UNIT_Num(A_t)\
  INSTANCE_Zero_Num(A_t);\
  INSTANCE_One_Num(A_t);\
  INSTANCE_Empty_Zero(A_t);

INSTANCES_UNIT_Num(Int)
INSTANCES_UNIT_Num(Int8)
INSTANCES_UNIT_Num(Int16)
INSTANCES_UNIT_Num(Int32)
INSTANCES_UNIT_Num(Int64)
INSTANCES_UNIT_Num(Word)
INSTANCES_UNIT_Num(Word8)
INSTANCES_UNIT_Num(Word16)
INSTANCES_UNIT_Num(Word32)
INSTANCES_UNIT_Num(Word64)

-----------------------------------------------------------------------------

#define INSTANCE_UNIT_CONTAINER(CLASS,METH,PRE,A_t,...)\
  instance (__VA_ARGS__) => CLASS (A_t) where\
    {METH = PRE.empty;\
    ;IS(CLASS) a = PRE.null a};

INSTANCE_UNIT_CONTAINER(Zero,zero,IS,IntSet)
INSTANCE_UNIT_CONTAINER(Zero,zero,IxS,IxSet a)
INSTANCE_UNIT_CONTAINER(Zero,zero,S,Set a,Ord a)
INSTANCE_UNIT_CONTAINER(Zero,zero,IM,IntMap b)
INSTANCE_UNIT_CONTAINER(Zero,zero,Ix,IxMap a b)
INSTANCE_UNIT_CONTAINER(Zero,zero,M,Map a b,Ord a)

INSTANCE_Empty_Zero(IntSet)
INSTANCE_Empty_Zero(IxSet a)
INSTANCE_Empty_Zero(Set a,Ord a)
INSTANCE_Empty_Zero(IntMap b)
INSTANCE_Empty_Zero(IxMap a b)
INSTANCE_Empty_Zero(Map a b,Ord a)

INSTANCE_Null_Zero(IntSet)
INSTANCE_Null_Zero(IxSet a)
INSTANCE_Null_Zero(Set a,Ord a)
INSTANCE_Null_Zero(IntMap b)
INSTANCE_Null_Zero(IxMap a b)
INSTANCE_Null_Zero(Map a b,Ord a)

-----------------------------------------------------------------------------
