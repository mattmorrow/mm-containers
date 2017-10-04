{-# OPTIONS -pgmP cpp -optP-w #-}
{-# LANGUAGE CPP #-}
#define HASH #
#define __INLINE(x) {-HASH INLINE x HASH-}

module MM.Data.Class.Base (
   module MM.Data.Class.Base
) where

import Prelude hiding(null)
import Data.Int
import Data.Word
import Data.Bits
import MM.Data.Types.Ix(Ix)
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
import Data.Monoid(Monoid(..))
import Data.List hiding((\\),null)

-----------------------------------------------------------------------------

class ToList o a | o -> a where
  toList :: o -> [a]
class FromList o a | o -> a where
  fromList :: [a] -> o
class (FromList o a,ToList o b) => List o a b | o -> a b
#define INSTANCE_LIST(A_t,B_t,C_t,CXT,FROMLIST,TOLIST)\
  instance (CXT) => List (A_t) (B_t) (C_t);\
  instance (CXT) => FromList (A_t) (B_t) where {fromList xs = FROMLIST xs};\
  instance (CXT) => ToList (A_t) (C_t) where {toList xs = TOLIST xs};
INSTANCE_LIST([a],a,a,,id,id)
INSTANCE_LIST(IntSet,Int,Int,,IS.fromList,IS.toList)
INSTANCE_LIST(IntMap a,(Int,a),(Int,a),,IM.fromList,IM.toList)
INSTANCE_LIST(Set a,a,a,Ord a,S.fromList,S.toList)
INSTANCE_LIST(Map a b,(a,b),(a,b),Ord a,M.fromList,M.toList)
INSTANCE_LIST(IxMap a b,(Ix a,b),(Ix a,b),,Ix.fromList,Ix.toList)
INSTANCE_LIST(IxSet a,Ix a,Ix a,,IxS.fromList,IxS.toList)
#undef INSTANCE_LIST

-----------------------------------------------------------------------------

#define FOREACH_UNIT(MACRO,...)\
  MACRO(Bot,bot,##__VA_ARGS__);\
  MACRO(Top,top,##__VA_ARGS__);\
  MACRO(Empty,empty,##__VA_ARGS__);\
  MACRO(Null,null,##__VA_ARGS__);\
  MACRO(Zero,zero,##__VA_ARGS__);\
  MACRO(One,one,##__VA_ARGS__);
#define IS(THIS)   is##THIS
#define CLASS_UNIT(CLASS,METH)\
  class CLASS a where\
    {METH :: a\
    ;IS(CLASS) :: a -> Bool};
FOREACH_UNIT(CLASS_UNIT)
#define INSTANCE_UNIT_Unit(CLASS,METH)\
  instance CLASS () where\
    {METH = ()\
    ;IS(CLASS) () = True};
FOREACH_UNIT(INSTANCE_UNIT_Unit)
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
#define INSTANCE_Empty_Zero(A_t,...)\
  instance (__VA_ARGS__) => Empty (A_t) where\
    {empty = zero\
    ;isEmpty a = isZero a};
#define INSTANCE_Null_Zero(A_t,...)\
  instance (__VA_ARGS__) => Null (A_t) where\
    {null = zero\
    ;isNull a = isZero a};
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
instance Empty Bool where {empty = False; isEmpty = not}
instance Bot Bool where {bot = False; isBot = not}
instance Zero Bool where {zero = False; isZero = not}
instance Top Bool where {top = True; isTop = id}
instance One Bool where {one = True; isOne = id}

-----------------------------------------------------------------------------

#if 0
instance (Lat a, Lat b) => Lat (a,b)
instance (MeetLat a, MeetLat b) => MeetLat (a,b)
instance (JoinLat a, JoinLat b) => JoinLat (a,b)
class (Bot a, Meet a) => MeetLat a
class (Top a, Join a) => JoinLat a
class (Bot a, MeetM m a) => MeetLatM m a
class (Top a, JoinM m a) => JoinLatM m a
class (MeetLat a, JoinLat a) => Lat a
class (MeetLatM m a, JoinLatM m a) => LatM m a
instance (Bot a, Meet a) => MeetLat a
instance (Top a, Join a) => JoinLat a
instance (Bot a, MeetM m a) => MeetLatM m a
instance (Top a, JoinM m a) => JoinLatM m a
instance (MeetLat a, JoinLat a) => Lat a
instance (MeetLatM m a, JoinLatM m a) => LatM m a
instance Lat ()
instance MeetLat ()
instance JoinLat ()
#endif

class Diff a where (\\) :: a -> a -> a
class Meet a where (/\) :: a -> a -> a
class Join a where (\/) :: a -> a -> a
class MeetM m a where meetM :: a -> a -> m a
class JoinM m a where joinM :: a -> a -> m a

instance Meet () where (/\) () () = ()
instance Join () where (\/) () () = ()
instance Diff () where (\\) () () = ()
instance (Meet a, Meet b) => Meet (a,b) where
  (/\) (a1,b1) (a2,b2)
    | !a <- a1/\a2
    , !b <- b1/\b2
    = (a,b)
instance (Join a, Join b) => Join (a,b) where
  (\/) (a1,b1) (a2,b2)
    | !a <- a1\/a2
    , !b <- b1\/b2
    = (a,b)
instance (Diff a, Diff b) => Diff (a,b) where
  (\\) (a1,b1) (a2,b2)
    | !a <- a1\\a2
    , !b <- b1\\b2
    = (a,b)

-----------------------------------------------------------------------------

class (Empty a, Meet a, Join a, Diff a) => O a
instance O ()
instance (O a, O b) => O (a,b)
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
  instance (__VA_ARGS__) => O (A_t);
INSTANCES_O_SET(IS,IntSet)
INSTANCES_O_SET(IxS,IxSet a)
INSTANCES_O_SET(S,Set a,Ord a)
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
  instance (__VA_ARGS__) => O (A_t);
INSTANCES_O_MAP(IM,IntMap b,O b)
INSTANCES_O_MAP(Ix,IxMap a b,O b)
INSTANCES_O_MAP(M,Map a b,Ord a,O b)

-----------------------------------------------------------------------------
