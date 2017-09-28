{-# LANGUAGE CPP #-}

module MM.Data.Class.List (
   List(..)
  ,FromList(..)
  ,ToList(..)
) where

import MM.Data.Map.Ord(Map)
import MM.Data.Set.Ord(Set)
import MM.Data.Map.Int(IntMap)
import MM.Data.Set.Int(IntSet)
import MM.Data.Map.Ix(IxMap)
import MM.Data.Set.Ix(IxSet)
import MM.Data.Types.Ix(Ix(..),unIx)
import qualified MM.Data.Map.Ord as M
import qualified MM.Data.Set.Ord as S
import qualified MM.Data.Map.Int as IM
import qualified MM.Data.Set.Int as IS
import qualified MM.Data.Map.Ix as Ix
import qualified MM.Data.Set.Ix as IxS

-----------------------------------------------------------------------------

-- | .
class ToList o a | o -> a where
  toList :: o -> [a]
class FromList o a | o -> a where
  fromList :: [a] -> o
class (FromList o a,ToList o b) => List o a b | o -> a b

-----------------------------------------------------------------------------

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
