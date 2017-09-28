
module MM.Data.Class.Map where

import MM.Data.Class.List

import MM.Data.Ix.Types
import MM.Data.Ix.Map
  (IxMap,Index,Trans,Rename)
import MM.Data.Ix.Set(IxSet)
import MM.Data.Ix.Trie(IxTrie)
import MM.Data.Map.Ord(Map)
import MM.Data.Set.Ord(Set)
import MM.Data.Map.Int(IntMap)
import MM.Data.Set.Int(IntSet)
import qualified MM.Data.Map.Ord as M
import qualified MM.Data.Set.Ord as S
import qualified MM.Data.Map.Int as IM
import qualified MM.Data.Set.Int as IS
import MM.Data.Quotient.UF(UF(..))
import qualified MM.Data.Quotient.UF as UF
import qualified MM.Data.Quotient.UF.Alt2 as UF2
import qualified MM.Data.Ix.Map as Ix
import qualified MM.Data.Ix.Set as IxS
import qualified MM.Data.Ix.Trie as IxT
import Data.Monoid(mempty)
import Data.List(foldl')

-----------------------------------------------------------------------------

-- | .
class Dom map a where
  dom :: map -> a

class Codom map b where
  codom :: map -> b

class (Restrict map a, Codom map b) => Img map a b where
  img :: map -> a -> b
  img f a = codom (restrict f a)

class InvImg map invmap where
  invImg :: map -> invmap

class Restrict map a where
  restrict :: map -> a -> map

class Compose ab bc ac | ab bc -> ac where
  (|.|) :: bc -> ab -> ac

class GetSetLook m a b where
  get :: m -> a -> b
  set :: m -> a -> b -> m
  look :: m -> a -> Maybe b

-----------------------------------------------------------------------------

instance Compose (Trans a b) (Trans b c) (Trans a c) where
  (|.|) g f = Ix.foldWithKey go mempty f
    where go i j h
            | Just k <- Ix.lookup j g
            = Ix.insert i k h
            | otherwise = h

instance Compose (IntMap Int) (IntMap Int) (IntMap Int) where
  (|.|) g f = IM.foldWithKey go mempty f
    where go i j h
            | Just k <- IM.lookup j g
            = IM.insert i k h
            | otherwise = h

instance (Ord a, Ord b) => Compose (Map a b) (Map b c) (Map a c) where
  (|.|) g f = M.foldWithKey go mempty f
    where go i j h
            | Just k <- M.lookup j g
            = M.insert i k h
            | otherwise = h

-----------------------------------------------------------------------------

instance InvImg (IxMap a (Ix b)) (IxMap b (IxSet a)) where
  invImg f = Ix.foldWithKey go mempty f
    where go i j g
            | !iset <- IxS.singleton i
            = Ix.insertWith IxS.union j iset g

instance InvImg (IxMap a Int) (IntMap (IxSet a)) where
  invImg f = Ix.foldWithKey go mempty f
    where go i j g
            | !iset <- IxS.singleton i
            = IM.insertWith IxS.union j iset g

instance InvImg (IntMap (Ix b)) (IxMap b IntSet) where
  invImg f = IM.foldWithKey go mempty f
    where go i j g
            | !iset <- IS.singleton i
            = Ix.insertWith IS.union j iset g

instance InvImg (IntMap Int) (IntMap IntSet) where
  invImg f = IM.foldWithKey go mempty f
    where go i j g
            | !iset <- IS.singleton i
            = IM.insertWith IS.union j iset g

-----------------------------------------------------------------------------

instance InvImg (IxMap a (IxSet b)) (IxMap b (IxSet a)) where
  invImg f = Ix.foldWithKey go mempty f
    where go i js g
            | !iset <- IxS.singleton i
            = IxS.fold (\j g-> Ix.insertWith IxS.union j iset g) g js

instance InvImg (IxMap a [Ix b]) (IxMap b (IxSet a)) where
  invImg f = Ix.foldWithKey go mempty f
    where go i js g
            | !iset <- IxS.singleton i
            = foldl' (\g j-> Ix.insertWith IxS.union j iset g) g js

instance InvImg (IxMap a IntSet) (IntMap (IxSet a)) where
  invImg f = Ix.foldWithKey go mempty f
    where go i js g
            | !iset <- IxS.singleton i
            = IS.fold (\j g-> IM.insertWith IxS.union j iset g) g js

instance InvImg (IxMap a [Int]) (IntMap (IxSet a)) where
  invImg f = Ix.foldWithKey go mempty f
    where go i js g
            | !iset <- IxS.singleton i
            = foldl' (\g j-> IM.insertWith IxS.union j iset g) g js

instance InvImg (IntMap (IxSet b)) (IxMap b IntSet) where
  invImg f = IM.foldWithKey go mempty f
    where go i js g
            | !iset <- IS.singleton i
            = IxS.fold (\j g-> Ix.insertWith IS.union j iset g) g js

instance InvImg (IntMap [Ix b]) (IxMap b IntSet) where
  invImg f = IM.foldWithKey go mempty f
    where go i js g
            | !iset <- IS.singleton i
            = foldl' (\g j-> Ix.insertWith IS.union j iset g) g js

instance InvImg (IntMap IntSet) (IntMap IntSet) where
  invImg f = IM.foldWithKey go mempty f
    where go i js g
            | !iset <- IS.singleton i
            = IS.fold (\j g-> IM.insertWith IS.union j iset g) g js

instance InvImg (IntMap [Int]) (IntMap IntSet) where
  invImg f = IM.foldWithKey go mempty f
    where go i js g
            | !iset <- IS.singleton i
            = foldl' (\g j-> IM.insertWith IS.union j iset g) g js

-----------------------------------------------------------------------------

instance GetSetLook (IxMap a b) (Ix a) b where
  get o i = o Ix.! i
  set o i x = Ix.insert i x o
  look o i = Ix.lookup i o

instance Dom (IxMap a b) [Ix a] where dom f = Ix.keys f
instance Dom (IxMap a b) (IxSet a) where dom f = fromList (Ix.keys f)
instance Codom (IxMap a b) [b] where codom f = Ix.elems f
instance Codom (Trans a b) (IxSet b) where codom f = fromList (Ix.elems f)
instance Codom (IxMap a Int) IntSet where codom f = fromList (Ix.elems f)
instance (Ord b) => Codom (IxMap a b) (Set b) where codom f = fromList (Ix.elems f)

instance Img (IxMap a b) (IxSet a) [b]
instance Img (Trans a b) (IxSet a) (IxSet b)
instance Img (IxMap a Int) (IxSet a) IntSet
instance (Ord b) => Img (IxMap a b) (IxSet a) (Set b)

instance Restrict (IxMap a b) [Ix a] where
  restrict f as = foldl' go mempty as
    where go g a = case Ix.lookup a f of
            Just b-> Ix.insert a b g
            Nothing-> g

instance Restrict (IxMap a b) (IxSet a) where
  restrict f as = IxS.fold go mempty as
    where go a g = case Ix.lookup a f of
            Just b-> Ix.insert a b g
            Nothing-> g

-----------------------------------------------------------------------------

instance GetSetLook (IntMap b) Int b where
  get o i = o IM.! i
  set o i x = IM.insert i x o
  look o i = IM.lookup i o

instance Dom (IntMap b) [Int] where dom f = IM.keys f
instance Dom (IntMap b) IntSet where dom f = fromList (IM.keys f)
instance Codom (IntMap b) [b] where codom f = IM.elems f
instance Codom (IntMap Int) IntSet where codom f = fromList (IM.elems f)
instance Codom (IntMap (Ix b)) (IxSet b) where codom f = fromList (IM.elems f)
instance (Ord b) => Codom (IntMap b) (Set b) where codom f = fromList (IM.elems f)

instance Img (IntMap b) IntSet [b]
instance Img (IntMap Int) IntSet IntSet
instance Img (IntMap (Ix b)) IntSet (IxSet b)
instance (Ord b) => Img (IntMap b) IntSet (Set b)

instance Restrict (IntMap b) [Int] where
  restrict f as = foldl' go mempty as
    where go g a = case IM.lookup a f of
            Just b-> IM.insert a b g
            Nothing-> g

instance Restrict (IntMap b) IntSet where
  restrict f as = IS.fold go mempty as
    where go a g = case IM.lookup a f of
            Just b-> IM.insert a b g
            Nothing-> g

-----------------------------------------------------------------------------

instance (Ord a) => Dom (Map a b) [a] where dom f = M.keys f
instance (Ord a) => Dom (Map a b) (Set a) where dom f = fromList (M.keys f)
instance (Ord a) => Codom (Map a b) [b] where codom f = M.elems f
instance (Ord a, Ord b) => Codom (Map a b) (Set b) where codom f = fromList (M.elems f)
instance (Ord a) => Codom (Map a (Ix b)) (IxSet b) where codom f = fromList (M.elems f)
instance (Ord a) => Codom (Map a (IxSet b)) (IxSet b) where codom f = IxS.unions (M.elems f)

-----------------------------------------------------------------------------

instance Dom (UF a b) [Ix a] where dom = UF.keys
instance Dom (UF a b) (IxSet a) where dom = fromList . UF.keys
instance Dom (UF2.UF a b) [Ix a] where dom = UF2.keys
instance Dom (UF2.UF a b) (IxSet a) where dom = fromList . UF2.keys

instance Codom (UF a b) [b] where codom = UF.elems
instance (Ord b) => Codom (UF a b) (Set b) where codom = fromList . UF.elems
instance (Ord b) => Codom (UF2.UF a b) (Set b) where codom = fromList . UF2.elems

-----------------------------------------------------------------------------
