{-# LANGUAGE CPP,BangPatterns #-}

module MM.Data.Map.Ord (
   Map

  ,mapM
  ,foldWithKeyM
  ,unionWithKeyM

  ,(!), null, size, member
  ,notMember, lookup, findWithDefault, empty, singleton, insert
  ,insertWith, insertWithKey, insertLookupWithKey, insertWith'
  ,insertWithKey', delete, adjust, adjustWithKey, update
  ,updateWithKey, updateLookupWithKey, alter, union, unionWith
  ,unionWithKey, unions, unionsWith, difference, differenceWith
  ,differenceWithKey, intersection, intersectionWith
  ,intersectionWithKey, map, mapWithKey, mapAccum, mapAccumWithKey
  ,mapAccumRWithKey, mapKeys, mapKeysWith, mapKeysMonotonic, fold
  ,foldWithKey, foldrWithKey, foldlWithKey, elems, keys, keysSet
  ,assocs, toList, fromList, fromListWith, fromListWithKey, toAscList
  ,toDescList, fromAscList, fromAscListWith, fromAscListWithKey
  ,fromDistinctAscList, filter, filterWithKey, partition
  ,partitionWithKey, mapMaybe, mapMaybeWithKey, mapEither
  ,mapEitherWithKey, split, splitLookup, isSubmapOf, isSubmapOfBy
  ,isProperSubmapOf, isProperSubmapOfBy, lookupIndex, findIndex
  ,elemAt, updateAt, deleteAt, findMin, findMax, deleteMin, deleteMax
  ,deleteFindMin, deleteFindMax, updateMin, updateMax
  ,updateMinWithKey, updateMaxWithKey, minView, maxView
  ,minViewWithKey, maxViewWithKey, showTree, showTreeWith, valid
  ,
) where

import qualified Data.Binary as Bin
import System.IO.Unsafe
import Prelude hiding (lookup, map, filter, null, mapM)
import qualified MM.Data.Set.Ord as Set
import qualified Data.List as List
import Data.Monoid (Monoid(..))
import Control.Applicative (Applicative(pure, (<*>)), (<$>))
import Text.Read

foldlStrict :: (a -> b -> a) -> a -> [b] -> a
foldlStrict f z xs
  = case xs of
      [] -> z
      (x : xx) -> let z' = f z x in seq z' (foldlStrict f z' xx)

mapM :: (Monad m) => (a -> m b) -> Map k a -> m (Map k b)
mapM f = go
  where go (Tip) = return Tip
        go (Bin o k x l r)
          = do !x <- f x
               !l <- go l
               !r <- go r
               return (Bin o k x l r)

foldWithKeyM :: (Monad m) => (k -> a -> b -> m b) -> b -> Map k a -> m b
foldWithKeyM f = go
  where go z (Tip) = return z
        go z (Bin _ k x l r)
          = do !z <- go z l
               !z <- f k x z
               go z r

-----------------------------------------------------------------------------

delta, ratio :: Int
delta = 5
ratio = 2

balance :: k -> a -> Map k a -> Map k a -> Map k a
balance k x l r
  | sizeL + sizeR <= 1 = Bin sizeX k x l r
  | sizeR >= delta * sizeL = rotateL k x l r
  | sizeL >= delta * sizeR = rotateR k x l r
  | otherwise = Bin sizeX k x l r
  where sizeL = size l
        sizeR = size r
        sizeX = sizeL + sizeR + 1

rotateL :: a -> b -> Map a b -> Map a b -> Map a b
rotateL k x l r@(Bin _ _ _ ly ry)
  | size ly < ratio * size ry = singleL k x l r
  | otherwise = doubleL k x l r
rotateL _ _ _ (Tip) = error "rotateL Tip"

rotateR :: a -> b -> Map a b -> Map a b -> Map a b
rotateR k x l@(Bin _ _ _ ly ry) r
  | size ry < ratio * size ly = singleR k x l r
  | otherwise = doubleR k x l r
rotateR _ _ (Tip) _ = error "rotateR Tip"

singleL, singleR :: a -> b -> Map a b -> Map a b -> Map a b
singleL k1 x1 t1 (Bin _ k2 x2 t2 t3)
  = bin k2 x2 (bin k1 x1 t1 t2) t3
singleL _ _ _ (Tip) = error "singleL Tip"

singleR k1 x1 (Bin _ k2 x2 t1 t2) t3
  = bin k2 x2 t1 (bin k1 x1 t2 t3)
singleR _ _ (Tip) _ = error "singleR Tip"

doubleL, doubleR :: a -> b -> Map a b -> Map a b -> Map a b
doubleL k1 x1 t1 (Bin _ k2 x2 (Bin _ k3 x3 t2 t3) t4)
  = bin k3 x3 (bin k1 x1 t1 t2) (bin k2 x2 t3 t4)
doubleL _ _ _ _ = error "doubleL"

doubleR k1 x1 (Bin _ k2 x2 t1 (Bin _ k3 x3 t2 t3)) t4
  = bin k3 x3 (bin k2 x2 t1 t2) (bin k1 x1 t3 t4)
doubleR _ _ _ _ = error "doubleR"

bin :: k -> a -> Map k a -> Map k a -> Map k a
bin k x l r = Bin (size l + size r + 1) k x l r

-----------------------------------------------------------------------------

data Map k a = Bin !Size !k a !(Map k a) !(Map k a) | Tip

type Size = Int

instance (Bin.Binary k, Bin.Binary a) => Bin.Binary (Map k a) where
  put (Tip) = Bin.putWord8 0
  put (Bin s k a l r)
    = Bin.putWord8 1 >> Bin.put s >> Bin.put k >> Bin.put a >> Bin.put l >> Bin.put r
  get
    = do tag <- Bin.getWord8
         case tag of
           0 -> return Tip
           _ -> Bin <$> Bin.get <*> Bin.get <*> Bin.get <*> Bin.get <*> Bin.get

instance (Ord k) => Monoid (Map k v) where
  mempty = empty
  mappend = union
  mconcat = unions

-----------------------------------------------------------------------------

null :: Map k a -> Bool
null t
  = case t of
      Tip -> True
      Bin{} -> False

size :: Map k a -> Int
size t
  = case t of
      Tip -> 0
      Bin sz _ _ _ _ -> sz

empty :: Map k a
empty = Tip

singleton :: k -> a -> Map k a
singleton k x = Bin 1 k x Tip Tip

-----------------------------------------------------------------------------

infixl 9 !
(!) :: (Ord k) => Map k a -> k -> a
(!) m k = find k m

#if 0
infixl 9 \\
(\\) :: (Ord k) => Map k a -> Map k b -> Map k a
(\\) m1 m2 = difference m1 m2
#endif

-----------------------------------------------------------------------------

lookup :: (Ord k) => k -> Map k a -> Maybe a
lookup k t
  = case t of
      Tip -> Nothing
      Bin _ kx x l r -> case compare k kx of
                          LT -> lookup k l
                          GT -> lookup k r
                          EQ -> Just x

lookupAssoc :: (Ord k) => k -> Map k a -> Maybe (k, a)
lookupAssoc k t
  = case t of
      Tip -> Nothing
      Bin _ kx x l r -> case compare k kx of
                          LT -> lookupAssoc k l
                          GT -> lookupAssoc k r
                          EQ -> Just (kx, x)

member :: (Ord k) => k -> Map k a -> Bool
member k m
  = case lookup k m of
      Nothing -> False
      Just _ -> True

notMember :: (Ord k) => k -> Map k a -> Bool
notMember k m = not $ member k m

find :: (Ord k) => k -> Map k a -> a
find k m
  = case lookup k m of
      Nothing -> error ("Map.find: element not in the map")
      Just x -> x

findWithDefault :: (Ord k) => a -> k -> Map k a -> a
findWithDefault def k m
  = case lookup k m of
      Nothing -> def
      Just x -> x

-----------------------------------------------------------------------------

insert :: (Ord k) => k -> a -> Map k a -> Map k a
insert kx x t
  = case t of
      Tip -> singleton kx x
      Bin sz ky y l r -> case compare kx ky of
                           LT -> balance ky y (insert kx x l) r
                           GT -> balance ky y l (insert kx x r)
                           EQ -> Bin sz kx x l r

insertWith :: (Ord k) => (a -> a -> a) -> k -> a -> Map k a -> Map k a
insertWith f k x m = insertWithKey (\ _ x' y' -> f x' y') k x m

insertWith' :: (Ord k) => (a -> a -> a) -> k -> a -> Map k a -> Map k a
insertWith' f k x m = insertWithKey' (\ _ x' y' -> f x' y') k x m

insertWithKey :: (Ord k) => (k -> a -> a -> a) -> k -> a -> Map k a -> Map k a
insertWithKey f kx x t
  = case t of
      Tip -> singleton kx x
      Bin sy ky y l r -> case compare kx ky of
                           LT -> balance ky y (insertWithKey f kx x l) r
                           GT -> balance ky y l (insertWithKey f kx x r)
                           EQ -> Bin sy kx (f kx x y) l r

insertWithKey' :: (Ord k) => (k -> a -> a -> a) -> k -> a -> Map k a -> Map k a
insertWithKey' f kx x t
  = case t of
      Tip -> singleton kx x
      Bin sy ky y l r -> case compare kx ky of
                           LT -> balance ky y (insertWithKey' f kx x l) r
                           GT -> balance ky y l (insertWithKey' f kx x r)
                           EQ -> let x' = f kx x y in seq x' (Bin sy kx x' l r)

insertLookupWithKey :: (Ord k) => (k -> a -> a -> a) -> k -> a -> Map k a -> (Maybe a, Map k a)
insertLookupWithKey f kx x t
  = case t of
      Tip -> (Nothing, singleton kx x)
      Bin sy ky y l r -> case compare kx ky of
                           LT -> let (found, l') = insertLookupWithKey f kx x l in
                                   (found, balance ky y l' r)
                           GT -> let (found, r') = insertLookupWithKey f kx x r in
                                   (found, balance ky y l r')
                           EQ -> (Just y, Bin sy kx (f kx x y) l r)

-----------------------------------------------------------------------------

delete :: (Ord k) => k -> Map k a -> Map k a
delete k t
  = case t of
      Tip -> Tip
      Bin _ kx x l r -> case compare k kx of
                          LT -> balance kx x (delete k l) r
                          GT -> balance kx x l (delete k r)
                          EQ -> glue l r

-----------------------------------------------------------------------------

adjust :: (Ord k) => (a -> a) -> k -> Map k a -> Map k a
adjust f k m = adjustWithKey (\ _ x -> f x) k m

adjustWithKey :: (Ord k) => (k -> a -> a) -> k -> Map k a -> Map k a
adjustWithKey f k m = updateWithKey (\ k' x' -> Just (f k' x')) k m

update :: (Ord k) => (a -> Maybe a) -> k -> Map k a -> Map k a
update f k m = updateWithKey (\ _ x -> f x) k m

updateWithKey :: (Ord k) => (k -> a -> Maybe a) -> k -> Map k a -> Map k a
updateWithKey f k t
  = case t of
      Tip -> Tip
      Bin sx kx x l r -> case compare k kx of
                           LT -> balance kx x (updateWithKey f k l) r
                           GT -> balance kx x l (updateWithKey f k r)
                           EQ -> case f kx x of
                                   Just x' -> Bin sx kx x' l r
                                   Nothing -> glue l r

updateLookupWithKey :: (Ord k) => (k -> a -> Maybe a) -> k -> Map k a -> (Maybe a, Map k a)
updateLookupWithKey f k t
  = case t of
      Tip -> (Nothing, Tip)
      Bin sx kx x l r -> case compare k kx of
                           LT -> let (found, l') = updateLookupWithKey f k l in
                                   (found, balance kx x l' r)
                           GT -> let (found, r') = updateLookupWithKey f k r in
                                   (found, balance kx x l r')
                           EQ -> case f kx x of
                                   Just x' -> (Just x', Bin sx kx x' l r)
                                   Nothing -> (Just x, glue l r)

alter :: (Ord k) => (Maybe a -> Maybe a) -> k -> Map k a -> Map k a
alter f k t
  = case t of
      Tip -> case f Nothing of
               Nothing -> Tip
               Just x -> singleton k x
      Bin sx kx x l r -> case compare k kx of
                           LT -> balance kx x (alter f k l) r
                           GT -> balance kx x l (alter f k r)
                           EQ -> case f (Just x) of
                                   Just x' -> Bin sx kx x' l r
                                   Nothing -> glue l r

-----------------------------------------------------------------------------

findIndex :: (Ord k) => k -> Map k a -> Int
findIndex k t
  = case lookupIndex k t of
      Nothing -> error "Map.findIndex: element is not in the map"
      Just idx -> idx

lookupIndex :: (Ord k) => k -> Map k a -> Maybe Int
lookupIndex k t = f 0 t
  where f _ (Tip) = Nothing
        f idx (Bin _ kx _ l r)
          = case compare k kx of
              LT -> f idx l
              GT -> f (idx + size l + 1) r
              EQ -> Just (idx + size l)

-----------------------------------------------------------------------------

elemAt :: Int -> Map k a -> (k, a)
elemAt _ (Tip) = error "Map.elemAt: index out of range"
elemAt i (Bin _ kx x l r)
  = case compare i sizeL of
      LT -> elemAt i l
      GT -> elemAt (i - sizeL - 1) r
      EQ -> (kx, x)
  where sizeL = size l

updateAt :: (k -> a -> Maybe a) -> Int -> Map k a -> Map k a
updateAt _ _ (Tip) = error "Map.updateAt: index out of range"
updateAt f i (Bin sx kx x l r)
  = case compare i sizeL of
      LT -> balance kx x (updateAt f i l) r
      GT -> balance kx x l (updateAt f (i - sizeL - 1) r)
      EQ -> case f kx x of
              Just x' -> Bin sx kx x' l r
              Nothing -> glue l r
  where sizeL = size l

deleteAt :: Int -> Map k a -> Map k a
deleteAt i m = updateAt (\ _ _ -> Nothing) i m

-----------------------------------------------------------------------------

findMin :: Map k a -> (k, a)
findMin (Bin _ kx x Tip _) = (kx, x)
findMin (Bin _ _ _ l _) = findMin l
findMin (Tip)
  = error "Map.findMin: empty map has no minimal element"

findMax :: Map k a -> (k, a)
findMax (Bin _ kx x _ Tip) = (kx, x)
findMax (Bin _ _ _ _ r) = findMax r
findMax (Tip)
  = error "Map.findMax: empty map has no maximal element"

deleteMin :: Map k a -> Map k a
deleteMin (Bin _ _ _ Tip r) = r
deleteMin (Bin _ kx x l r) = balance kx x (deleteMin l) r
deleteMin (Tip) = Tip
deleteMax :: Map k a -> Map k a
deleteMax (Bin _ _ _ l Tip) = l
deleteMax (Bin _ kx x l r) = balance kx x l (deleteMax r)
deleteMax (Tip) = Tip

updateMin :: (a -> Maybe a) -> Map k a -> Map k a
updateMin f m = updateMinWithKey (\ _ x -> f x) m

updateMax :: (a -> Maybe a) -> Map k a -> Map k a
updateMax f m = updateMaxWithKey (\ _ x -> f x) m

updateMinWithKey :: (k -> a -> Maybe a) -> Map k a -> Map k a
updateMinWithKey f t
  = case t of
      Bin sx kx x Tip r -> case f kx x of
                             Nothing -> r
                             Just x' -> Bin sx kx x' Tip r
      Bin _ kx x l r -> balance kx x (updateMinWithKey f l) r
      Tip -> Tip

updateMaxWithKey :: (k -> a -> Maybe a) -> Map k a -> Map k a
updateMaxWithKey f t
  = case t of
      Bin sx kx x l Tip -> case f kx x of
                             Nothing -> l
                             Just x' -> Bin sx kx x' l Tip
      Bin _ kx x l r -> balance kx x l (updateMaxWithKey f r)
      Tip -> Tip

minViewWithKey :: Map k a -> Maybe ((k, a), Map k a)
minViewWithKey (Tip) = Nothing
minViewWithKey x = Just (deleteFindMin x)
maxViewWithKey :: Map k a -> Maybe ((k, a), Map k a)
maxViewWithKey (Tip) = Nothing
maxViewWithKey x = Just (deleteFindMax x)

minView :: Map k a -> Maybe (a, Map k a)
minView (Tip) = Nothing
minView x = Just (first snd $ deleteFindMin x)
maxView :: Map k a -> Maybe (a, Map k a)
maxView (Tip) = Nothing
maxView x = Just (first snd $ deleteFindMax x)

first :: (a -> b) -> (a, c) -> (b, c)
first f (x, y) = (f x, y)

-----------------------------------------------------------------------------

unions :: (Ord k) => [Map k a] -> Map k a
unions ts = foldlStrict union empty ts

unionsWith :: (Ord k) => (a -> a -> a) -> [Map k a] -> Map k a
unionsWith f ts = foldlStrict (unionWith f) empty ts

union :: (Ord k) => Map k a -> Map k a -> Map k a
union (Tip) t2 = t2
union t1 (Tip) = t1
union t1 t2 = hedgeUnionL (const LT) (const GT) t1 t2

hedgeUnionL :: (Ord a) => (a -> Ordering) -> (a -> Ordering) -> Map a b -> Map a b -> Map a b
hedgeUnionL _ _ t1 (Tip) = t1
hedgeUnionL cmplo cmphi (Tip) (Bin _ kx x l r) = join kx x (filterGt cmplo l) (filterLt cmphi r)
hedgeUnionL cmplo cmphi (Bin _ kx x l r) t2 =
  join kx x
    (hedgeUnionL cmplo cmpkx l (trim cmplo cmpkx t2))
    (hedgeUnionL cmpkx cmphi r (trim cmpkx cmphi t2))
  where cmpkx k = compare kx k

unionWith :: (Ord k) => (a -> a -> a) -> Map k a -> Map k a -> Map k a
unionWith f m1 m2 = unionWithKey (\ _ x y -> f x y) m1 m2

unionWithKey :: (Ord k) => (k -> a -> a -> a) -> Map k a -> Map k a -> Map k a
unionWithKey _ (Tip) t2 = t2
unionWithKey _ t1 (Tip) = t1
unionWithKey f t1 t2 = hedgeUnionWithKey f (const LT) (const GT) t1 t2

hedgeUnionWithKey :: (Ord a) => (a -> b -> b -> b) -> (a -> Ordering) -> (a -> Ordering) -> Map a b -> Map a b -> Map a b
hedgeUnionWithKey _ _ _ t1 (Tip) = t1
hedgeUnionWithKey _ cmplo cmphi (Tip) (Bin _ kx x l r) = join kx x (filterGt cmplo l) (filterLt cmphi r)
hedgeUnionWithKey f cmplo cmphi (Bin _ kx x l r) t2 =
  join kx newx
    (hedgeUnionWithKey f cmplo cmpkx l lt)
    (hedgeUnionWithKey f cmpkx cmphi r gt)
  where cmpkx k = compare kx k
        lt = trim cmplo cmpkx t2
        (found, gt) = trimLookupLo kx cmphi t2
        newx
          = case found of
              Nothing -> x
              Just (_, y) -> f kx x y

unionWithKeyM :: (Monad m, Ord k) => (k -> a -> a -> m a) -> Map k a -> Map k a -> m (Map k a)
unionWithKeyM _ (Tip) t2 = return t2
unionWithKeyM _ t1 (Tip) = return t1
unionWithKeyM f t1 t2 = hedgeUnionWithKeyM f (const LT) (const GT) t1 t2

hedgeUnionWithKeyM
  :: (Monad m, Ord k)
  => (k -> a -> a -> m a)
  -> (k -> Ordering)
  -> (k -> Ordering)
  -> Map k a -> Map k a -> m (Map k a)
hedgeUnionWithKeyM _ _ _ t1 (Tip) = return t1
hedgeUnionWithKeyM _ cmplo cmphi (Tip) (Bin _ kx x l r)
  | !l <- filterGt cmplo l
  , !r <- filterLt cmphi r
  , !o <- join kx x l r = return o
hedgeUnionWithKeyM f cmplo cmphi (Bin _ kx x l r) t2
  | let cmpkx k = compare kx k
  ,        !lt  <- trim cmplo cmpkx t2
  , (found,!gt) <- trimLookupLo kx cmphi t2 = do
    !newx <- case found of
              Nothing-> return x
              Just (_,y)-> f kx x y
    !l <- hedgeUnionWithKeyM f cmplo cmpkx l lt
    !r <- hedgeUnionWithKeyM f cmpkx cmphi r gt
    let !o = join kx newx l r
    return o

-----------------------------------------------------------------------------

difference :: (Ord k) => Map k a -> Map k b -> Map k a
difference (Tip) _ = Tip
difference t1 (Tip) = t1
difference t1 t2 = hedgeDiff (const LT) (const GT) t1 t2

hedgeDiff :: (Ord a) => (a -> Ordering) -> (a -> Ordering) -> Map a b -> Map a c -> Map a b
hedgeDiff _ _ (Tip) _ = Tip
hedgeDiff cmplo cmphi (Bin _ kx x l r) (Tip)
  = join kx x (filterGt cmplo l) (filterLt cmphi r)
hedgeDiff cmplo cmphi t (Bin _ kx _ l r)
  = merge (hedgeDiff cmplo cmpkx (trim cmplo cmpkx t) l)
      (hedgeDiff cmpkx cmphi (trim cmpkx cmphi t) r)
  where cmpkx k = compare kx k

differenceWith :: (Ord k) => (a -> b -> Maybe a) -> Map k a -> Map k b -> Map k a
differenceWith f m1 m2 = differenceWithKey (\ _ x y -> f x y) m1 m2

differenceWithKey :: (Ord k) => (k -> a -> b -> Maybe a) -> Map k a -> Map k b -> Map k a
differenceWithKey _ (Tip) _ = Tip
differenceWithKey _ t1 (Tip) = t1
differenceWithKey f t1 t2
  = hedgeDiffWithKey f (const LT) (const GT) t1 t2

hedgeDiffWithKey :: (Ord a) => (a -> b -> c -> Maybe b) -> (a -> Ordering) -> (a -> Ordering) -> Map a b -> Map a c -> Map a b
hedgeDiffWithKey _ _ _ (Tip) _ = Tip
hedgeDiffWithKey _ cmplo cmphi (Bin _ kx x l r) (Tip)
  = join kx x (filterGt cmplo l) (filterLt cmphi r)
hedgeDiffWithKey f cmplo cmphi t (Bin _ kx x l r)
  = case found of
      Nothing -> merge tl tr
      Just (ky, y) -> case f ky y x of
                        Nothing -> merge tl tr
                        Just z -> join ky z tl tr
  where cmpkx k = compare kx k
        lt = trim cmplo cmpkx t
        (found, gt) = trimLookupLo kx cmphi t
        tl = hedgeDiffWithKey f cmplo cmpkx lt l
        tr = hedgeDiffWithKey f cmpkx cmphi gt r

-----------------------------------------------------------------------------

intersection :: (Ord k) => Map k a -> Map k b -> Map k a
intersection m1 m2 = intersectionWithKey (\ _ x _ -> x) m1 m2

intersectionWith :: (Ord k) => (a -> b -> c) -> Map k a -> Map k b -> Map k c
intersectionWith f m1 m2
  = intersectionWithKey (\ _ x y -> f x y) m1 m2

intersectionWithKey :: (Ord k) => (k -> a -> b -> c) -> Map k a -> Map k b -> Map k c
intersectionWithKey _ (Tip) _ = Tip
intersectionWithKey _ _ (Tip) = Tip
intersectionWithKey f t1@(Bin s1 k1 x1 l1 r1)
  t2@(Bin s2 k2 x2 l2 r2)
  = if s1 >= s2 then
      let (lt, found, gt) = splitLookupWithKey k2 t1
          tl = intersectionWithKey f lt l2
          tr = intersectionWithKey f gt r2
        in
        case found of
          Just (k, x) -> join k (f k x x2) tl tr
          Nothing -> merge tl tr
      else
      let (lt, found, gt) = splitLookup k1 t2
          tl = intersectionWithKey f l1 lt
          tr = intersectionWithKey f r1 gt
        in
        case found of
          Just x -> join k1 (f k1 x1 x) tl tr
          Nothing -> merge tl tr

-----------------------------------------------------------------------------

isSubmapOf :: (Ord k, Eq a) => Map k a -> Map k a -> Bool
isSubmapOf m1 m2 = isSubmapOfBy (==) m1 m2

isSubmapOfBy :: (Ord k) => (a -> b -> Bool) -> Map k a -> Map k b -> Bool
isSubmapOfBy f t1 t2 = (size t1 <= size t2) && (submap' f t1 t2)

submap' :: (Ord a) => (b -> c -> Bool) -> Map a b -> Map a c -> Bool
submap' _ (Tip) _ = True
submap' _ _ (Tip) = False
submap' f (Bin _ kx x l r) t
  = case found of
      Nothing -> False
      Just y -> f x y && submap' f l lt && submap' f r gt
  where (lt, found, gt) = splitLookup kx t

isProperSubmapOf :: (Ord k, Eq a) => Map k a -> Map k a -> Bool
isProperSubmapOf m1 m2 = isProperSubmapOfBy (==) m1 m2

isProperSubmapOfBy :: (Ord k) => (a -> b -> Bool) -> Map k a -> Map k b -> Bool
isProperSubmapOfBy f t1 t2
  = (size t1 < size t2) && (submap' f t1 t2)

-----------------------------------------------------------------------------

filter :: (Ord k) => (a -> Bool) -> Map k a -> Map k a
filter p m = filterWithKey (\ _ x -> p x) m

filterWithKey :: (Ord k) => (k -> a -> Bool) -> Map k a -> Map k a
filterWithKey _ (Tip) = Tip
filterWithKey p (Bin _ kx x l r)
  | p kx x = join kx x (filterWithKey p l) (filterWithKey p r)
  | otherwise = merge (filterWithKey p l) (filterWithKey p r)

-----------------------------------------------------------------------------

partition :: (Ord k) => (a -> Bool) -> Map k a -> (Map k a, Map k a)
partition p m = partitionWithKey (\ _ x -> p x) m

partitionWithKey :: (Ord k) => (k -> a -> Bool) -> Map k a -> (Map k a, Map k a)
partitionWithKey _ (Tip) = (Tip, Tip)
partitionWithKey p (Bin _ kx x l r)
  | p kx x = (join kx x l1 r1, merge l2 r2)
  | otherwise = (merge l1 r1, join kx x l2 r2)
  where (l1, l2) = partitionWithKey p l
        (r1, r2) = partitionWithKey p r

-----------------------------------------------------------------------------

mapMaybe :: (Ord k) => (a -> Maybe b) -> Map k a -> Map k b
mapMaybe f m = mapMaybeWithKey (\ _ x -> f x) m

mapMaybeWithKey :: (Ord k) => (k -> a -> Maybe b) -> Map k a -> Map k b
mapMaybeWithKey _ (Tip) = Tip
mapMaybeWithKey f (Bin _ kx x l r)
  = case f kx x of
      Just y -> join kx y (mapMaybeWithKey f l) (mapMaybeWithKey f r)
      Nothing -> merge (mapMaybeWithKey f l) (mapMaybeWithKey f r)

mapEither :: (Ord k) => (a -> Either b c) -> Map k a -> (Map k b, Map k c)
mapEither f m = mapEitherWithKey (\ _ x -> f x) m

mapEitherWithKey :: (Ord k) => (k -> a -> Either b c) -> Map k a -> (Map k b, Map k c)
mapEitherWithKey _ (Tip) = (Tip, Tip)
mapEitherWithKey f (Bin _ kx x l r)
  = case f kx x of
      Left y -> (join kx y l1 r1, merge l2 r2)
      Right z -> (merge l1 r1, join kx z l2 r2)
  where (l1, l2) = mapEitherWithKey f l
        (r1, r2) = mapEitherWithKey f r

-----------------------------------------------------------------------------

map :: (a -> b) -> Map k a -> Map k b
map f m = mapWithKey (\ _ x -> f x) m

mapWithKey :: (k -> a -> b) -> Map k a -> Map k b
mapWithKey _ (Tip) = Tip
mapWithKey f (Bin sx kx x l r)
  = Bin sx kx (f kx x) (mapWithKey f l) (mapWithKey f r)

mapAccum :: (a -> b -> (a, c)) -> a -> Map k b -> (a, Map k c)
mapAccum f a m = mapAccumWithKey (\ a' _ x' -> f a' x') a m

mapAccumWithKey :: (a -> k -> b -> (a, c)) -> a -> Map k b -> (a, Map k c)
mapAccumWithKey f a t = mapAccumL f a t

mapAccumL :: (a -> k -> b -> (a, c)) -> a -> Map k b -> (a, Map k c)
mapAccumL f a t
  = case t of
      Tip -> (a, Tip)
      Bin sx kx x l r -> let (a1, l') = mapAccumL f a l
                             (a2, x') = f a1 kx x
                             (a3, r') = mapAccumL f a2 r
                           in (a3, Bin sx kx x' l' r')

mapAccumRWithKey :: (a -> k -> b -> (a, c)) -> a -> Map k b -> (a, Map k c)
mapAccumRWithKey f a t
  = case t of
      Tip -> (a, Tip)
      Bin sx kx x l r -> let (a1, r') = mapAccumRWithKey f a r
                             (a2, x') = f a1 kx x
                             (a3, l') = mapAccumRWithKey f a2 l
                           in (a3, Bin sx kx x' l' r')

mapKeys :: (Ord k2) => (k1 -> k2) -> Map k1 a -> Map k2 a
mapKeys = mapKeysWith (\ x _ -> x)

mapKeysWith :: (Ord k2) => (a -> a -> a) -> (k1 -> k2) -> Map k1 a -> Map k2 a
mapKeysWith c f = fromListWith c . List.map fFirst . toList
  where fFirst (x, y) = (f x, y)

mapKeysMonotonic :: (k1 -> k2) -> Map k1 a -> Map k2 a
mapKeysMonotonic _ (Tip) = Tip
mapKeysMonotonic f (Bin sz k x l r)
  = Bin sz (f k) x (mapKeysMonotonic f l) (mapKeysMonotonic f r)

-----------------------------------------------------------------------------

fold :: (a -> b -> b) -> b -> Map k a -> b
fold f z m = foldWithKey (\ _ x' z' -> f x' z') z m

foldWithKey :: (k -> a -> b -> b) -> b -> Map k a -> b
foldWithKey f z t = foldrWithKey f z t

foldrWithKey :: (k -> a -> b -> b) -> b -> Map k a -> b
foldrWithKey _ z (Tip) = z
foldrWithKey f z (Bin _ kx x l r)
  = foldrWithKey f (f kx x (foldrWithKey f z r)) l

foldlWithKey :: (b -> k -> a -> b) -> b -> Map k a -> b
foldlWithKey _ z (Tip) = z
foldlWithKey f z (Bin _ kx x l r)
  = foldlWithKey f (f (foldlWithKey f z l) kx x) r

-----------------------------------------------------------------------------

elems :: Map k a -> [a]
elems m = [x | (_, x) <- assocs m]

keys :: Map k a -> [k]
keys m = [k | (k, _) <- assocs m]

keysSet :: Map k a -> Set.Set k
keysSet m = Set.fromDistinctAscList (keys m)

assocs :: Map k a -> [(k, a)]
assocs m = toList m

fromList :: (Ord k) => [(k, a)] -> Map k a
fromList xs = foldlStrict ins empty xs
  where ins t (k, x) = insert k x t

fromListWith :: (Ord k) => (a -> a -> a) -> [(k, a)] -> Map k a
fromListWith f xs = fromListWithKey (\ _ x y -> f x y) xs

fromListWithKey :: (Ord k) => (k -> a -> a -> a) -> [(k, a)] -> Map k a
fromListWithKey f xs = foldlStrict ins empty xs
  where ins t (k, x) = insertWithKey f k x t

toList :: Map k a -> [(k, a)]
toList t = toAscList t

toAscList :: Map k a -> [(k, a)]
toAscList t = foldrWithKey (\ k x xs -> (k, x) : xs) [] t

toDescList :: Map k a -> [(k, a)]
toDescList t = foldlWithKey (\ xs k x -> (k, x) : xs) [] t

fromAscList :: (Eq k) => [(k, a)] -> Map k a
fromAscList xs = fromAscListWithKey (\ _ x _ -> x) xs

fromAscListWith :: (Eq k) => (a -> a -> a) -> [(k, a)] -> Map k a
fromAscListWith f xs = fromAscListWithKey (\ _ x y -> f x y) xs

fromAscListWithKey :: (Eq k) => (k -> a -> a -> a) -> [(k, a)] -> Map k a
fromAscListWithKey f xs = fromDistinctAscList (combineEq f xs)
  where combineEq _ xs'
          = case xs' of
              [] -> []
              [x] -> [x]
              (x : xx) -> combineEq' x xx
        combineEq' z [] = [z]
        combineEq' z@(kz, zz) (x@(kx, xx) : xs')
          | kx == kz = let yy = f kx xx zz in combineEq' (kx, yy) xs'
          | otherwise = z : combineEq' x xs'

fromDistinctAscList :: [(k, a)] -> Map k a
fromDistinctAscList xs = build const (length xs) xs
  where build c 0 xs' = c Tip xs'
        build c 5 xs'
          = case xs' of
              ((k1, x1) : (k2, x2) : (k3, x3) : (k4, x4) : (k5, x5) : xx) -> c
                                                                               (bin k4 x4
                                                                                  (bin k2 x2
                                                                                     (singleton k1
                                                                                        x1)
                                                                                     (singleton k3
                                                                                        x3))
                                                                                  (singleton k5 x5))
                                                                               xx
              _ -> error "fromDistinctAscList build"
        build c n xs' = seq nr $ build (buildR nr c) nl xs'
          where nl = n `div` 2
                nr = n - nl - 1
        buildR n c l ((k, x) : ys) = build (buildB l k x c) n ys
        buildR _ _ _ [] = error "fromDistinctAscList buildR []"
        buildB l k x c r zs = c (bin k x l r) zs

-----------------------------------------------------------------------------

trim :: (k -> Ordering) -> (k -> Ordering) -> Map k a -> Map k a
trim _ _ (Tip) = Tip
trim cmplo cmphi t@(Bin _ kx _ l r)
  = case cmplo kx of
      LT -> case cmphi kx of
              GT -> t
              _ -> trim cmplo cmphi l
      _ -> trim cmplo cmphi r

trimLookupLo :: (Ord k) => k -> (k -> Ordering) -> Map k a -> (Maybe (k, a), Map k a)
trimLookupLo _ _ (Tip) = (Nothing, Tip)
trimLookupLo lo cmphi t@(Bin _ kx x l r)
  = case compare lo kx of
      LT -> case cmphi kx of
              GT -> (lookupAssoc lo t, t)
              _ -> trimLookupLo lo cmphi l
      GT -> trimLookupLo lo cmphi r
      EQ -> (Just (kx, x), trim (compare lo) cmphi r)

-----------------------------------------------------------------------------

filterGt :: (Ord k) => (k -> Ordering) -> Map k a -> Map k a
filterGt _ (Tip) = Tip
filterGt cmp (Bin _ kx x l r)
  = case cmp kx of
      LT -> join kx x (filterGt cmp l) r
      GT -> filterGt cmp r
      EQ -> r

filterLt :: (Ord k) => (k -> Ordering) -> Map k a -> Map k a
filterLt _ (Tip) = Tip
filterLt cmp (Bin _ kx x l r)
  = case cmp kx of
      LT -> filterLt cmp l
      GT -> join kx x l (filterLt cmp r)
      EQ -> l

-----------------------------------------------------------------------------

split :: (Ord k) => k -> Map k a -> (Map k a, Map k a)
split _ (Tip) = (Tip, Tip)
split k (Bin _ kx x l r)
  = case compare k kx of
      LT -> let (lt, gt) = split k l in (lt, join kx x gt r)
      GT -> let (lt, gt) = split k r in (join kx x l lt, gt)
      EQ -> (l, r)

splitLookup :: (Ord k) => k -> Map k a -> (Map k a, Maybe a, Map k a)
splitLookup _ (Tip) = (Tip, Nothing, Tip)
splitLookup k (Bin _ kx x l r)
  = case compare k kx of
      LT -> let (lt, z, gt) = splitLookup k l in (lt, z, join kx x gt r)
      GT -> let (lt, z, gt) = splitLookup k r in (join kx x l lt, z, gt)
      EQ -> (l, Just x, r)

splitLookupWithKey :: (Ord k) => k -> Map k a -> (Map k a, Maybe (k, a), Map k a)
splitLookupWithKey _ (Tip) = (Tip, Nothing, Tip)
splitLookupWithKey k (Bin _ kx x l r)
  = case compare k kx of
      LT -> let (lt, z, gt) = splitLookupWithKey k l in
              (lt, z, join kx x gt r)
      GT -> let (lt, z, gt) = splitLookupWithKey k r in
              (join kx x l lt, z, gt)
      EQ -> (l, Just (kx, x), r)

-----------------------------------------------------------------------------

insertMax :: k -> a -> Map k a -> Map k a
insertMax kx x t
  = case t of
      Tip -> singleton kx x
      Bin _ ky y l r -> balance ky y l (insertMax kx x r)

insertMin :: k -> a -> Map k a -> Map k a
insertMin kx x t
  = case t of
      Tip -> singleton kx x
      Bin _ ky y l r -> balance ky y (insertMin kx x l) r

-----------------------------------------------------------------------------

join :: (Ord k) => k -> a -> Map k a -> Map k a -> Map k a
join kx x (Tip) r = insertMin kx x r
join kx x l (Tip) = insertMax kx x l
join kx x l@(Bin sizeL ky y ly ry) r@(Bin sizeR kz z lz rz)
  | delta * sizeL <= sizeR = balance kz z (join kx x l lz) rz
  | delta * sizeR <= sizeL = balance ky y ly (join kx x ry r)
  | otherwise = bin kx x l r

merge :: Map k a -> Map k a -> Map k a
merge (Tip) r = r
merge l (Tip) = l
merge l@(Bin sizeL kx x lx rx) r@(Bin sizeR ky y ly ry)
  | delta * sizeL <= sizeR = balance ky y (merge l ly) ry
  | delta * sizeR <= sizeL = balance kx x lx (merge rx r)
  | otherwise = glue l r

-----------------------------------------------------------------------------

glue :: Map k a -> Map k a -> Map k a
glue (Tip) r = r
glue l (Tip) = l
glue l r
  | size l > size r =
    let ((km, m), l') = deleteFindMax l in balance km m l' r
  | otherwise =
    let ((km, m), r') = deleteFindMin r in balance km m l r'

deleteFindMin :: Map k a -> ((k, a), Map k a)
deleteFindMin t
  = case t of
      Bin _ k x Tip r -> ((k, x), r)
      Bin _ k x l r -> let (km, l') = deleteFindMin l in
                         (km, balance k x l' r)
      Tip -> (error
                "Map.deleteFindMin: can not return the minimal element of an empty map",
              Tip)

deleteFindMax :: Map k a -> ((k, a), Map k a)
deleteFindMax t
  = case t of
      Bin _ k x l Tip -> ((k, x), l)
      Bin _ k x l r -> let (km, r') = deleteFindMax r in
                         (km, balance k x l r')
      Tip -> (error
                "Map.deleteFindMax: can not return the maximal element of an empty map",
              Tip)

-----------------------------------------------------------------------------

instance Functor (Map k) where
  fmap f m = map f m

instance (Eq k, Eq a) => Eq (Map k a) where
  t1 == t2 = (size t1 == size t2) && (toAscList t1 == toAscList t2)

instance (Ord k, Ord v) => Ord (Map k v) where
  compare m1 m2 = compare (toAscList m1) (toAscList m2)

-----------------------------------------------------------------------------

#if 0
instance (Ord k, Read k, Read e) => Read (Map k e) where
  readsPrec p
    = readParen (p > 10) $
        \ r ->
          do ("fromList", s) <- lex r
             (xs, t) <- reads s
             return (fromList xs, t)

instance (Show k, Show a) => Show (Map k a) where
  showsPrec d m
    = showParen (d > 10) $ showString "fromList " . shows (toList m)
#endif

instance (Show k, Show a) => Show (Map k a) where
  showsPrec _ Tip = showString "mempty"
  showsPrec _ o   = shows (toList o)

instance (Ord k, Read k, Read e) => Read (Map k e) where
  readPrec = (parens . choice)
    [do Ident "mempty" <- lexP; return mempty
    ,do xs <- readPrec; return (fromList xs)]
  readListPrec = readListPrecDefault

instance (Show k) => Show (Map k ()) where
  showsPrec _ Tip = showString "mempty"
  showsPrec _ o   = shows (fmap fst (toList o))

instance (Ord k, Read k) => Read (Map k ()) where
  readPrec = (parens . choice)
    [do Ident "mempty" <- lexP; return mempty
    ,do xs <- readPrec; return (fromList (zip xs (repeat ())))]
  readListPrec = readListPrecDefault

-----------------------------------------------------------------------------

showTree :: (Show k, Show a) => Map k a -> String
showTree m = showTreeWith showElem True False m
  where showElem k x = show k ++ ":=" ++ show x
showTreeWith :: (k -> a -> String) -> Bool -> Bool -> Map k a -> String
showTreeWith showelem hang wide t
  | hang = (showsTreeHang showelem wide [] t) ""
  | otherwise = (showsTree showelem wide [] [] t) ""
showsTree :: (k -> a -> String) -> Bool -> [String] -> [String] -> Map k a -> ShowS
showsTree showelem wide lbars rbars t
  = case t of
      Tip -> showsBars lbars . showString "|\n"
      Bin _ kx x Tip Tip -> showsBars lbars . showString (showelem kx x)
                              . showString "\n"
      Bin _ kx x l r -> showsTree showelem wide (withBar rbars)
                          (withEmpty rbars)
                          r
                          . showWide wide rbars
                          . showsBars lbars
                          . showString (showelem kx x)
                          . showString "\n"
                          . showWide wide lbars
                          . showsTree showelem wide (withEmpty lbars) (withBar lbars) l
showsTreeHang :: (k -> a -> String) -> Bool -> [String] -> Map k a -> ShowS
showsTreeHang showelem wide bars t
  = case t of
      Tip -> showsBars bars . showString "|\n"
      Bin _ kx x Tip Tip -> showsBars bars . showString (showelem kx x) .
                              showString "\n"
      Bin _ kx x l r -> showsBars bars . showString (showelem kx x) .
                          showString "\n"
                          . showWide wide bars
                          . showsTreeHang showelem wide (withBar bars) l
                          . showWide wide bars
                          . showsTreeHang showelem wide (withEmpty bars) r
showWide :: Bool -> [String] -> String -> String
showWide wide bars
  | wide = showString (concat (reverse bars)) . showString "|\n"
  | otherwise = id
showsBars :: [String] -> ShowS
showsBars bars
  = case bars of
      [] -> id
      _ -> showString (concat (reverse (tail bars))) . showString node
node :: String
node = "+--"
withBar, withEmpty :: [String] -> [String]
withBar bars = "|  " : bars
withEmpty bars = "   " : bars

-----------------------------------------------------------------------------

valid :: (Ord k) => Map k a -> Bool
valid t = balanced t && ordered t && validsize t

validsize :: Map a b -> Bool
validsize t = (realsize t == Just (size t))
  where realsize t'
          = case t' of
              Tip -> Just 0
              Bin sz _ _ l r -> case (realsize l, realsize r) of
                                  (Just n, Just m) | n + m + 1 == sz -> Just sz
                                  _ -> Nothing

ordered :: (Ord a) => Map a b -> Bool
ordered t = bounded (const True) (const True) t
  where bounded lo hi t'
          = case t' of
              Tip -> True
              Bin _ kx _ l r -> (lo kx) && (hi kx) && bounded lo (< kx) l &&
                                  bounded (> kx) hi r

balanced :: Map k a -> Bool
balanced t
  = case t of
      Tip -> True
      Bin _ _ _ l r -> (size l + size r <= 1 ||
                          (size l <= delta * size r && size r <= delta * size l))
                         && balanced l
                         && balanced r

-----------------------------------------------------------------------------
