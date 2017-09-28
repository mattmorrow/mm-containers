{-# LANGUAGE CPP #-}

-- (c) Daan Leijen 2002
-- BSD3

module MM.Data.Set.Int (
   IntSet
  ,mapM_,foldM
  ,null,size,member,notMember,isSubsetOf
  ,isProperSubsetOf,empty,singleton,insert,delete
  ,union,unions,difference,intersection,intersections
  ,filter,partition,split,splitMember
  ,minWithDefault,maxWithDefault,findMin
  ,findMax,deleteMin,deleteMax,deleteFindMin
  ,deleteFindMax,maxView,minView,map,fold,foldLazy
  ,elems,toList,fromList,toAscList,fromAscList
  ,fromDistinctAscList,showTree,showTreeWith
) where

{-
  IntSet, (\\), null, size, member, notMember, isSubsetOf,
  isProperSubsetOf, empty, singleton, insert, delete, union, unions,
  difference, intersection, filter, partition, split, splitMember,
  findMin, findMax, deleteMin, deleteMax, deleteFindMin,
  deleteFindMax, maxView, minView, map, fold, elems, toList,
  fromList, toAscList, fromAscList, fromDistinctAscList, showTree,
  showTreeWith
-}

import Data.Binary
import Prelude hiding (lookup,filter,foldr,foldl,null,map,mapM_)
import Data.Bits
import qualified Data.List as List
import Data.Monoid (Monoid(..))
import Data.Maybe (fromMaybe)
import Control.Applicative((<$>),(<*>))
import Text.Read hiding(get)
import Data.Word

-----------------------------------------------------------------------------

-- | .
foldM :: (Monad m) => (Int -> b -> m b) -> b -> IntSet -> m b
{-# INLINE foldM #-}
foldM f !z t = do
  case t of
    Binary 0 m l r | m < 0-> do
      !z <- go z l
      go z r
    Binary{}-> go z t
    Tip x-> f x z
    Nil-> return z
  where go z Nil = return z
        go z (Tip x) = f x z
        go z (Binary _ _ l r) = do
          !z <- go z r
          go z l

mapM_ :: (Monad m) => (Int -> m b) -> IntSet -> m ()
{-# INLINE mapM_ #-}
mapM_ f t = do
  case t of
    Binary 0 m l r
      | m < 0-> go l >> go r
    Binary{}-> go t
    Tip x-> f x >> return ()
    Nil-> return ()
  where go Nil = return ()
        go (Tip x) = f x >> return ()
        go (Binary _ _ l r) = go r >> go l

-----------------------------------------------------------------------------

-- A "Nat" is a natural machine word (an unsigned Int)
type Nat = Word

natFromInt :: Int -> Nat
natFromInt i = fromIntegral i

intFromNat :: Nat -> Int
intFromNat w = fromIntegral w

data IntSet = Binary {-# UNPACK #-} !Prefix {-# UNPACK #-} !Mask !IntSet !IntSet
            | Tip {-# UNPACK #-} !Int
            | Nil
-- Invariant: Nil is never found as a child of Binary.
-- Invariant: The Mask is a power of 2.  It is the largest bit position at which
--            two elements of the set differ.
-- Invariant: Prefix is the common high-order bits that all elements share to
--            the left of the Mask bit.
-- Invariant: In Binary prefix mask left right, left consists of the elements that
--            don't have the mask bit set; right is all the elements that do.

type Prefix = Int
type Mask   = Int

instance Monoid IntSet where
    mempty  = empty
    mappend = union
    mconcat = unions
instance Binary IntSet where
  put Nil = putWord8 0
  put (Tip k) = putWord8 1 >> put k
  put (Binary p m l r) = putWord8 2 >> put p >> put m >> put l >> put r
  get = do tag <- getWord8
           case tag of
            0 -> return Nil
            1 -> Tip <$> get
            _ -> Binary <$> get <*> get <*> get <*> get

-----------------------------------------------------------------------------

join :: Prefix -> IntSet -> Prefix -> IntSet -> IntSet
join p1 t1 p2 t2
  | !m <- branchMask p1 p2
  , !p <- mask p1 m
  = case zero p1 m of
      True-> Binary p m t1 t2
      False-> Binary p m t2 t1

bin :: Prefix -> Mask -> IntSet -> IntSet -> IntSet
bin p m l r
  | Nil <- r  = l
  | Nil <- l  = r
  | otherwise = Binary p m l r

-----------------------------------------------------------------------------

-- endian independent
zero :: Int -> Mask -> Bool
zero i m = (natFromInt i) .&. (natFromInt m) == 0

-- endian independent
nomatch :: Int -> Prefix -> Mask -> Bool
nomatch i p m = (mask i m) /= p

-- endian independent
match :: Int -> Prefix -> Mask -> Bool
match i p m = (mask i m) == p

-- endian independent
-- Suppose a is largest such that 2^a divides 2*m.
-- Then mask i m is i with the low a bits zeroed out.
mask :: Int -> Mask -> Prefix
mask i m = maskW (natFromInt i) (natFromInt m)

-- endian independent
zeroN :: Nat -> Nat -> Bool
zeroN i m = (i .&. m) == 0

-----------------------------------------------------------------------------

-- big endian
maskW :: Nat -> Nat -> Prefix
maskW i m
  | !n <- complement (m-1)
  , !l <- n`xor`m
  , !p <- i .&. l
  = intFromNat p

-- big endian
shorter :: Mask -> Mask -> Bool
shorter m1 m2 = natFromInt m2 < natFromInt m1

-- big endian
branchMask :: Prefix -> Prefix -> Mask
branchMask p1 p2
  | !n1  <- natFromInt p1
  , !n2  <- natFromInt p2
  , !n   <- n1 `xor` n2
  , !m   <- highestBitMask n
  = intFromNat m

-- Returns a word where only the highest bit is set.
-- It is found by first setting all bits in lower positions than the
-- highest bit and than taking an exclusive or with the original value.
highestBitMask :: Nat -> Nat
highestBitMask x
  | !x <- x .|. shiftR x 1
  , !x <- x .|. shiftR x 2
  , !x <- x .|. shiftR x 4
  , !x <- x .|. shiftR x 8
  , !x <- x .|. shiftR x 16
  , !x <- x .|. shiftR x 32
  = x `xor` shiftR x 1

-----------------------------------------------------------------------------

foldlStrict :: (a -> b -> a) -> a -> [b] -> a
foldlStrict _ z [] = z
foldlStrict f z (x:xs)
  | !z <- f z x
  = foldlStrict f z xs

-----------------------------------------------------------------------------

-- | /O(1)/. The empty set.
empty :: IntSet
empty = Nil

-- | /O(1)/
null :: IntSet -> Bool
null Nil = True
null _   = False

-- | /O(n)/
size :: IntSet -> Int
size t
  = case t of
      Binary _ _ l r -> size l + size r
      Tip _ -> 1
      Nil   -> 0

-----------------------------------------------------------------------------

-- | /O(min(n,W))/
member :: Int -> IntSet -> Bool
member x t
  = case t of
      Binary p m l r
        | nomatch x p m -> False
        | zero x m      -> member x l
        | otherwise     -> member x r
      Tip y -> (x==y)
      Nil   -> False

-- | /O(min(n,W))/.
notMember :: Int -> IntSet -> Bool
notMember k = not . member k

-----------------------------------------------------------------------------

-- 'lookup' is used by 'intersection' for left-biasing
lookup :: Int -> IntSet -> Maybe Int
lookup k t
  | !nk <- natFromInt k
  = lookupN nk t

lookupN :: Nat -> IntSet -> Maybe Int
lookupN k t
  = case t of
      Binary _ m l r
        | zeroN k (natFromInt m) -> lookupN k l
        | otherwise              -> lookupN k r
      Tip kx
        | (k == natFromInt kx)  -> Just kx
        | otherwise             -> Nothing
      Nil -> Nothing

-----------------------------------------------------------------------------

-- | /O(1)/. A set of one element.
singleton :: Int -> IntSet
singleton x = Tip x

-- | /O(min(n,W))/.
insert :: Int -> IntSet -> IntSet
insert x t
  = case t of
      Binary p m l r
        | nomatch x p m -> join x (Tip x) p t
        | zero x m      -> Binary p m (insert x l) r
        | otherwise     -> Binary p m l (insert x r)
      Tip y
        | x==y          -> Tip x
        | otherwise     -> join x (Tip x) y t
      Nil -> Tip x

-- right-biased insertion, used by 'union'
insertR :: Int -> IntSet -> IntSet
insertR x t
  = case t of
      Binary p m l r
        | nomatch x p m -> join x (Tip x) p t
        | zero x m      -> Binary p m (insert x l) r
        | otherwise     -> Binary p m l (insert x r)
      Tip y
        | x==y          -> t
        | otherwise     -> join x (Tip x) y t
      Nil -> Tip x

-- | /O(min(n,W))/.
delete :: Int -> IntSet -> IntSet
delete x t
  = case t of
      Binary p m l r
        | nomatch x p m -> t
        | zero x m      -> bin p m (delete x l) r
        | otherwise     -> bin p m l (delete x r)
      Tip y
        | x==y          -> Nil
        | otherwise     -> t
      Nil -> Nil

-----------------------------------------------------------------------------

unions :: [IntSet] -> IntSet
unions xs = foldlStrict union empty xs

-- | /O(n+m)/.
union :: IntSet -> IntSet -> IntSet
union t1@(Binary p1 m1 l1 r1) t2@(Binary p2 m2 l2 r2)
  | shorter m1 m2 = case () of
      _   | nomatch p2 p1 m1      -> join p1 t1 p2 t2
          | zero p2 m1
          , !l <- union l1 t2     -> Binary p1 m1 l r1
          | !r <- union r1 t2     -> Binary p1 m1 l1 r
  | shorter m2 m1 = case () of
      _ | nomatch p1 p2 m2        -> join p1 t1 p2 t2
        | zero p1 m2
        , !l <- union t1 l2       -> Binary p2 m2 l r2
        | !r <- union t1 r2       -> Binary p2 m2 l2 r
  | p1==p2
  , !l <- union l1 l2
  , !r <- union r1 r2             = Binary p1 m1 l r
  | otherwise                     = join p1 t1 p2 t2
union (Tip k) t                   = insert k t
union t (Tip k)                   = insertR k t
union Nil t                       = t
union t Nil                       = t

-----------------------------------------------------------------------------

-- | /O(n+m)/.
difference :: IntSet -> IntSet -> IntSet
difference t1@(Binary p1 m1 l1 r1) t2@(Binary p2 m2 l2 r2)
  | shorter m1 m2 = case () of
      _ | nomatch p2 p1 m1        -> t1
        | zero p2 m1
        , !l <- difference l1 t2  -> bin p1 m1 l r1
        | !r <- difference r1 t2  -> bin p1 m1 l1 r
  | shorter m2 m1 = case () of
      _ | nomatch p1 p2 m2        -> t1
        | zero p1 m2              -> difference t1 l2
        | otherwise               -> difference t1 r2
  | p1==p2
  , !l <- difference l1 l2
  , !r <- difference r1 r2        = bin p1 m1 l r
  | otherwise                     = t1
difference t1@(Tip k) t2
  | member k t2                   = Nil
  | otherwise                     = t1
difference Nil _                  = Nil
difference t (Tip k)              = delete k t
difference t Nil                  = t

-----------------------------------------------------------------------------

intersections :: [IntSet] -> IntSet
intersections [] = empty
intersections (x:xs) = foldlStrict intersection x xs

-- | /O(n+m)/.
intersection :: IntSet -> IntSet -> IntSet
intersection t1@(Binary p1 m1 l1 r1) t2@(Binary p2 m2 l2 r2)
  | shorter m1 m2 = case () of
      _ | nomatch p2 p1 m1      -> Nil
        | zero p2 m1            -> intersection l1 t2
        | otherwise             -> intersection r1 t2
  | shorter m2 m1 = case () of
      _ | nomatch p1 p2 m2      -> Nil
        | zero p1 m2            -> intersection t1 l2
        | otherwise             -> intersection t1 r2
  | p1==p2
  , !l <- intersection l1 l2
  , !r <- intersection r1 r2    = bin p1 m1 l r
  | otherwise                   = Nil
intersection t1@(Tip k) t2
  | member k t2                 = t1
  | otherwise                   = Nil
intersection t (Tip k)
  | Just y <- lookup k t        = Tip y
  | otherwise                   = Nil
intersection Nil _              = Nil
intersection _ Nil              = Nil

-----------------------------------------------------------------------------

-- | /O(n+m)/.
isProperSubsetOf :: IntSet -> IntSet -> Bool
isProperSubsetOf t1 t2
  = case subsetCmp t1 t2 of
      LT -> True
      _  -> False

subsetCmp :: IntSet -> IntSet -> Ordering
subsetCmp t1@(Binary p1 m1 l1 r1) (Binary p2 m2 l2 r2)
  | shorter m1 m2  = GT
  | shorter m2 m1  = case subsetCmpLt of
                       GT -> GT
                       _ -> LT
  | p1 == p2       = subsetCmpEq
  | otherwise      = GT  -- disjoint
  where
    subsetCmpLt | nomatch p1 p2 m2  = GT
                | zero p1 m2        = subsetCmp t1 l2
                | otherwise         = subsetCmp t1 r2
    subsetCmpEq = case (subsetCmp l1 l2, subsetCmp r1 r2) of
                    (GT,_ ) -> GT
                    (_ ,GT) -> GT
                    (EQ,EQ) -> EQ
                    _       -> LT

subsetCmp (Binary _ _ _ _) _  = GT
subsetCmp (Tip x) (Tip y)
  | x==y       = EQ
  | otherwise  = GT  -- disjoint
subsetCmp (Tip x) t
  | member x t = LT
  | otherwise  = GT  -- disjoint
subsetCmp Nil Nil = EQ
subsetCmp Nil _   = LT

-- | /O(n+m)/.
isSubsetOf :: IntSet -> IntSet -> Bool
isSubsetOf t1@(Binary p1 m1 l1 r1) (Binary p2 m2 l2 r2)
  | shorter m1 m2  = False
  | shorter m2 m1  = match p1 p2 m2 && (if zero p1 m2 then isSubsetOf t1 l2
                                                      else isSubsetOf t1 r2)
  | otherwise      = (p1==p2) && isSubsetOf l1 l2 && isSubsetOf r1 r2
isSubsetOf (Binary _ _ _ _) _  = False
isSubsetOf (Tip x) t        = member x t
isSubsetOf Nil _            = True

-----------------------------------------------------------------------------

-- | /O(n)/.
filter :: (Int -> Bool) -> IntSet -> IntSet
filter predicate t
  = case t of
      Binary p m l r
        -> bin p m (filter predicate l) (filter predicate r)
      Tip x
        | predicate x -> t
        | otherwise   -> Nil
      Nil -> Nil

-- | /O(n)/.
partition :: (Int -> Bool) -> IntSet -> (IntSet,IntSet)
partition predicate t
  = case t of
      Binary p m l r
        -> let (l1,l2) = partition predicate l
               (r1,r2) = partition predicate r
           in (bin p m l1 r1, bin p m l2 r2)
      Tip x
        | predicate x -> (t,Nil)
        | otherwise   -> (Nil,t)
      Nil -> (Nil,Nil)

-- | /O(min(n,W))/. The expression (@'split' x set@) is a pair @(set1,set2)@
-- where @set1@ comprises the elements of @set@ less than @x@ and @set2@
-- comprises the elements of @set@ greater than @x@.
--
-- > split 3 (fromList [1..5]) == (fromList [1,2], fromList [4,5])
split :: Int -> IntSet -> (IntSet,IntSet)
split x t
  = case t of
      Binary _ m l r
        | m < 0       -> if x >= 0 then let (lt,gt) = split' x l
                                        in (union r lt, gt)
                                   else let (lt,gt) = split' x r
                                        in (lt, union gt l)
                                   -- handle negative numbers.
        | otherwise   -> split' x t
      Tip y
        | x>y         -> (t,Nil)
        | x<y         -> (Nil,t)
        | otherwise   -> (Nil,Nil)
      Nil             -> (Nil, Nil)

split' :: Int -> IntSet -> (IntSet,IntSet)
split' x t
  = case t of
      Binary p m l r
        | match x p m -> if zero x m then let (lt,gt) = split' x l
                                          in (lt,union gt r)
                                     else let (lt,gt) = split' x r
                                          in (union l lt,gt)
        | otherwise   -> if x < p then (Nil, t)
                                  else (t, Nil)
      Tip y
        | x>y       -> (t,Nil)
        | x<y       -> (Nil,t)
        | otherwise -> (Nil,Nil)
      Nil -> (Nil,Nil)

-- | /O(min(n,W))/. Performs a 'split' but also returns whether the pivot
-- element was found in the original set.
splitMember :: Int -> IntSet -> (IntSet,Bool,IntSet)
splitMember x t
  = case t of
      Binary _ m l r
        | m < 0       -> if x >= 0 then let (lt,found,gt) = splitMember' x l
                                        in (union r lt, found, gt)
                                   else let (lt,found,gt) = splitMember' x r
                                        in (lt, found, union gt l)
                                   -- handle negative numbers.
        | otherwise   -> splitMember' x t
      Tip y
        | x>y       -> (t,False,Nil)
        | x<y       -> (Nil,False,t)
        | otherwise -> (Nil,True,Nil)
      Nil -> (Nil,False,Nil)

splitMember' :: Int -> IntSet -> (IntSet,Bool,IntSet)
splitMember' x t
  = case t of
      Binary p m l r
         | match x p m ->  if zero x m then let (lt,found,gt) = splitMember x l
                                            in (lt,found,union gt r)
                                       else let (lt,found,gt) = splitMember x r
                                            in (union l lt,found,gt)
         | otherwise   -> if x < p then (Nil, False, t)
                                   else (t, False, Nil)
      Tip y
        | x>y       -> (t,False,Nil)
        | x<y       -> (Nil,False,t)
        | otherwise -> (Nil,True,Nil)
      Nil -> (Nil,False,Nil)

-----------------------------------------------------------------------------

-- | /O(min(n,W))/. Retrieves the maximal key of the set, and the set
-- stripped of that element, or 'Nothing' if passed an empty set.
maxView :: IntSet -> Maybe (Int, IntSet)
maxView t
    = case t of
        Binary p m l r | m < 0 -> let (result,t') = maxViewUnsigned l
                                in Just (result, bin p m t' r)
        Binary p m l r         -> let (result,t') = maxViewUnsigned r
                                in Just (result, bin p m l t')
        Tip y -> Just (y,Nil)
        Nil -> Nothing

maxViewUnsigned :: IntSet -> (Int, IntSet)
maxViewUnsigned t
    = case t of
        Binary p m l r -> let (result,t') = maxViewUnsigned r
                        in (result, bin p m l t')
        Tip y -> (y, Nil)
        Nil -> error "maxViewUnsigned Nil"

-- | /O(min(n,W))/. Retrieves the minimal key of the set, and the set
-- stripped of that element, or 'Nothing' if passed an empty set.
minView :: IntSet -> Maybe (Int, IntSet)
minView t
    = case t of
        Binary p m l r | m < 0 -> let (result,t') = minViewUnsigned r
                                in Just (result, bin p m l t')
        Binary p m l r         -> let (result,t') = minViewUnsigned l
                                in Just (result, bin p m t' r)
        Tip y -> Just (y, Nil)
        Nil -> Nothing

minViewUnsigned :: IntSet -> (Int, IntSet)
minViewUnsigned t
    = case t of
        Binary p m l r -> let (result,t') = minViewUnsigned l
                        in (result, bin p m t' r)
        Tip y -> (y, Nil)
        Nil -> error "minViewUnsigned Nil"

-- | /O(min(n,W))/. Delete and find the minimal element.
--
-- > deleteFindMin set = (findMin set, deleteMin set)
deleteFindMin :: IntSet -> (Int, IntSet)
deleteFindMin = fromMaybe
  (error "deleteFindMin: empty set has no minimal element") . minView

-- | /O(min(n,W))/. Delete and find the maximal element.
--
-- > deleteFindMax set = (findMax set, deleteMax set)
deleteFindMax :: IntSet -> (Int, IntSet)
deleteFindMax = fromMaybe
  (error "deleteFindMax: empty set has no maximal element") . maxView

-- | /O(min(n,W))/. The minimal element of the set.
findMin :: IntSet -> Int
findMin Nil = error "findMin: empty set has no minimal element"
findMin (Tip x) = x
findMin (Binary _ m l r)
  |   m < 0   = find r
  | otherwise = find l
    where find (Tip x)        = x
          find (Binary _ _ l' _) = find l'
          find Nil            = error "findMin Nil"

-- | /O(min(n,W))/. The maximal element of a set.
findMax :: IntSet -> Int
findMax Nil = error "findMax: empty set has no maximal element"
findMax (Tip x) = x
findMax (Binary _ m l r)
  |   m < 0   = find l
  | otherwise = find r
    where find (Tip x)        = x
          find (Binary _ _ _ r') = find r'
          find Nil            = error "findMax Nil"

minWithDefault :: Int -> IntSet -> Int
minWithDefault dflt Nil = dflt
minWithDefault _ (Tip k) = k
minWithDefault _ (Binary _ m l r)
  | m < 0 = go r
  | otherwise = go l
  where go (Tip k) = k
        go (Binary _ _ l _) = go l
        go Nil = error "minWithDefault Nil"

maxWithDefault :: Int -> IntSet -> Int
maxWithDefault dflt Nil = dflt
maxWithDefault _ (Tip k) = k
maxWithDefault _ (Binary _ m l r)
  | m < 0 = go l
  | otherwise = go r
  where go (Tip k) = k
        go (Binary _ _ _ r) = go r
        go Nil = error "maxWithDefault Nil"

-- | /O(min(n,W))/. Delete the minimal element.
deleteMin :: IntSet -> IntSet
deleteMin = maybe
  (error "deleteMin: empty set has no minimal element") snd . minView

-- | /O(min(n,W))/. Delete the maximal element.
deleteMax :: IntSet -> IntSet
deleteMax = maybe
  (error "deleteMax: empty set has no maximal element") snd . maxView

-----------------------------------------------------------------------------

-- | /O(n*min(n,W))/.
map :: (Int->Int) -> IntSet -> IntSet
map f = fromList . List.map f . toList

-- | /O(n)/.
fold :: (Int -> b -> b) -> b -> IntSet -> b
fold f z t
  = case t of
      Binary 0 m l r
        | m < 0
        , !z <- foldl' f z l
        -> foldl' f z r -- foldr f (foldr f z l) r
      -- put negative numbers before.
      Binary _ _ _ _ -> foldl' f z t
      Tip x       -> f x z
      Nil         -> z

foldLazy :: (Int -> b -> b) -> b -> IntSet -> b
foldLazy f z t
  = case t of
      Binary 0 m l r | m < 0 -> foldr f (foldr f z l) r
      -- put negative numbers before.
      Binary _ _ _ _ -> foldr f z t
      Tip x       -> f x z
      Nil         -> z

foldr :: (Int -> b -> b) -> b -> IntSet -> b
foldr f z t
  = case t of
      Binary _ _ l r -> foldr f (foldr f z r) l
      Tip x       -> f x z
      Nil         -> z

foldl' :: (Int -> b -> b) -> b -> IntSet -> b
foldl' f z t
  | Binary _ _ l r <- t
  , !z <- foldl' f z r
  = foldl' f z l
  | Tip x <- t
  = f x z
  | Nil <- t
  = z

-----------------------------------------------------------------------------

-- | /O(n)/.
elems :: IntSet -> [Int]
elems s = toList s

-- | /O(n)/.
toList :: IntSet -> [Int]
toList t = foldLazy (:) [] t

-- | /O(n)/.
toAscList :: IntSet -> [Int]
toAscList t = toList t

-- | /O(n*min(n,W))/.
fromList :: [Int] -> IntSet
fromList xs = foldlStrict ins empty xs
  where ins t x = insert x t

-- | /O(n)/.
fromAscList :: [Int] -> IntSet
fromAscList [] = Nil
fromAscList (x0 : xs0) = fromDistinctAscList (combineEq x0 xs0)
  where
    combineEq x' [] = [x']
    combineEq x' (x:xs)
      | x==x'     = combineEq x' xs
      | otherwise = x' : combineEq x xs

-- | /O(n)/.
fromDistinctAscList :: [Int] -> IntSet
fromDistinctAscList []         = Nil
fromDistinctAscList (z0 : zs0) = work z0 zs0 Nada
  where
    work x []     stk = finish x (Tip x) stk
    work x (z:zs) stk = reduce z zs (branchMask z x) x (Tip x) stk
    reduce z zs _ px tx Nada = work z zs (Push px tx Nada)
    reduce z zs m px tx stk@(Push py ty stk') =
        let mxy = branchMask px py
            pxy = mask px mxy
        in  if shorter m mxy
                 then reduce z zs m pxy (Binary pxy mxy ty tx) stk'
                 else work z zs (Push px tx stk)
    finish _  t  Nada = t
    finish px tx (Push py ty stk) = finish p (join py ty px tx) stk
        where m = branchMask px py
              p = mask px m
data Stack = Push {-# UNPACK #-} !Prefix !IntSet !Stack | Nada

-----------------------------------------------------------------------------

instance Eq IntSet where
  t1 == t2  = equal t1 t2
  t1 /= t2  = nequal t1 t2
equal :: IntSet -> IntSet -> Bool
equal (Binary p1 m1 l1 r1) (Binary p2 m2 l2 r2)
  = (m1 == m2) && (p1 == p2) && (equal l1 l2) && (equal r1 r2)
equal (Tip x) (Tip y) = (x==y)
equal Nil Nil = True
equal _   _   = False
nequal :: IntSet -> IntSet -> Bool
nequal (Binary p1 m1 l1 r1) (Binary p2 m2 l2 r2)
  = (m1 /= m2) || (p1 /= p2) || (nequal l1 l2) || (nequal r1 r2)
nequal (Tip x) (Tip y) = (x/=y)
nequal Nil Nil = False
nequal _   _   = True
instance Ord IntSet where
    compare s1 s2 = compare (toAscList s1) (toAscList s2)
    -- tentative implementation. See if more efficient exists.

-----------------------------------------------------------------------------

#if 0
instance Show IntSet where
  showsPrec p xs = showParen (p > 10) $
    showString "fromList " . shows (toList xs)

instance Read IntSet where
  readPrec = parens $ prec 10 $ do
    Ident "fromList" <- lexP
    xs <- readPrec
    return (fromList xs)
  readListPrec = readListPrecDefault
#endif

instance Show IntSet where
  showsPrec _ Nil = showString "mempty"
  showsPrec _ o   = shows (toList o)

instance Read IntSet where
  readPrec = (parens . choice)
    [do Ident "mempty" <- lexP; return mempty
    ,do xs <- readPrec; return (fromList xs)]
  readListPrec = readListPrecDefault

-----------------------------------------------------------------------------

-- | /O(n)/.
showTree :: IntSet -> String
showTree s = showTreeWith True False s
{- | /O(n)/. The expression (@'showTreeWith' hang wide map@) shows
 the tree that implements the set. If @hang@ is
 'True', a /hanging/ tree is shown otherwise a rotated tree is shown. If
 @wide@ is 'True', an extra wide version is shown.
-}
showTreeWith :: Bool -> Bool -> IntSet -> String
showTreeWith hang wide t
  | hang      = (showsTreeHang wide [] t) ""
  | otherwise = (showsTree wide [] [] t) ""
showsTree :: Bool -> [String] -> [String] -> IntSet -> ShowS
showsTree wide lbars rbars t
  = case t of
      Binary p m l r
          -> showsTree wide (withBar rbars) (withEmpty rbars) r .
             showWide wide rbars .
             showsBars lbars . showString (showBinary p m) . showString "\n" .
             showWide wide lbars .
             showsTree wide (withEmpty lbars) (withBar lbars) l
      Tip x
          -> showsBars lbars . showString " " . shows x . showString "\n"
      Nil -> showsBars lbars . showString "|\n"
showsTreeHang :: Bool -> [String] -> IntSet -> ShowS
showsTreeHang wide bars t
  = case t of
      Binary p m l r
          -> showsBars bars . showString (showBinary p m) . showString "\n" .
             showWide wide bars .
             showsTreeHang wide (withBar bars) l .
             showWide wide bars .
             showsTreeHang wide (withEmpty bars) r
      Tip x
          -> showsBars bars . showString " " . shows x . showString "\n"
      Nil -> showsBars bars . showString "|\n"
showBinary :: Prefix -> Mask -> String
showBinary _ _
  = "*" -- ++ show (p,m)
showWide :: Bool -> [String] -> String -> String
showWide wide bars
  | wide      = showString (concat (reverse bars)) . showString "|\n"
  | otherwise = id
showsBars :: [String] -> ShowS
showsBars bars
  = case bars of
      [] -> id
      _  -> showString (concat (reverse (tail bars))) . showString node
node :: String
node           = "+--"
withBar, withEmpty :: [String] -> [String]
withBar bars   = "|  ":bars
withEmpty bars = "   ":bars

-----------------------------------------------------------------------------
