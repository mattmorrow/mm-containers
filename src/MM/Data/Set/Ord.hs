{-# LANGUAGE CPP #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Set
-- Copyright   :  (c) Daan Leijen 2002
-- License     :  BSD-style
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- An efficient implementation of sets.
--
-- Since many function names (but not the type name) clash with
-- "Prelude" names, this module is usually imported @qualified@, e.g.
--
-- >  import Data.Set (Set)
-- >  import qualified Data.Set as Set
--
-- The implementation of 'Set' is based on /size balanced/ binary trees (or
-- trees of /bounded balance/) as described by:
--
--    * Stephen Adams, \"/Efficient sets: a balancing act/\",
--	Journal of Functional Programming 3(4):553-562, October 1993,
--	<http://www.swiss.ai.mit.edu/~adams/BB/>.
--
--    * J. Nievergelt and E.M. Reingold,
--	\"/Binary search trees of bounded balance/\",
--	SIAM journal of computing 2(1), March 1973.
--
-- Note that the implementation is /left-biased/ -- the elements of a
-- first argument are always preferred to the second, for example in
-- 'union' or 'insert'.  Of course, left-biasing can only be observed
-- when equality is an equivalence relation instead of structural
-- equality.
-----------------------------------------------------------------------------

module MM.Data.Set.Ord  (
            -- * Set type
              Set(..)          -- instance Eq,Ord,Show,Read,Data,Typeable

            -- * Operators
            , (\\)

            -- * Query
            , null
            , size
            , member
            , notMember
            , isSubsetOf
            , isProperSubsetOf

            -- * Construction
            , empty
            , singleton
            , insert
            , delete

            -- * Combine
            , union, unions
            , difference
            , intersection

            -- * Filter
            , filter
            , partition
            , split
            , splitMember

            -- * Map
	    , map
	    , mapMonotonic

            -- * Fold
            , fold

            -- * Min\/Max
            , findMin
            , findMax
            , deleteMin
            , deleteMax
            , deleteFindMin
            , deleteFindMax
            , maxView
            , minView

            -- * Conversion

            -- ** List
            , elems
            , toList
            , fromList

            -- ** Ordered list
            , toAscList
            , fromAscList
            , fromDistinctAscList

            -- * Debugging
            , showTree
            , showTreeWith
            , valid
            ) where

import Data.Binary

import Prelude hiding (filter,foldr,null,map)
import qualified Data.List as List
import Data.Monoid (Monoid(..))
import Control.Applicative (Applicative(pure,(<*>)),(<$>))

{-
-- just for testing
import QuickCheck
import List (nub,sort)
import qualified List
-}

import Text.Read hiding(get)

{--------------------------------------------------------------------
  Operators
--------------------------------------------------------------------}
infixl 9 \\ --

-- | /O(n+m)/. See 'difference'.
(\\) :: Ord a => Set a -> Set a -> Set a
m1 \\ m2 = difference m1 m2

{--------------------------------------------------------------------
  Sets are size balanced trees
--------------------------------------------------------------------}
-- | A set of values @a@.
data Set a    = Tip
              | Binary {-# UNPACK #-} !Size a !(Set a) !(Set a)



instance (Binary a) => Binary (Set a) where
  put (Tip) = putWord8 0
  put (Binary s a l r) = putWord8 1 >> put s >> put a >> put l >> put r
  get = do tag <- getWord8
           case tag of
            0 -> return Tip
            _ -> Binary <$> get <*> get <*> get <*> get



type Size     = Int

instance Ord a => Monoid (Set a) where
    mempty  = empty
    mappend = union
    mconcat = unions


{--------------------------------------------------------------------
  Query
--------------------------------------------------------------------}
-- | /O(1)/. Is this the empty set?
null :: Set a -> Bool
null t
  = case t of
      Tip    -> True
      Binary {} -> False

-- | /O(1)/. The number of elements in the set.
size :: Set a -> Int
size t
  = case t of
      Tip          -> 0
      Binary sz _ _ _ -> sz

-- | /O(log n)/. Is the element in the set?
member :: Ord a => a -> Set a -> Bool
member x t
  = case t of
      Tip -> False
      Binary _ y l r
          -> case compare x y of
               LT -> member x l
               GT -> member x r
               EQ -> True

-- | /O(log n)/. Is the element not in the set?
notMember :: Ord a => a -> Set a -> Bool
notMember x t = not $ member x t

{--------------------------------------------------------------------
  Construction
--------------------------------------------------------------------}
-- | /O(1)/. The empty set.
empty  :: Set a
empty
  = Tip

-- | /O(1)/. Create a singleton set.
singleton :: a -> Set a
singleton x
  = Binary 1 x Tip Tip

{--------------------------------------------------------------------
  Insertion, Deletion
--------------------------------------------------------------------}
-- | /O(log n)/. Insert an element in a set.
-- If the set already contains an element equal to the given value,
-- it is replaced with the new value.
insert :: Ord a => a -> Set a -> Set a
insert x t
  = case t of
      Tip -> singleton x
      Binary sz y l r
          -> case compare x y of
               LT -> balance y (insert x l) r
               GT -> balance y l (insert x r)
               EQ -> Binary sz x l r


-- | /O(log n)/. Delete an element from a set.
delete :: Ord a => a -> Set a -> Set a
delete x t
  = case t of
      Tip -> Tip
      Binary _ y l r
          -> case compare x y of
               LT -> balance y (delete x l) r
               GT -> balance y l (delete x r)
               EQ -> glue l r

{--------------------------------------------------------------------
  Subset
--------------------------------------------------------------------}
-- | /O(n+m)/. Is this a proper subset? (ie. a subset but not equal).
isProperSubsetOf :: Ord a => Set a -> Set a -> Bool
isProperSubsetOf s1 s2
    = (size s1 < size s2) && (isSubsetOf s1 s2)


-- | /O(n+m)/. Is this a subset?
-- @(s1 `isSubsetOf` s2)@ tells whether @s1@ is a subset of @s2@.
isSubsetOf :: Ord a => Set a -> Set a -> Bool
isSubsetOf t1 t2
  = (size t1 <= size t2) && (isSubsetOfX t1 t2)

isSubsetOfX :: Ord a => Set a -> Set a -> Bool
isSubsetOfX Tip _ = True
isSubsetOfX _ Tip = False
isSubsetOfX (Binary _ x l r) t
  = found && isSubsetOfX l lt && isSubsetOfX r gt
  where
    (lt,found,gt) = splitMember x t


{--------------------------------------------------------------------
  Minimal, Maximal
--------------------------------------------------------------------}
-- | /O(log n)/. The minimal element of a set.
findMin :: Set a -> a
findMin (Binary _ x Tip _) = x
findMin (Binary _ _ l _)   = findMin l
findMin Tip             = error "Set.findMin: empty set has no minimal element"

-- | /O(log n)/. The maximal element of a set.
findMax :: Set a -> a
findMax (Binary _ x _ Tip)  = x
findMax (Binary _ _ _ r)    = findMax r
findMax Tip              = error "Set.findMax: empty set has no maximal element"

-- | /O(log n)/. Delete the minimal element.
deleteMin :: Set a -> Set a
deleteMin (Binary _ _ Tip r) = r
deleteMin (Binary _ x l r)   = balance x (deleteMin l) r
deleteMin Tip             = Tip

-- | /O(log n)/. Delete the maximal element.
deleteMax :: Set a -> Set a
deleteMax (Binary _ _ l Tip) = l
deleteMax (Binary _ x l r)   = balance x l (deleteMax r)
deleteMax Tip             = Tip


{--------------------------------------------------------------------
  Union.
--------------------------------------------------------------------}
-- | The union of a list of sets: (@'unions' == 'foldl' 'union' 'empty'@).
unions :: Ord a => [Set a] -> Set a
unions ts
  = foldlStrict union empty ts


-- | /O(n+m)/. The union of two sets, preferring the first set when
-- equal elements are encountered.
-- The implementation uses the efficient /hedge-union/ algorithm.
-- Hedge-union is more efficient on (bigset `union` smallset).
union :: Ord a => Set a -> Set a -> Set a
union Tip t2  = t2
union t1 Tip  = t1
union t1 t2 = hedgeUnion (const LT) (const GT) t1 t2

hedgeUnion :: Ord a
           => (a -> Ordering) -> (a -> Ordering) -> Set a -> Set a -> Set a
hedgeUnion _     _     t1 Tip
  = t1
hedgeUnion cmplo cmphi Tip (Binary _ x l r)
  = join x (filterGt cmplo l) (filterLt cmphi r)
hedgeUnion cmplo cmphi (Binary _ x l r) t2
  = join x (hedgeUnion cmplo cmpx l (trim cmplo cmpx t2))
           (hedgeUnion cmpx cmphi r (trim cmpx cmphi t2))
  where
    cmpx y  = compare x y

{--------------------------------------------------------------------
  Difference
--------------------------------------------------------------------}
-- | /O(n+m)/. Difference of two sets.
-- The implementation uses an efficient /hedge/ algorithm comparable with /hedge-union/.
difference :: Ord a => Set a -> Set a -> Set a
difference Tip _   = Tip
difference t1 Tip  = t1
difference t1 t2   = hedgeDiff (const LT) (const GT) t1 t2

hedgeDiff :: Ord a
          => (a -> Ordering) -> (a -> Ordering) -> Set a -> Set a -> Set a
hedgeDiff _ _ Tip _
  = Tip
hedgeDiff cmplo cmphi (Binary _ x l r) Tip
  = join x (filterGt cmplo l) (filterLt cmphi r)
hedgeDiff cmplo cmphi t (Binary _ x l r)
  = merge (hedgeDiff cmplo cmpx (trim cmplo cmpx t) l)
          (hedgeDiff cmpx cmphi (trim cmpx cmphi t) r)
  where
    cmpx y = compare x y

{--------------------------------------------------------------------
  Intersection
--------------------------------------------------------------------}
-- | /O(n+m)/. The intersection of two sets.
-- Elements of the result come from the first set, so for example
--
-- > import qualified Data.Set as S
-- > data AB = A | B deriving Show
-- > instance Ord AB where compare _ _ = EQ
-- > instance Eq AB where _ == _ = True
-- > main = print (S.singleton A `S.intersection` S.singleton B,
-- >               S.singleton B `S.intersection` S.singleton A)
--
-- prints @(fromList [A],fromList [B])@.
intersection :: Ord a => Set a -> Set a -> Set a
intersection Tip _ = Tip
intersection _ Tip = Tip
intersection t1@(Binary s1 x1 l1 r1) t2@(Binary s2 x2 l2 r2) =
   if s1 >= s2 then
      let (lt,found,gt) = splitLookup x2 t1
          tl            = intersection lt l2
          tr            = intersection gt r2
      in case found of
      Just x -> join x tl tr
      Nothing -> merge tl tr
   else let (lt,found,gt) = splitMember x1 t2
            tl            = intersection l1 lt
            tr            = intersection r1 gt
        in if found then join x1 tl tr
           else merge tl tr

{--------------------------------------------------------------------
  Filter and partition
--------------------------------------------------------------------}
-- | /O(n)/. Filter all elements that satisfy the predicate.
filter :: Ord a => (a -> Bool) -> Set a -> Set a
filter _ Tip = Tip
filter p (Binary _ x l r)
  | p x       = join x (filter p l) (filter p r)
  | otherwise = merge (filter p l) (filter p r)

-- | /O(n)/. Partition the set into two sets, one with all elements that satisfy
-- the predicate and one with all elements that don't satisfy the predicate.
-- See also 'split'.
partition :: Ord a => (a -> Bool) -> Set a -> (Set a,Set a)
partition _ Tip = (Tip,Tip)
partition p (Binary _ x l r)
  | p x       = (join x l1 r1,merge l2 r2)
  | otherwise = (merge l1 r1,join x l2 r2)
  where
    (l1,l2) = partition p l
    (r1,r2) = partition p r

{----------------------------------------------------------------------
  Map
----------------------------------------------------------------------}

-- | /O(n*log n)/.
-- @'map' f s@ is the set obtained by applying @f@ to each element of @s@.
--
-- It's worth noting that the size of the result may be smaller if,
-- for some @(x,y)@, @x \/= y && f x == f y@

map :: (Ord a, Ord b) => (a->b) -> Set a -> Set b
map f = fromList . List.map f . toList

-- | /O(n)/. The
--
-- @'mapMonotonic' f s == 'map' f s@, but works only when @f@ is monotonic.
-- /The precondition is not checked./
-- Semi-formally, we have:
--
-- > and [x < y ==> f x < f y | x <- ls, y <- ls]
-- >                     ==> mapMonotonic f s == map f s
-- >     where ls = toList s

mapMonotonic :: (a->b) -> Set a -> Set b
mapMonotonic _ Tip = Tip
mapMonotonic f (Binary sz x l r) =
    Binary sz (f x) (mapMonotonic f l) (mapMonotonic f r)


{--------------------------------------------------------------------
  Fold
--------------------------------------------------------------------}
-- | /O(n)/. Fold over the elements of a set in an unspecified order.
fold :: (a -> b -> b) -> b -> Set a -> b
fold f z s
  = foldr f z s

-- | /O(n)/. Post-order fold.
foldr :: (a -> b -> b) -> b -> Set a -> b
foldr _ z Tip           = z
foldr f z (Binary _ x l r) = foldr f (f x (foldr f z r)) l

{--------------------------------------------------------------------
  List variations
--------------------------------------------------------------------}
-- | /O(n)/. The elements of a set.
elems :: Set a -> [a]
elems s
  = toList s

{--------------------------------------------------------------------
  Lists
--------------------------------------------------------------------}
-- | /O(n)/. Convert the set to a list of elements.
toList :: Set a -> [a]
toList s
  = toAscList s

-- | /O(n)/. Convert the set to an ascending list of elements.
toAscList :: Set a -> [a]
toAscList t
  = foldr (:) [] t


-- | /O(n*log n)/. Create a set from a list of elements.
fromList :: Ord a => [a] -> Set a
fromList xs
  = foldlStrict ins empty xs
  where
    ins t x = insert x t

{--------------------------------------------------------------------
  Building trees from ascending/descending lists can be done in linear time.

  Note that if [xs] is ascending that:
    fromAscList xs == fromList xs
--------------------------------------------------------------------}
-- | /O(n)/. Build a set from an ascending list in linear time.
-- /The precondition (input list is ascending) is not checked./
fromAscList :: Eq a => [a] -> Set a
fromAscList xs
  = fromDistinctAscList (combineEq xs)
  where
  -- [combineEq xs] combines equal elements with [const] in an ordered list [xs]
  combineEq xs'
    = case xs' of
        []     -> []
        [x]    -> [x]
        (x:xx) -> combineEq' x xx

  combineEq' z [] = [z]
  combineEq' z (x:xs')
    | z==x      =   combineEq' z xs'
    | otherwise = z:combineEq' x xs'


-- | /O(n)/. Build a set from an ascending list of distinct elements in linear time.
-- /The precondition (input list is strictly ascending) is not checked./
fromDistinctAscList :: [a] -> Set a
fromDistinctAscList xs
  = build const (length xs) xs
  where
    -- 1) use continutations so that we use heap space instead of stack space.
    -- 2) special case for n==5 to build bushier trees.
    build c 0 xs'  = c Tip xs'
    build c 5 xs'  = case xs' of
                       (x1:x2:x3:x4:x5:xx)
                            -> c (bin x4 (bin x2 (singleton x1) (singleton x3)) (singleton x5)) xx
                       _ -> error "fromDistinctAscList build 5"
    build c n xs'  = seq nr $ build (buildR nr c) nl xs'
                   where
                     nl = n `div` 2
                     nr = n - nl - 1

    buildR n c l (x:ys) = build (buildB l x c) n ys
    buildR _ _ _ []     = error "fromDistinctAscList buildR []"
    buildB l x c r zs   = c (bin x l r) zs

{--------------------------------------------------------------------
  Eq converts the set to a list. In a lazy setting, this
  actually seems one of the faster methods to compare two trees
  and it is certainly the simplest :-)
--------------------------------------------------------------------}
instance Eq a => Eq (Set a) where
  t1 == t2  = (size t1 == size t2) && (toAscList t1 == toAscList t2)

{--------------------------------------------------------------------
  Ord
--------------------------------------------------------------------}

instance Ord a => Ord (Set a) where
    compare s1 s2 = compare (toAscList s1) (toAscList s2)

-----------------------------------------------------------------------------

#if 0
instance Show a => Show (Set a) where
  showsPrec p xs = showParen (p > 10) $
    showString "fromList " . shows (toList xs)
instance (Read a, Ord a) => Read (Set a) where
  readPrec = parens $ prec 10 $ do
    Ident "fromList" <- lexP
    xs <- readPrec
    return (fromList xs)
  readListPrec = readListPrecDefault
#endif

instance (Show a) => Show (Set a) where
  showsPrec _ Tip = showString "mempty"
  showsPrec _ o   = shows (toList o)

instance (Ord a, Read a) => Read (Set a) where
  readPrec = (parens . choice)
    [do Ident "mempty" <- lexP; return mempty
    ,do xs <- readPrec; return (fromList xs)]
  readListPrec = readListPrecDefault

-----------------------------------------------------------------------------
{--------------------------------------------------------------------
  Utility functions that return sub-ranges of the original
  tree. Some functions take a comparison function as argument to
  allow comparisons against infinite values. A function [cmplo x]
  should be read as [compare lo x].

  [trim cmplo cmphi t]  A tree that is either empty or where [cmplo x == LT]
                        and [cmphi x == GT] for the value [x] of the root.
  [filterGt cmp t]      A tree where for all values [k]. [cmp k == LT]
  [filterLt cmp t]      A tree where for all values [k]. [cmp k == GT]

  [split k t]           Returns two trees [l] and [r] where all values
                        in [l] are <[k] and all keys in [r] are >[k].
  [splitMember k t]     Just like [split] but also returns whether [k]
                        was found in the tree.
--------------------------------------------------------------------}

{--------------------------------------------------------------------
  [trim lo hi t] trims away all subtrees that surely contain no
  values between the range [lo] to [hi]. The returned tree is either
  empty or the key of the root is between @lo@ and @hi@.
--------------------------------------------------------------------}
trim :: (a -> Ordering) -> (a -> Ordering) -> Set a -> Set a
trim _     _     Tip = Tip
trim cmplo cmphi t@(Binary _ x l r)
  = case cmplo x of
      LT -> case cmphi x of
              GT -> t
              _  -> trim cmplo cmphi l
      _  -> trim cmplo cmphi r

{-
XXX unused code

trimMemberLo :: Ord a => a -> (a -> Ordering) -> Set a -> (Bool, Set a)
trimMemberLo _  _     Tip = (False,Tip)
trimMemberLo lo cmphi t@(Binary _ x l r)
  = case compare lo x of
      LT -> case cmphi x of
              GT -> (member lo t, t)
              _  -> trimMemberLo lo cmphi l
      GT -> trimMemberLo lo cmphi r
      EQ -> (True,trim (compare lo) cmphi r)
-}

{--------------------------------------------------------------------
  [filterGt x t] filter all values >[x] from tree [t]
  [filterLt x t] filter all values <[x] from tree [t]
--------------------------------------------------------------------}
filterGt :: (a -> Ordering) -> Set a -> Set a
filterGt _ Tip = Tip
filterGt cmp (Binary _ x l r)
  = case cmp x of
      LT -> join x (filterGt cmp l) r
      GT -> filterGt cmp r
      EQ -> r

filterLt :: (a -> Ordering) -> Set a -> Set a
filterLt _ Tip = Tip
filterLt cmp (Binary _ x l r)
  = case cmp x of
      LT -> filterLt cmp l
      GT -> join x l (filterLt cmp r)
      EQ -> l


{--------------------------------------------------------------------
  Split
--------------------------------------------------------------------}
-- | /O(log n)/. The expression (@'split' x set@) is a pair @(set1,set2)@
-- where @set1@ comprises the elements of @set@ less than @x@ and @set2@
-- comprises the elements of @set@ greater than @x@.
split :: Ord a => a -> Set a -> (Set a,Set a)
split _ Tip = (Tip,Tip)
split x (Binary _ y l r)
  = case compare x y of
      LT -> let (lt,gt) = split x l in (lt,join y gt r)
      GT -> let (lt,gt) = split x r in (join y l lt,gt)
      EQ -> (l,r)

-- | /O(log n)/. Performs a 'split' but also returns whether the pivot
-- element was found in the original set.
splitMember :: Ord a => a -> Set a -> (Set a,Bool,Set a)
splitMember x t = let (l,m,r) = splitLookup x t in
     (l,maybe False (const True) m,r)

-- | /O(log n)/. Performs a 'split' but also returns the pivot
-- element that was found in the original set.
splitLookup :: Ord a => a -> Set a -> (Set a,Maybe a,Set a)
splitLookup _ Tip = (Tip,Nothing,Tip)
splitLookup x (Binary _ y l r)
   = case compare x y of
       LT -> let (lt,found,gt) = splitLookup x l in (lt,found,join y gt r)
       GT -> let (lt,found,gt) = splitLookup x r in (join y l lt,found,gt)
       EQ -> (l,Just y,r)

{--------------------------------------------------------------------
  Utility functions that maintain the balance properties of the tree.
  All constructors assume that all values in [l] < [x] and all values
  in [r] > [x], and that [l] and [r] are valid trees.

  In order of sophistication:
    [Binary sz x l r]    The type constructor.
    [bin x l r]       Maintains the correct size, assumes that both [l]
                      and [r] are balanced with respect to each other.
    [balance x l r]   Restores the balance and size.
                      Assumes that the original tree was balanced and
                      that [l] or [r] has changed by at most one element.
    [join x l r]      Restores balance and size.

  Furthermore, we can construct a new tree from two trees. Both operations
  assume that all values in [l] < all values in [r] and that [l] and [r]
  are valid:
    [glue l r]        Glues [l] and [r] together. Assumes that [l] and
                      [r] are already balanced with respect to each other.
    [merge l r]       Merges two trees and restores balance.

  Note: in contrast to Adam's paper, we use (<=) comparisons instead
  of (<) comparisons in [join], [merge] and [balance].
  Quickcheck (on [difference]) showed that this was necessary in order
  to maintain the invariants. It is quite unsatisfactory that I haven't
  been able to find out why this is actually the case! Fortunately, it
  doesn't hurt to be a bit more conservative.
--------------------------------------------------------------------}

{--------------------------------------------------------------------
  Join
--------------------------------------------------------------------}
join :: a -> Set a -> Set a -> Set a
join x Tip r  = insertMin x r
join x l Tip  = insertMax x l
join x l@(Binary sizeL y ly ry) r@(Binary sizeR z lz rz)
  | delta*sizeL <= sizeR  = balance z (join x l lz) rz
  | delta*sizeR <= sizeL  = balance y ly (join x ry r)
  | otherwise             = bin x l r


-- insertMin and insertMax don't perform potentially expensive comparisons.
insertMax,insertMin :: a -> Set a -> Set a
insertMax x t
  = case t of
      Tip -> singleton x
      Binary _ y l r
          -> balance y l (insertMax x r)

insertMin x t
  = case t of
      Tip -> singleton x
      Binary _ y l r
          -> balance y (insertMin x l) r

{--------------------------------------------------------------------
  [merge l r]: merges two trees.
--------------------------------------------------------------------}
merge :: Set a -> Set a -> Set a
merge Tip r   = r
merge l Tip   = l
merge l@(Binary sizeL x lx rx) r@(Binary sizeR y ly ry)
  | delta*sizeL <= sizeR = balance y (merge l ly) ry
  | delta*sizeR <= sizeL = balance x lx (merge rx r)
  | otherwise            = glue l r

{--------------------------------------------------------------------
  [glue l r]: glues two trees together.
  Assumes that [l] and [r] are already balanced with respect to each other.
--------------------------------------------------------------------}
glue :: Set a -> Set a -> Set a
glue Tip r = r
glue l Tip = l
glue l r
  | size l > size r = let (m,l') = deleteFindMax l in balance m l' r
  | otherwise       = let (m,r') = deleteFindMin r in balance m l r'


-- | /O(log n)/. Delete and find the minimal element.
--
-- > deleteFindMin set = (findMin set, deleteMin set)

deleteFindMin :: Set a -> (a,Set a)
deleteFindMin t
  = case t of
      Binary _ x Tip r -> (x,r)
      Binary _ x l r   -> let (xm,l') = deleteFindMin l in (xm,balance x l' r)
      Tip           -> (error "Set.deleteFindMin: can not return the minimal element of an empty set", Tip)

-- | /O(log n)/. Delete and find the maximal element.
--
-- > deleteFindMax set = (findMax set, deleteMax set)
deleteFindMax :: Set a -> (a,Set a)
deleteFindMax t
  = case t of
      Binary _ x l Tip -> (x,l)
      Binary _ x l r   -> let (xm,r') = deleteFindMax r in (xm,balance x l r')
      Tip           -> (error "Set.deleteFindMax: can not return the maximal element of an empty set", Tip)

-- | /O(log n)/. Retrieves the minimal key of the set, and the set
-- stripped of that element, or 'Nothing' if passed an empty set.
minView :: Set a -> Maybe (a, Set a)
minView Tip = Nothing
minView x = Just (deleteFindMin x)

-- | /O(log n)/. Retrieves the maximal key of the set, and the set
-- stripped of that element, or 'Nothing' if passed an empty set.
maxView :: Set a -> Maybe (a, Set a)
maxView Tip = Nothing
maxView x = Just (deleteFindMax x)

{--------------------------------------------------------------------
  [balance x l r] balances two trees with value x.
  The sizes of the trees should balance after decreasing the
  size of one of them. (a rotation).

  [delta] is the maximal relative difference between the sizes of
          two trees, it corresponds with the [w] in Adams' paper,
          or equivalently, [1/delta] corresponds with the $\alpha$
          in Nievergelt's paper. Adams shows that [delta] should
          be larger than 3.745 in order to garantee that the
          rotations can always restore balance.

  [ratio] is the ratio between an outer and inner sibling of the
          heavier subtree in an unbalanced setting. It determines
          whether a double or single rotation should be performed
          to restore balance. It is correspondes with the inverse
          of $\alpha$ in Adam's article.

  Note that:
  - [delta] should be larger than 4.646 with a [ratio] of 2.
  - [delta] should be larger than 3.745 with a [ratio] of 1.534.

  - A lower [delta] leads to a more 'perfectly' balanced tree.
  - A higher [delta] performs less rebalancing.

  - Balancing is automatic for random data and a balancing
    scheme is only necessary to avoid pathological worst cases.
    Almost any choice will do in practice

  - Allthough it seems that a rather large [delta] may perform better
    than smaller one, measurements have shown that the smallest [delta]
    of 4 is actually the fastest on a wide range of operations. It
    especially improves performance on worst-case scenarios like
    a sequence of ordered insertions.

  Note: in contrast to Adams' paper, we use a ratio of (at least) 2
  to decide whether a single or double rotation is needed. Allthough
  he actually proves that this ratio is needed to maintain the
  invariants, his implementation uses a (invalid) ratio of 1.
  He is aware of the problem though since he has put a comment in his
  original source code that he doesn't care about generating a
  slightly inbalanced tree since it doesn't seem to matter in practice.
  However (since we use quickcheck :-) we will stick to strictly balanced
  trees.
--------------------------------------------------------------------}
delta,ratio :: Int
delta = 4
ratio = 2

balance :: a -> Set a -> Set a -> Set a
balance x l r
  | sizeL + sizeR <= 1    = Binary sizeX x l r
  | sizeR >= delta*sizeL  = rotateL x l r
  | sizeL >= delta*sizeR  = rotateR x l r
  | otherwise             = Binary sizeX x l r
  where
    sizeL = size l
    sizeR = size r
    sizeX = sizeL + sizeR + 1

-- rotate
rotateL :: a -> Set a -> Set a -> Set a
rotateL x l r@(Binary _ _ ly ry)
  | size ly < ratio*size ry = singleL x l r
  | otherwise               = doubleL x l r
rotateL _ _ Tip = error "rotateL Tip"

rotateR :: a -> Set a -> Set a -> Set a
rotateR x l@(Binary _ _ ly ry) r
  | size ry < ratio*size ly = singleR x l r
  | otherwise               = doubleR x l r
rotateR _ Tip _ = error "rotateL Tip"

-- basic rotations
singleL, singleR :: a -> Set a -> Set a -> Set a
singleL x1 t1 (Binary _ x2 t2 t3)  = bin x2 (bin x1 t1 t2) t3
singleL _  _  Tip               = error "singleL"
singleR x1 (Binary _ x2 t1 t2) t3  = bin x2 t1 (bin x1 t2 t3)
singleR _  Tip              _   = error "singleR"

doubleL, doubleR :: a -> Set a -> Set a -> Set a
doubleL x1 t1 (Binary _ x2 (Binary _ x3 t2 t3) t4) = bin x3 (bin x1 t1 t2) (bin x2 t3 t4)
doubleL _ _ _ = error "doubleL"
doubleR x1 (Binary _ x2 t1 (Binary _ x3 t2 t3)) t4 = bin x3 (bin x2 t1 t2) (bin x1 t3 t4)
doubleR _ _ _ = error "doubleR"


{--------------------------------------------------------------------
  The bin constructor maintains the size of the tree
--------------------------------------------------------------------}
bin :: a -> Set a -> Set a -> Set a
bin x l r
  = Binary (size l + size r + 1) x l r


{--------------------------------------------------------------------
  Utilities
--------------------------------------------------------------------}
foldlStrict :: (a -> b -> a) -> a -> [b] -> a
foldlStrict f z xs
  = case xs of
      []     -> z
      (x:xx) -> let z' = f z x in seq z' (foldlStrict f z' xx)


{--------------------------------------------------------------------
  Debugging
--------------------------------------------------------------------}
-- | /O(n)/. Show the tree that implements the set. The tree is shown
-- in a compressed, hanging format.
showTree :: Show a => Set a -> String
showTree s
  = showTreeWith True False s


{- | /O(n)/. The expression (@showTreeWith hang wide map@) shows
 the tree that implements the set. If @hang@ is
 @True@, a /hanging/ tree is shown otherwise a rotated tree is shown. If
 @wide@ is 'True', an extra wide version is shown.

> Set> putStrLn $ showTreeWith True False $ fromDistinctAscList [1..5]
> 4
> +--2
> |  +--1
> |  +--3
> +--5
>
> Set> putStrLn $ showTreeWith True True $ fromDistinctAscList [1..5]
> 4
> |
> +--2
> |  |
> |  +--1
> |  |
> |  +--3
> |
> +--5
>
> Set> putStrLn $ showTreeWith False True $ fromDistinctAscList [1..5]
> +--5
> |
> 4
> |
> |  +--3
> |  |
> +--2
>    |
>    +--1

-}
showTreeWith :: Show a => Bool -> Bool -> Set a -> String
showTreeWith hang wide t
  | hang      = (showsTreeHang wide [] t) ""
  | otherwise = (showsTree wide [] [] t) ""

showsTree :: Show a => Bool -> [String] -> [String] -> Set a -> ShowS
showsTree wide lbars rbars t
  = case t of
      Tip -> showsBars lbars . showString "|\n"
      Binary _ x Tip Tip
          -> showsBars lbars . shows x . showString "\n"
      Binary _ x l r
          -> showsTree wide (withBar rbars) (withEmpty rbars) r .
             showWide wide rbars .
             showsBars lbars . shows x . showString "\n" .
             showWide wide lbars .
             showsTree wide (withEmpty lbars) (withBar lbars) l

showsTreeHang :: Show a => Bool -> [String] -> Set a -> ShowS
showsTreeHang wide bars t
  = case t of
      Tip -> showsBars bars . showString "|\n"
      Binary _ x Tip Tip
          -> showsBars bars . shows x . showString "\n"
      Binary _ x l r
          -> showsBars bars . shows x . showString "\n" .
             showWide wide bars .
             showsTreeHang wide (withBar bars) l .
             showWide wide bars .
             showsTreeHang wide (withEmpty bars) r

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

{--------------------------------------------------------------------
  Assertions
--------------------------------------------------------------------}
-- | /O(n)/. Test if the internal set structure is valid.
valid :: Ord a => Set a -> Bool
valid t
  = balanced t && ordered t && validsize t

ordered :: Ord a => Set a -> Bool
ordered t
  = bounded (const True) (const True) t
  where
    bounded lo hi t'
      = case t' of
          Tip         -> True
          Binary _ x l r -> (lo x) && (hi x) && bounded lo (<x) l && bounded (>x) hi r

balanced :: Set a -> Bool
balanced t
  = case t of
      Tip         -> True
      Binary _ _ l r -> (size l + size r <= 1 || (size l <= delta*size r && size r <= delta*size l)) &&
                     balanced l && balanced r

validsize :: Set a -> Bool
validsize t
  = (realsize t == Just (size t))
  where
    realsize t'
      = case t' of
          Tip          -> Just 0
          Binary sz _ l r -> case (realsize l,realsize r) of
                            (Just n,Just m)  | n+m+1 == sz  -> Just sz
                            _                -> Nothing

{-
{--------------------------------------------------------------------
  Testing
--------------------------------------------------------------------}
testTree :: [Int] -> Set Int
testTree xs   = fromList xs
test1 = testTree [1..20]
test2 = testTree [30,29..10]
test3 = testTree [1,4,6,89,2323,53,43,234,5,79,12,9,24,9,8,423,8,42,4,8,9,3]

{--------------------------------------------------------------------
  QuickCheck
--------------------------------------------------------------------}
qcheck prop
  = check config prop
  where
    config = Config
      { configMaxTest = 500
      , configMaxFail = 5000
      , configSize    = \n -> (div n 2 + 3)
      , configEvery   = \n args -> let s = show n in s ++ [ '\b' | _ <- s ]
      }


{--------------------------------------------------------------------
  Arbitrary, reasonably balanced trees
--------------------------------------------------------------------}
instance (Enum a) => Arbitrary (Set a) where
  arbitrary = sized (arbtree 0 maxkey)
            where maxkey  = 10000

arbtree :: (Enum a) => Int -> Int -> Int -> Gen (Set a)
arbtree lo hi n
  | n <= 0        = return Tip
  | lo >= hi      = return Tip
  | otherwise     = do{ i  <- choose (lo,hi)
                      ; m  <- choose (1,30)
                      ; let (ml,mr)  | m==(1::Int)= (1,2)
                                     | m==2       = (2,1)
                                     | m==3       = (1,1)
                                     | otherwise  = (2,2)
                      ; l  <- arbtree lo (i-1) (n `div` ml)
                      ; r  <- arbtree (i+1) hi (n `div` mr)
                      ; return (bin (toEnum i) l r)
                      }


{--------------------------------------------------------------------
  Valid tree's
--------------------------------------------------------------------}
forValid :: (Enum a,Show a,Testable b) => (Set a -> b) -> Property
forValid f
  = forAll arbitrary $ \t ->
--    classify (balanced t) "balanced" $
    classify (size t == 0) "empty" $
    classify (size t > 0  && size t <= 10) "small" $
    classify (size t > 10 && size t <= 64) "medium" $
    classify (size t > 64) "large" $
    balanced t ==> f t

forValidIntTree :: Testable a => (Set Int -> a) -> Property
forValidIntTree f
  = forValid f

forValidUnitTree :: Testable a => (Set Int -> a) -> Property
forValidUnitTree f
  = forValid f


prop_Valid
  = forValidUnitTree $ \t -> valid t

{--------------------------------------------------------------------
  Single, Insert, Delete
--------------------------------------------------------------------}
prop_Single :: Int -> Bool
prop_Single x
  = (insert x empty == singleton x)

prop_InsertValid :: Int -> Property
prop_InsertValid k
  = forValidUnitTree $ \t -> valid (insert k t)

prop_InsertDelete :: Int -> Set Int -> Property
prop_InsertDelete k t
  = not (member k t) ==> delete k (insert k t) == t

prop_DeleteValid :: Int -> Property
prop_DeleteValid k
  = forValidUnitTree $ \t ->
    valid (delete k (insert k t))

{--------------------------------------------------------------------
  Balance
--------------------------------------------------------------------}
prop_Join :: Int -> Property
prop_Join x
  = forValidUnitTree $ \t ->
    let (l,r) = split x t
    in valid (join x l r)

prop_Merge :: Int -> Property
prop_Merge x
  = forValidUnitTree $ \t ->
    let (l,r) = split x t
    in valid (merge l r)


{--------------------------------------------------------------------
  Union
--------------------------------------------------------------------}
prop_UnionValid :: Property
prop_UnionValid
  = forValidUnitTree $ \t1 ->
    forValidUnitTree $ \t2 ->
    valid (union t1 t2)

prop_UnionInsert :: Int -> Set Int -> Bool
prop_UnionInsert x t
  = union t (singleton x) == insert x t

prop_UnionAssoc :: Set Int -> Set Int -> Set Int -> Bool
prop_UnionAssoc t1 t2 t3
  = union t1 (union t2 t3) == union (union t1 t2) t3

prop_UnionComm :: Set Int -> Set Int -> Bool
prop_UnionComm t1 t2
  = (union t1 t2 == union t2 t1)


prop_DiffValid
  = forValidUnitTree $ \t1 ->
    forValidUnitTree $ \t2 ->
    valid (difference t1 t2)

prop_Diff :: [Int] -> [Int] -> Bool
prop_Diff xs ys
  =  toAscList (difference (fromList xs) (fromList ys))
    == List.sort ((List.\\) (nub xs)  (nub ys))

prop_IntValid
  = forValidUnitTree $ \t1 ->
    forValidUnitTree $ \t2 ->
    valid (intersection t1 t2)

prop_Int :: [Int] -> [Int] -> Bool
prop_Int xs ys
  =  toAscList (intersection (fromList xs) (fromList ys))
    == List.sort (nub ((List.intersect) (xs)  (ys)))

{--------------------------------------------------------------------
  Lists
--------------------------------------------------------------------}
prop_Ordered
  = forAll (choose (5,100)) $ \n ->
    let xs = [0..n::Int]
    in fromAscList xs == fromList xs

prop_List :: [Int] -> Bool
prop_List xs
  = (sort (nub xs) == toList (fromList xs))
-}
