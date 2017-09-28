{-# LANGUAGE NoMonomorphismRestriction #-}


module MM.Data.Set.Ix (
   IxSet,SetIx
  ,castIxSet
  ,castIxSetArgToIntSet
  ,castIntSetArgToIxSet
  ,fromIntSet,toIntSet
  ,mapM_,foldM
  ,null,size,member,notMember,isSubsetOf
  ,isProperSubsetOf,empty,singleton,insert,delete
  ,union,unions,difference,intersection,intersections
  ,filter,partition,split,splitMember
  ,minWithDefault,maxWithDefault,findMin
  ,findMax,deleteMin,deleteMax,deleteFindMin
  ,deleteFindMax,maxView,minView,map,fold,foldLazy
  ,elems,toList,fromList,toAscList,fromAscList
  ,fromDistinctAscList
) where

import MM.Data.Types.Ix
import Prelude hiding
  (lookup,filter,foldr,foldl,null,map,mapM_)
import MM.Data.Set.Int(IntSet)
import qualified MM.Data.Set.Int as IS
import Data.Monoid(Monoid(..))
import Unsafe.Coerce(unsafeCoerce)
import Data.Binary(Binary(..))


-- type IxSet a = IntSet
-- toIxSet :: [Ix a] -> IxSet a
-- fromIxSet :: IxSet a -> [Ix a]
-- toIxSet = IS.fromList . fmap unIx
-- fromIxSet = fmap Ix . IS.toList



-- | .
type SetIx a = Ix (IxSet a)

newtype IxSet a = IxSet
  {unIxSet :: IntSet}
  deriving(Eq,Ord)

instance Show (IxSet a) where
  showsPrec p (IxSet a) = showsPrec p a

instance Read (IxSet a) where
  readsPrec p s = [(IxSet a,s) | (a,s) <- readsPrec p s]

castIxSet :: IxSet a -> IxSet b
castIxSet (IxSet m) = (IxSet m)

toIntSet :: IxSet a -> IntSet
toIntSet (IxSet m) = m

fromIntSet :: IntSet -> IxSet a
fromIntSet m = IxSet m

castIxSetArgToIntSet :: f (IxSet a) -> f IntSet
castIxSetArgToIntSet = unsafeCoerce

castIntSetArgToIxSet :: f IntSet -> f (IxSet a)
castIntSetArgToIxSet = unsafeCoerce


instance Monoid (IxSet a) where
  mempty = empty
  mappend = union
  mconcat = unions
instance Binary (IxSet a) where
  put (IxSet m) = put m
  get = return . IxSet =<< get




foldM :: (Monad m) => (Ix a -> b -> m b) -> b -> IxSet a -> m b
mapM_ :: (Monad m) => (Ix a -> m b) -> IxSet a -> m ()
empty :: IxSet a
null :: IxSet a -> Bool
size :: IxSet a -> Ix a
member :: Ix a -> IxSet a -> Bool
notMember :: Ix a -> IxSet a -> Bool
singleton :: Ix a -> IxSet a
insert :: Ix a -> IxSet a -> IxSet a
delete :: Ix a -> IxSet a -> IxSet a
union :: IxSet a -> IxSet a -> IxSet a
unions :: [IxSet a] -> IxSet a
difference :: IxSet a -> IxSet a -> IxSet a
intersection :: IxSet a -> IxSet a -> IxSet a
intersections :: [IxSet a] -> IxSet a
isProperSubsetOf :: IxSet a -> IxSet a -> Bool
isSubsetOf :: IxSet a -> IxSet a -> Bool
filter :: (Ix a -> Bool) -> IxSet a -> IxSet a
partition :: (Ix a -> Bool) -> IxSet a -> (IxSet a,IxSet a)
split :: Ix a -> IxSet a -> (IxSet a,IxSet a)
splitMember :: Ix a -> IxSet a -> (IxSet a,Bool,IxSet a)
maxView :: IxSet a -> Maybe (Ix a, IxSet a)
minView :: IxSet a -> Maybe (Ix a, IxSet a)
deleteFindMin :: IxSet a -> (Ix a, IxSet a)
deleteFindMax :: IxSet a -> (Ix a, IxSet a)
findMin :: IxSet a -> Ix a
findMax :: IxSet a -> Ix a
deleteMin :: IxSet a -> IxSet a
deleteMax :: IxSet a -> IxSet a
minWithDefault :: Ix a -> IxSet a -> Ix a
maxWithDefault :: Ix a -> IxSet a -> Ix a
map :: (Ix a-> Ix a) -> IxSet a -> IxSet a
fold :: (Ix a -> b -> b) -> b -> IxSet a -> b
foldLazy :: (Ix a -> b -> b) -> b -> IxSet a -> b
elems :: IxSet a -> [Ix a]
toList :: IxSet a -> [Ix a]
toAscList :: IxSet a -> [Ix a]
fromList :: [Ix a] -> IxSet a
fromAscList :: [Ix a] -> IxSet a
fromDistinctAscList :: [Ix a] -> IxSet a
empty = unsafeCoerce IS.empty
null = unsafeCoerce IS.null
size = unsafeCoerce IS.size
member = unsafeCoerce IS.member
notMember = unsafeCoerce IS.notMember
singleton = unsafeCoerce IS.singleton
insert = unsafeCoerce IS.insert
delete = unsafeCoerce IS.delete
union = unsafeCoerce IS.union
unions = unsafeCoerce IS.unions
difference = unsafeCoerce IS.difference
intersection = unsafeCoerce IS.intersection
intersections = unsafeCoerce IS.intersections
isProperSubsetOf = unsafeCoerce IS.isProperSubsetOf
isSubsetOf = unsafeCoerce IS.isSubsetOf
filter = unsafeCoerce IS.filter
partition = unsafeCoerce IS.partition
split = unsafeCoerce IS.split
splitMember = unsafeCoerce IS.splitMember
maxView = unsafeCoerce IS.maxView
minView = unsafeCoerce IS.minView
deleteFindMin = unsafeCoerce IS.deleteFindMin
deleteFindMax = unsafeCoerce IS.deleteFindMax
findMin = unsafeCoerce IS.findMin
findMax = unsafeCoerce IS.findMax
deleteMin = unsafeCoerce IS.deleteMin
deleteMax = unsafeCoerce IS.deleteMax
minWithDefault = unsafeCoerce IS.minWithDefault
maxWithDefault = unsafeCoerce IS.maxWithDefault
map = unsafeCoerce IS.map
fold = unsafeCoerce IS.fold
foldLazy = unsafeCoerce IS.foldLazy
elems = unsafeCoerce IS.elems
toList = unsafeCoerce IS.toList
toAscList = unsafeCoerce IS.toAscList
fromList = unsafeCoerce IS.fromList
fromAscList = unsafeCoerce IS.fromAscList
fromDistinctAscList = unsafeCoerce IS.fromDistinctAscList
foldM a b c = IS.foldM (unsafeCoerce a) b (unsafeCoerce c)
mapM_ a b = IS.mapM_ (unsafeCoerce a) (unsafeCoerce b)


{-# INLINE empty #-}
{-# INLINE null #-}
{-# INLINE size #-}
{-# INLINE member #-}
{-# INLINE notMember #-}
{-# INLINE singleton #-}
{-# INLINE insert #-}
{-# INLINE delete #-}
{-# INLINE union #-}
{-# INLINE unions #-}
{-# INLINE difference #-}
{-# INLINE intersection #-}
{-# INLINE intersections #-}
{-# INLINE isProperSubsetOf #-}
{-# INLINE isSubsetOf #-}
{-# INLINE filter #-}
{-# INLINE partition #-}
{-# INLINE split #-}
{-# INLINE splitMember #-}
{-# INLINE maxView #-}
{-# INLINE minView #-}
{-# INLINE deleteFindMin #-}
{-# INLINE deleteFindMax #-}
{-# INLINE findMin #-}
{-# INLINE findMax #-}
{-# INLINE deleteMin #-}
{-# INLINE deleteMax #-}
{-# INLINE minWithDefault #-}
{-# INLINE maxWithDefault #-}
{-# INLINE map #-}
{-# INLINE fold #-}
{-# INLINE foldLazy #-}
{-# INLINE elems #-}
{-# INLINE toList #-}
{-# INLINE toAscList #-}
{-# INLINE fromList #-}
{-# INLINE fromAscList #-}
{-# INLINE fromDistinctAscList #-}
{-# INLINE foldM #-}
{-# INLINE mapM_ #-}






{-
empty :: IntSet
null :: IntSet -> Bool
size :: IntSet -> Int
member :: Int -> IntSet -> Bool
notMember :: Int -> IntSet -> Bool
singleton :: Int -> IntSet
insert :: Int -> IntSet -> IntSet
delete :: Int -> IntSet -> IntSet
union :: IntSet -> IntSet -> IntSet
unions :: [IntSet] -> IntSet
difference :: IntSet -> IntSet -> IntSet
intersection :: IntSet -> IntSet -> IntSet
intersections :: [IntSet] -> IntSet
isProperSubsetOf :: IntSet -> IntSet -> Bool
isSubsetOf :: IntSet -> IntSet -> Bool
filter :: (Int -> Bool) -> IntSet -> IntSet
partition :: (Int -> Bool) -> IntSet -> (IntSet,IntSet)
split :: Int -> IntSet -> (IntSet,IntSet)
splitMember :: Int -> IntSet -> (IntSet,Bool,IntSet)
maxView :: IntSet -> Maybe (Int, IntSet)
minView :: IntSet -> Maybe (Int, IntSet)
deleteFindMin :: IntSet -> (Int, IntSet)
deleteFindMax :: IntSet -> (Int, IntSet)
findMin :: IntSet -> Int
findMax :: IntSet -> Int
deleteMin :: IntSet -> IntSet
deleteMax :: IntSet -> IntSet
map :: (Int->Int) -> IntSet -> IntSet
fold :: (Int -> b -> b) -> b -> IntSet -> b
foldLazy :: (Int -> b -> b) -> b -> IntSet -> b
elems :: IntSet -> [Int]
toList :: IntSet -> [Int]
toAscList :: IntSet -> [Int]
fromList :: [Int] -> IntSet
fromAscList :: [Int] -> IntSet
fromDistinctAscList :: [Int] -> IntSet
-}








