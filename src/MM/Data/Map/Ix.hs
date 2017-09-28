{-# LANGUAGE BangPatterns, NoMonomorphismRestriction #-}

module MM.Data.Map.Ix (
   IxMap,MapIx,Index,Trans,Rename
  ,castIxMap
  ,castIxMapArgToIntMap
  ,castIntMapArgToIxMap
  ,fromIntMap,toIntMap
  ,(!),null,size,member,notMember
  ,lookup,findWithDefault,empty,singleton,insert
  ,insertWith,insertWithKey,insertLookupWithKey,delete
  ,adjust,adjustWithKey,update,updateWithKey
  ,updateLookupWithKey,alter,union,unionWith
  ,unionWithKey,unions,unionsWith,difference
  ,differenceWith,differenceWithKey,intersection
  ,intersectionWith,intersectionWithKey,map,mapWithKey
  ,mapAccum,mapAccumWithKey,mapAccumRWithKey,fold
  ,foldWithKey,foldWithKey2,foldWithKey3
  ,elems,keys,keysSet,assocs,toList
  ,fromList,fromListWith,fromListWithKey,toAscList
  ,fromAscList,fromAscListWith,fromAscListWithKey
  ,fromDistinctAscList,filter,filterWithKey,partition
  ,partitionWithKey,mapMaybe,mapMaybeWithKey,mapEither
  ,mapEitherWithKey,split,splitLookup,isSubmapOf
  ,isSubmapOfBy,isProperSubmapOf,isProperSubmapOfBy
  ,minKeyWithDefault,maxKeyWithDefault
  ,maxView,minView,findMin,findMax,deleteMin
  ,deleteMax,deleteFindMin,deleteFindMax,updateMin
  ,updateMax,updateMinWithKey,updateMaxWithKey
  ,minViewWithKey,maxViewWithKey
  ,insertWithM,insertWithKeyM,updateWithKeyM,foldM
  ,foldlM,foldlM',mapM,mapM_,mapWithKeyM
  ,mapWithKeyM_,intersectionWithM,intersectionWithKeyM
  ,unionWithM,unionWithKeyM,differenceWithM
  ,differenceWithKeyM,filterM,filterWithKeyM


) where

import MM.Data.Types.Ix
import Prelude hiding
  (lookup,map,filter,foldr,foldl,null,mapM,mapM_)
import Unsafe.Coerce(unsafeCoerce)
import MM.Data.Map.Int(IntMap)
import MM.Data.Set.Int(IntSet)
import qualified MM.Data.Map.Int as IM
import qualified MM.Data.Set.Int as IS
import Data.Monoid (Monoid(..))
import Data.Binary



-- | .
type MapIx a b = Ix (IxMap a b)
type Index a = IxMap a a
type Trans a b = IxMap a (Ix b)
type Rename a = Trans a a


newtype IxMap a b = IxMap
  {unIxMap :: IntMap b}
  deriving(Eq,Ord)

instance (Show b) => Show (IxMap a b) where
  showsPrec p (IxMap a) = showsPrec p a

instance (Read b) => Read (IxMap a b) where
  readsPrec p s = [(IxMap a,s) | (a,s) <- readsPrec p s]

instance Monoid (IxMap a b) where
  mempty = empty
  mappend = union
  mconcat = unions
instance Functor (IxMap a) where
  fmap = map
instance (Binary b) => Binary (IxMap a b) where
  put (IxMap m) = put m
  get = return . IxMap =<< get

castIxMap :: IxMap a b -> IxMap c b
castIxMap (IxMap m) = (IxMap m)

toIntMap :: IxMap a b -> IntMap b
toIntMap (IxMap m) = m

fromIntMap :: IntMap b -> IxMap a b
fromIntMap m = IxMap m

castIxMapArgToIntMap :: f (IxMap a b) -> f (IntMap b)
castIxMapArgToIntMap = unsafeCoerce

castIntMapArgToIxMap :: f (IntMap b) -> f (IxMap a b)
castIntMapArgToIxMap = unsafeCoerce


(!)                   :: IxMap a b -> Ix a -> b
size                  :: IxMap a b -> Int
member                :: Ix a -> IxMap a b -> Bool
notMember             :: Ix a -> IxMap a b -> Bool
findWithDefault       :: b -> Ix a -> IxMap a b -> b
mapMaybe              :: (b -> Maybe c) -> IxMap a b -> IxMap a c
mapMaybeWithKey       :: (Ix a -> b -> Maybe c) -> IxMap a b -> IxMap a c
mapEither             :: (b -> Either c d) -> IxMap a b -> (IxMap a c, IxMap a d)
mapEitherWithKey      :: (Ix a -> b -> Either c d) -> IxMap a b -> (IxMap a c, IxMap a d)
empty                 :: IxMap a b
singleton             :: Ix a -> b -> IxMap a b
null                  :: IxMap a b -> Bool
lookup                :: Ix a -> IxMap a b -> Maybe b
insert                :: Ix a -> b -> IxMap a b -> IxMap a b
delete                :: Ix a -> IxMap a b -> IxMap a b
insertWith            :: (b -> b -> b) -> Ix a -> b -> IxMap a b -> IxMap a b
insertWithKey         :: (Ix a -> b -> b -> b) -> Ix a -> b -> IxMap a b -> IxMap a b
insertLookupWithKey   :: (Ix a -> b -> b -> b) -> Ix a -> b -> IxMap a b -> (Maybe b, IxMap a b)
adjust                :: (b -> b) -> Ix a -> IxMap a b -> IxMap a b
update                :: (b -> Maybe b) -> Ix a -> IxMap a b -> IxMap a b
adjustWithKey         :: (Ix a -> b -> b) -> Ix a -> IxMap a b -> IxMap a b
updateWithKey         :: (Ix a -> b -> Maybe b) -> Ix a -> IxMap a b -> IxMap a b
updateLookupWithKey   :: (Ix a -> b -> Maybe b) -> Ix a -> IxMap a b -> (Maybe b, IxMap a b)
alter                 :: (Maybe b -> Maybe b) -> Ix a -> IxMap a b -> IxMap a b
fold                  :: (b -> c -> c) -> c -> IxMap a b -> c
foldWithKey           :: (Ix a -> b -> c -> c) -> c -> IxMap a b -> c
foldWithKey2          :: (Ix a -> b -> c -> d -> (# c,d #)) -> c -> d -> IxMap a b -> (# c,d #)
foldWithKey3          :: (Ix a -> b -> c -> d -> e -> (# c,d,e #)) -> c -> d -> e -> IxMap a b -> (# c,d,e #)
map                   :: (b -> c) -> IxMap a b -> IxMap a c
mapWithKey            :: (Ix a -> b -> c) -> IxMap a b -> IxMap a c
mapAccum              :: (b -> c -> (b, d)) -> b -> IxMap a c -> (b, IxMap a d)
mapAccumWithKey       :: (b -> Ix a -> c -> (b, d)) -> b -> IxMap a c -> (b, IxMap a d)
mapAccumRWithKey      :: (b -> Ix a -> c -> (b, d)) -> b -> IxMap a c -> (b, IxMap a d)
intersection          :: IxMap a b -> IxMap a c -> IxMap a b
intersectionWith      :: (b -> c -> d) -> IxMap a b -> IxMap a c -> IxMap a d
intersectionWithKey   :: (Ix a -> b -> c -> d) -> IxMap a b -> IxMap a c -> IxMap a d
unions                :: [IxMap a b] -> IxMap a b
unionsWith            :: (b -> b -> b) -> [] (IxMap a b) -> IxMap a b
union                 :: IxMap a b -> IxMap a b -> IxMap a b
unionWith             :: (b -> b -> b) -> IxMap a b -> IxMap a b -> IxMap a b
unionWithKey          :: (Ix a -> b -> b -> b) -> IxMap a b -> IxMap a b -> IxMap a b
difference            :: IxMap a b -> IxMap a c -> IxMap a b
differenceWith        :: (b -> c -> Maybe b) -> IxMap a b -> IxMap a c -> IxMap a b
differenceWithKey     :: (Ix a -> b -> c -> Maybe b) -> IxMap a b -> IxMap a c -> IxMap a b
filter                :: (b -> Bool) -> IxMap a b -> IxMap a b
filterWithKey         :: (Ix a -> b -> Bool) -> IxMap a b -> IxMap a b
split                 :: Ix a -> IxMap a b -> (IxMap a b, IxMap a b)
splitLookup           :: Ix a -> IxMap a b -> (IxMap a b, Maybe b, IxMap a b)
updateMinWithKey      :: (Ix a -> b -> b) -> IxMap a b -> IxMap a b
updateMaxWithKey      :: (Ix a -> b -> b) -> IxMap a b -> IxMap a b
maxViewWithKey        :: IxMap a b -> Maybe ((Ix a,b), IxMap a b)
minViewWithKey        :: IxMap a b -> Maybe ((Ix a,b), IxMap a b)
updateMax             :: (b -> b) -> IxMap a b -> IxMap a b
updateMin             :: (b -> b) -> IxMap a b -> IxMap a b
maxView               :: IxMap a b -> Maybe (b, IxMap a b)
minView               :: IxMap a b -> Maybe (b, IxMap a b)
deleteFindMax         :: IxMap a b -> (b, IxMap a b)
deleteFindMin         :: IxMap a b -> (b, IxMap a b)
findMin               :: IxMap a b -> (Ix a,b)
findMax               :: IxMap a b -> (Ix a,b)
minKeyWithDefault     :: Ix a -> IxMap a b -> Ix a
maxKeyWithDefault     :: Ix a -> IxMap a b -> Ix a
deleteMin             :: IxMap a b -> IxMap a b
deleteMax             :: IxMap a b -> IxMap a b
isProperSubmapOf      :: (Eq b) => IxMap a b -> IxMap a b -> Bool
isProperSubmapOfBy    :: (b -> c -> Bool) -> IxMap a b -> IxMap a c -> Bool
isSubmapOf            :: (Eq b) => IxMap a b -> IxMap a b -> Bool
isSubmapOfBy          :: (b -> c -> Bool) -> IxMap a b -> IxMap a c -> Bool
partition             :: (b -> Bool) -> IxMap a b -> (IxMap a b, IxMap a b)
partitionWithKey      :: (Ix a -> b -> Bool) -> IxMap a b -> (IxMap a b, IxMap a b)
elems                 :: IxMap a b -> [b]
keys                  :: IxMap a b -> [Ix a]
keysSet               :: IxMap a b -> IntSet
assocs                :: IxMap a b -> [(Ix a, b)]
toList                :: IxMap a b -> [(Ix a, b)]
toAscList             :: IxMap a b -> [(Ix a, b)]
fromList              :: [(Ix a, b)] -> IxMap a b
fromListWith          :: (b -> b -> b) -> [(Ix a, b)] -> IxMap a b
fromListWithKey       :: (Ix a -> b -> b -> b) -> [(Ix a, b)] -> IxMap a b
fromAscList           :: [(Ix a, b)] -> IxMap a b
fromAscListWith       :: (b -> b -> b) -> [(Ix a, b)] -> IxMap a b
fromAscListWithKey    :: (Ix a -> b -> b -> b) -> [(Ix a, b)] -> IxMap a b
fromDistinctAscList   :: [(Ix a, b)] -> IxMap a b

insertWithM           :: (Monad m) => (b -> b -> m b) -> Ix a -> b -> IxMap a b -> m (IxMap a b)
insertWithKeyM        :: (Monad m) => (Ix a -> b -> b -> m b) -> Ix a -> b -> IxMap a b -> m (IxMap a b)
updateWithKeyM        :: (Monad m) => (Ix a -> b -> m (Maybe b)) -> Ix a -> IxMap a b -> m (IxMap a b)
foldM                 :: (Monad m) => (b -> c -> m c) -> c -> IxMap a b -> m c
foldlM                :: (Monad m) => (Ix a -> b -> c -> m c) -> c -> IxMap a b -> m c
foldlM'               :: (Monad m) => (Ix a -> b -> c -> m c) -> c -> IxMap a b -> m c
mapM                  :: (Monad m) => (b -> m c) -> IxMap a b -> m (IxMap a c)
mapM_                 :: (Monad m) => (b -> m c) -> IxMap a b -> m ()
mapWithKeyM           :: (Monad m) => (Ix a -> b -> m c) -> IxMap a b -> m (IxMap a c)
mapWithKeyM_          :: (Monad m) => (Ix a -> b -> m c) -> IxMap a b -> m ()
intersectionWithM     :: (Monad m) => (b -> c -> m d) -> IxMap a b -> IxMap a c -> m (IxMap a d)
intersectionWithKeyM  :: (Monad m) => (Ix a -> b -> c -> m d) -> IxMap a b -> IxMap a c -> m (IxMap a d)
unionWithM            :: (Monad m) => (b -> b -> m b) -> IxMap a b -> IxMap a b -> m (IxMap a b)
unionWithKeyM         :: (Monad m) => (Ix a -> b -> b -> m b) -> IxMap a b -> IxMap a b -> m (IxMap a b)
differenceWithM       :: (Monad m) => (b -> c -> m (Maybe b)) -> IxMap a b -> IxMap a c -> m (IxMap a b)
differenceWithKeyM    :: (Monad m) => (Ix a -> b -> c -> m (Maybe b)) -> IxMap a b -> IxMap a c -> m (IxMap a b)
filterM               :: (Monad m) => (b -> m Bool) -> IxMap a b -> m (IxMap a b)
filterWithKeyM        :: (Monad m) => (Ix a -> b -> m Bool) -> IxMap a b -> m (IxMap a b)






(!) = unsafeCoerce (IM.!)
size = unsafeCoerce IM.size
member = unsafeCoerce IM.member
notMember = unsafeCoerce IM.notMember
findWithDefault = unsafeCoerce IM.findWithDefault
mapMaybe = unsafeCoerce IM.mapMaybe
mapMaybeWithKey = unsafeCoerce IM.mapMaybeWithKey
mapEither = unsafeCoerce IM.mapEither
mapEitherWithKey = unsafeCoerce IM.mapEitherWithKey
empty = unsafeCoerce IM.empty
singleton = unsafeCoerce IM.singleton
null = unsafeCoerce IM.null
lookup = unsafeCoerce IM.lookup
insert = unsafeCoerce IM.insert
delete = unsafeCoerce IM.delete
insertWith = unsafeCoerce IM.insertWith
insertWithKey = unsafeCoerce IM.insertWithKey
insertLookupWithKey = unsafeCoerce IM.insertLookupWithKey
adjust = unsafeCoerce IM.adjust
update = unsafeCoerce IM.update
adjustWithKey = unsafeCoerce IM.adjustWithKey
updateWithKey = unsafeCoerce IM.updateWithKey
updateLookupWithKey = unsafeCoerce IM.updateLookupWithKey
alter = unsafeCoerce IM.alter
fold = unsafeCoerce IM.fold
foldWithKey = unsafeCoerce IM.foldWithKey
foldWithKey2 = unsafeCoerce IM.foldWithKey2
foldWithKey3 = unsafeCoerce IM.foldWithKey3
map = unsafeCoerce IM.map
mapWithKey = unsafeCoerce IM.mapWithKey
mapAccum = unsafeCoerce IM.mapAccum
mapAccumWithKey = unsafeCoerce IM.mapAccumWithKey
mapAccumRWithKey = unsafeCoerce IM.mapAccumRWithKey
intersection = unsafeCoerce IM.intersection
intersectionWith = unsafeCoerce IM.intersectionWith
intersectionWithKey = unsafeCoerce IM.intersectionWithKey
unions = unsafeCoerce IM.unions
unionsWith = unsafeCoerce IM.unionsWith
union = unsafeCoerce IM.union
unionWith = unsafeCoerce IM.unionWith
unionWithKey = unsafeCoerce IM.unionWithKey
difference = unsafeCoerce IM.difference
differenceWith = unsafeCoerce IM.differenceWith
differenceWithKey = unsafeCoerce IM.differenceWithKey
filter = unsafeCoerce IM.filter
filterWithKey = unsafeCoerce IM.filterWithKey
split = unsafeCoerce IM.split
splitLookup = unsafeCoerce IM.splitLookup
updateMinWithKey = unsafeCoerce IM.updateMinWithKey
updateMaxWithKey = unsafeCoerce IM.updateMaxWithKey
maxViewWithKey = unsafeCoerce IM.maxViewWithKey
minViewWithKey = unsafeCoerce IM.minViewWithKey
updateMax = unsafeCoerce IM.updateMax
updateMin = unsafeCoerce IM.updateMin
maxView = unsafeCoerce IM.maxView
minView = unsafeCoerce IM.minView
deleteFindMax = unsafeCoerce IM.deleteFindMax
deleteFindMin = unsafeCoerce IM.deleteFindMin
findMin = unsafeCoerce IM.findMin
findMax = unsafeCoerce IM.findMax
deleteMin = unsafeCoerce IM.deleteMin
deleteMax = unsafeCoerce IM.deleteMax
minKeyWithDefault = unsafeCoerce IM.minKeyWithDefault
maxKeyWithDefault = unsafeCoerce IM.maxKeyWithDefault
isProperSubmapOf (IxMap m1) (IxMap m2)        = IM.isProperSubmapOf m1 m2
isProperSubmapOfBy f (IxMap m1) (IxMap m2)    = IM.isProperSubmapOfBy f m1 m2
isSubmapOf (IxMap m1) (IxMap m2)              = IM.isSubmapOf m1 m2
isSubmapOfBy f (IxMap m1) (IxMap m2)          = IM.isSubmapOfBy f m1 m2
partition = unsafeCoerce IM.partition
partitionWithKey = unsafeCoerce IM.partitionWithKey
elems = unsafeCoerce IM.elems
keys = unsafeCoerce IM.keys
keysSet = unsafeCoerce IM.keysSet
assocs = unsafeCoerce IM.assocs
toList = unsafeCoerce IM.toList
toAscList = unsafeCoerce IM.toAscList
fromList = unsafeCoerce IM.fromList
fromListWith = unsafeCoerce IM.fromListWith
fromListWithKey = unsafeCoerce IM.fromListWithKey
fromAscList = unsafeCoerce IM.fromAscList
fromAscListWith = unsafeCoerce IM.fromAscListWith
fromAscListWithKey = unsafeCoerce IM.fromAscListWithKey
fromDistinctAscList = unsafeCoerce IM.fromDistinctAscList
-- insertWithM = unsafeCoerce IM.insertWithM
-- insertWithKeyM = unsafeCoerce IM.insertWithKeyM
-- updateWithKeyM = unsafeCoerce IM.updateWithKeyM
-- foldM = unsafeCoerce IM.foldM
-- foldlM = unsafeCoerce IM.foldlM
-- foldlM' = unsafeCoerce IM.foldlM'
-- mapM = unsafeCoerce IM.mapM
-- mapM_ = unsafeCoerce IM.mapM_
-- mapWithKeyM = unsafeCoerce IM.mapWithKeyM
-- mapWithKeyM_ = unsafeCoerce IM.mapWithKeyM_
-- intersectionWithM = unsafeCoerce IM.intersectionWithM
-- intersectionWithKeyM = unsafeCoerce IM.intersectionWithKeyM
-- unionWithM = unsafeCoerce IM.unionWithM
-- unionWithKeyM = unsafeCoerce IM.unionWithKeyM
-- differenceWithM = unsafeCoerce IM.differenceWithM
-- differenceWithKeyM = unsafeCoerce IM.differenceWithKeyM
-- filterM = unsafeCoerce IM.filterM
-- filterWithKeyM = unsafeCoerce IM.filterWithKeyM
insertWithM f (Ix i) b (IxMap m)              = unsafeCoerce (IM.insertWithM f i b m)
insertWithKeyM f (Ix i) b (IxMap m)           = unsafeCoerce (IM.insertWithKeyM (ixFunToIntFun1 f) i b m)
updateWithKeyM f (Ix i) (IxMap m)             = unsafeCoerce (IM.updateWithKeyM (ixFunToIntFun1 f) i m)
foldM f c (IxMap m)                           = IM.foldM f c m
foldlM f c (IxMap m)                          = IM.foldlM (ixFunToIntFun1 f) c m
foldlM' f c (IxMap m)                         = IM.foldlM' (ixFunToIntFun1 f) c m
mapM f (IxMap m)                              = unsafeCoerce (IM.mapM f m)
mapM_ f (IxMap m)                             = IM.mapM_ f m
mapWithKeyM f (IxMap m)                       = unsafeCoerce (IM.mapWithKeyM (ixFunToIntFun1 f) m)
mapWithKeyM_ f (IxMap m)                      = IM.mapWithKeyM_ (ixFunToIntFun1 f) m
intersectionWithM f (IxMap m1) (IxMap m2)     = unsafeCoerce (IM.intersectionWithM f m1 m2)
intersectionWithKeyM f (IxMap m1) (IxMap m2)  = unsafeCoerce (IM.intersectionWithKeyM (ixFunToIntFun1 f) m1 m2)
unionWithM f (IxMap m1) (IxMap m2)            = unsafeCoerce (IM.unionWithM f m1 m2)
unionWithKeyM f (IxMap m1) (IxMap m2)         = unsafeCoerce (IM.unionWithKeyM (ixFunToIntFun1 f) m1 m2)
differenceWithM f (IxMap m1) (IxMap m2)       = unsafeCoerce (IM.differenceWithM f m1 m2)
differenceWithKeyM f (IxMap m1) (IxMap m2)    = unsafeCoerce (IM.differenceWithKeyM (ixFunToIntFun1 f) m1 m2)
filterM f (IxMap m)                           = unsafeCoerce (IM.filterM f m)
filterWithKeyM f (IxMap m)                    = unsafeCoerce (IM.filterWithKeyM (ixFunToIntFun1 f) m)

ixFunToIntFun1 :: (Ix a -> b) -> (Int -> b)
ixFunToIntFun1 = unsafeCoerce
{-# INLINE ixFunToIntFun1 #-}

-- (!) (IxMap m) (Ix i)                          = m IM.! i                                                                     -- IxMap a b -> Ix a -> b
-- size (IxMap m)                                = IM.size m                                                                    -- IxMap a b -> Int
-- member (Ix i) (IxMap m)                       = IM.member i m                                                                -- Ix a -> IxMap a b -> Bool
-- notMember (Ix i) (IxMap m)                    = IM.notMember i m                                                             -- Ix a -> IxMap a b -> Bool
-- findWithDefault b (Ix i) (IxMap m)            = IM.findWithDefault b i m                                                     -- b -> Ix a -> IxMap a b -> b
-- mapMaybe f (IxMap m)                          = IxMap (IM.mapMaybe f m)                                                      -- (b -> Maybe c) -> IxMap a b -> IxMap a c
-- mapMaybeWithKey f (IxMap m)                   = IxMap (IM.mapMaybeWithKey (ixFunToIntFun1 f) m)                              -- (Ix a -> b -> Maybe c) -> IxMap a b -> IxMap a c
-- mapEither f (IxMap m)                         = bothToIxMap (IM.mapEither f m)                                               -- (b -> Either c d) -> IxMap a b -> (IxMap a c, IxMap a d)
-- mapEitherWithKey f (IxMap m)                  = bothToIxMap (IM.mapEitherWithKey (ixFunToIntFun1 f) m)                       -- (Ix a -> b -> Either c d) -> IxMap a b -> (IxMap a c, IxMap a d)
-- empty                                         = IxMap IM.empty                                                               -- IxMap a b
-- singleton (Ix i) a                            = IxMap (IM.singleton i a)                                                     -- Ix a -> b -> IxMap a b
-- null (IxMap m)                                = IM.null m                                                                    -- IxMap a b -> Bool
-- lookup (Ix i) (IxMap m)                       = IM.lookup i m                                                                -- Ix a -> IxMap a b -> Maybe b
-- insert (Ix i) b (IxMap m)                     = IxMap (IM.insert i b m)                                                      -- Ix a -> b -> IxMap a b -> IxMap a b
-- delete (Ix i) (IxMap m)                       = IxMap (IM.delete i m)                                                        -- Ix a -> IxMap a b -> IxMap a b
-- insertWith f (Ix i) b (IxMap m)               = IxMap (IM.insertWith f i b m)                                                -- (b -> b -> b) -> Ix a -> b -> IxMap a b -> IxMap a b
-- insertWithKey f (Ix i) b (IxMap m)            = IxMap (IM.insertWithKey (ixFunToIntFun1 f) i b m)                            -- (Ix a -> b -> b -> b) -> Ix a -> b -> IxMap a b -> IxMap a b
-- insertLookupWithKey f (Ix i) b (IxMap m)      = sndToIxMap (IM.insertLookupWithKey (ixFunToIntFun1 f) i b m)                 -- (Ix a -> b -> b -> b) -> Ix a -> b -> IxMap a b -> (Maybe b, IxMap a b)
-- adjust f (Ix i) (IxMap m)                     = IxMap (IM.adjust f i m)                                                      -- (b -> b) -> Ix a -> IxMap a b -> IxMap a b
-- update f (Ix i) (IxMap m)                     = IxMap (IM.update f i m)                                                      -- (b -> Maybe b) -> Ix a -> IxMap a b -> IxMap a b
-- adjustWithKey f (Ix i) (IxMap m)              = IxMap (IM.adjustWithKey (ixFunToIntFun1 f) i m)                              -- (Ix a -> b -> b) -> Ix a -> IxMap a b -> IxMap a b
-- updateWithKey f (Ix i) (IxMap m)              = IxMap (IM.updateWithKey (ixFunToIntFun1 f) i m)                              -- (Ix a -> b -> Maybe b) -> Ix a -> IxMap a b -> IxMap a b
-- updateLookupWithKey f (Ix i) (IxMap m)        = sndToIxMap (IM.updateLookupWithKey (ixFunToIntFun1 f) i m)                   -- (Ix a -> b -> Maybe b) -> Ix a -> IxMap a b -> (Maybe b, IxMap a b)
-- alter f (Ix i) (IxMap m)                      = IxMap (IM.alter f i m)                                                       -- (Maybe b -> Maybe b) -> Ix a -> IxMap a b -> IxMap a b
-- fold f c (IxMap m)                            = IM.fold f c m                                                                -- (b -> c -> c) -> c -> IxMap a b -> c
-- foldWithKey f c (IxMap m)                     = IM.foldWithKey (ixFunToIntFun1 f) c m                                        -- (Ix a -> b -> c -> c) -> c -> IxMap a b -> c
-- map f (IxMap m)                               = IxMap (IM.map f m)                                                           -- (b -> c) -> IxMap a b -> IxMap a c
-- mapWithKey f (IxMap m)                        = IxMap (IM.mapWithKey (ixFunToIntFun1 f) m)                                   -- (Ix a -> b -> c) -> IxMap a b -> IxMap a c
-- mapAccum f b (IxMap m)                        = sndToIxMap (IM.mapAccum f b m)                                               -- (b -> c -> (b, d)) -> b -> IxMap a c -> (b, IxMap a d)
-- mapAccumWithKey f b (IxMap m)                 = sndToIxMap (IM.mapAccumWithKey (ixFunToIntFun2 f) b m)                       -- (b -> Ix a -> c -> (b, d)) -> b -> IxMap a c -> (b, IxMap a d)
-- mapAccumRWithKey f b (IxMap m)                = sndToIxMap (IM.mapAccumRWithKey (ixFunToIntFun2 f) b m)                      -- (b -> Ix a -> c -> (b, d)) -> b -> IxMap a c -> (b, IxMap a d)
-- intersection (IxMap m1) (IxMap m2)            = IxMap (IM.intersection m1 m2)                                                -- IxMap a b -> IxMap a b -> IxMap a b
-- intersectionWith f (IxMap m1) (IxMap m2)      = IxMap (IM.intersectionWith f m1 m2)                                          -- (b -> c -> d) -> IxMap a b -> IxMap a c -> IxMap a d
-- intersectionWithKey f (IxMap m1) (IxMap m2)   = IxMap (IM.intersectionWithKey (ixFunToIntFun1 f) m1 m2)                      -- (Ix a -> b -> c -> d) -> IxMap a b -> IxMap a c -> IxMap a d
-- unions xs                                     = IxMap (IM.unions (listToIntMap xs))                                          -- [IxMap a b] -> IxMap a b
-- unionsWith f xs                               = IxMap (IM.unionsWith f (listToIntMap xs))                                    -- (b -> b -> b) -> [IxMap a b] -> IxMap a b
-- union (IxMap m1) (IxMap m2)                   = IxMap (IM.union m1 m2)                                                       -- IxMap a b -> IxMap a b -> IxMap a b
-- unionWith f (IxMap m1) (IxMap m2)             = IxMap (IM.unionWith f m1 m2)                                                 -- (b -> b -> b) -> IxMap a b -> IxMap a b -> IxMap a b
-- unionWithKey f (IxMap m1) (IxMap m2)          = IxMap (IM.unionWithKey (ixFunToIntFun1 f) m1 m2)                             -- (Ix a -> b -> b -> b) -> IxMap a b -> IxMap a b -> IxMap a b
-- difference (IxMap m1) (IxMap m2)              = IxMap (IM.difference m1 m2)                                                  -- IxMap a b -> IxMap a c -> IxMap a b
-- differenceWith f (IxMap m1) (IxMap m2)        = IxMap (IM.differenceWith f m1 m2)                                            -- (b -> c -> Maybe b) -> IxMap a b -> IxMap a c -> IxMap a b
-- differenceWithKey f (IxMap m1) (IxMap m2)     = IxMap (IM.differenceWithKey (ixFunToIntFun1 f) m1 m2)                        -- (Ix a -> b -> c -> Maybe b) -> IxMap a b -> IxMap a c -> IxMap a b
-- filter f (IxMap m)                            = IxMap (IM.filter f m)                                                        -- (b -> Bool) -> IxMap a b -> IxMap a b
-- filterWithKey f (IxMap m)                     = IxMap (IM.filterWithKey (ixFunToIntFun1 f) m)                                -- (Ix a -> b -> Bool) -> IxMap a b -> IxMap a b
-- split (Ix i) (IxMap m)                        = bothToIxMap (IM.split i m)                                                   -- Ix a -> IxMap a b -> (IxMap a b, IxMap a b)
-- splitLookup (Ix i) (IxMap m)                  = fstTrdToIxMap (IM.splitLookup i m)                                           -- Ix a -> IxMap a b -> (IxMap a b, Maybe b, IxMap a b)
-- updateMinWithKey f (IxMap m)                  = IxMap (IM.updateMinWithKey (ixFunToIntFun1 f) m)                             -- (Ix a -> b -> b) -> IxMap a b -> IxMap a b
-- updateMaxWithKey f (IxMap m)                  = IxMap (IM.updateMaxWithKey (ixFunToIntFun1 f) m)                             -- (Ix a -> b -> b) -> IxMap a b -> IxMap a b
-- maxViewWithKey (IxMap m)                      = mbsndToIxMap' (IM.maxViewWithKey m)                                          -- IxMap a b -> Maybe ((Ix a,b), IxMap a b)
-- minViewWithKey (IxMap m)                      = mbsndToIxMap' (IM.minViewWithKey m)                                          -- IxMap a b -> Maybe ((Ix a,b), IxMap a b)
-- updateMax f (IxMap m)                         = IxMap (IM.updateMax f m)                                                     -- (b -> b) -> IxMap a b -> IxMap a b
-- updateMin f (IxMap m)                         = IxMap (IM.updateMin f m)                                                     -- (b -> b) -> IxMap a b -> IxMap a b
-- maxView (IxMap m)                             = mbsndToIxMap (IM.maxView m)                                                  -- IxMap a b -> Maybe (b, IxMap a b)
-- minView (IxMap m)                             = mbsndToIxMap (IM.minView m)                                                  -- IxMap a b -> Maybe (b, IxMap a b)
-- deleteFindMax (IxMap m)                       = sndToIxMap (IM.deleteFindMax m)                                              -- IxMap a b -> (b, IxMap a b)
-- deleteFindMin (IxMap m)                       = sndToIxMap (IM.deleteFindMin m)                                              -- IxMap a b -> (b, IxMap a b)
-- findMin (IxMap m)                             = fstToIx (IM.findMin m)                                                       -- IxMap a b -> (Ix a,b)
-- findMax (IxMap m)                             = fstToIx (IM.findMax m)                                                       -- IxMap a b -> (Ix a,b)
-- deleteMin (IxMap m)                           = IxMap (IM.deleteMin m)                                                       -- IxMap a b -> IxMap a b
-- deleteMax (IxMap m)                           = IxMap (IM.deleteMax m)                                                       -- IxMap a b -> IxMap a b
-- isProperSubmapOf (IxMap m1) (IxMap m2)        = IM.isProperSubmapOf m1 m2                                                    -- (Eq b) => IxMap a b -> IxMap a b -> Bool
-- isProperSubmapOfBy f (IxMap m1) (IxMap m2)    = IM.isProperSubmapOfBy f m1 m2                                                -- (b -> c -> Bool) -> IxMap a b -> IxMap a c -> Bool
-- isSubmapOf (IxMap m1) (IxMap m2)              = IM.isSubmapOf m1 m2                                                          -- (Eq b) => IxMap a b -> IxMap a b -> Bool
-- isSubmapOfBy f (IxMap m1) (IxMap m2)          = IM.isSubmapOfBy f m1 m2                                                      -- (b -> c -> Bool) -> IxMap a b -> IxMap a c -> Bool
-- partition f (IxMap m)                         = bothToIxMap (IM.partition f m)                                               -- (b -> Bool) -> IxMap a b -> (IxMap a b, IxMap a b)
-- partitionWithKey f (IxMap m)                  = bothToIxMap (IM.partitionWithKey (ixFunToIntFun1 f) m)                       -- (Ix a -> b -> Bool) -> IxMap a b -> (IxMap a b, IxMap a b)
-- elems (IxMap m)                               = IM.elems m                                                                   -- IxMap a b -> [b]
-- keys (IxMap m)                                = intListToIxList (IM.keys m)                                                  -- IxMap a b -> [Ix a]
-- keysSet (IxMap m)                             = IM.keysSet m                                                                 -- IxMap a b -> IntSet
-- assocs (IxMap m)                              = intAssocsToIxAssocs (IM.assocs m)                                            -- IxMap a b -> [(Ix a, b)]
-- toList (IxMap m)                              = intAssocsToIxAssocs (IM.toList m)                                            -- IxMap a b -> [(Ix a, b)]
-- toAscList (IxMap m)                           = intAssocsToIxAssocs (IM.toAscList m)                                         -- IxMap a b -> [(Ix a, b)]
-- fromList xs                                   = IxMap (IM.fromList (ixAssocsToIntAssocs xs))                                 -- [(Ix a, b)] -> IxMap a b
-- fromListWith f xs                             = IxMap (IM.fromListWith f (ixAssocsToIntAssocs xs))                           -- (b -> b -> b) -> [(Ix a, b)] -> IxMap a b
-- fromListWithKey f xs                          = IxMap (IM.fromListWithKey (ixFunToIntFun1 f) (ixAssocsToIntAssocs xs))       -- (Ix a -> b -> b -> b) -> [(Ix a, b)] -> IxMap a b
-- fromAscList xs                                = IxMap (IM.fromAscList (ixAssocsToIntAssocs xs))                              -- [(Ix a, b)] -> IxMap a b
-- fromAscListWith f xs                          = IxMap (IM.fromAscListWith f (ixAssocsToIntAssocs xs))                        -- (b -> b -> b) -> [(Ix a, b)] -> IxMap a b
-- fromAscListWithKey f xs                       = IxMap (IM.fromAscListWithKey (ixFunToIntFun1 f) (ixAssocsToIntAssocs xs))    -- (Ix a -> b -> b -> b) -> [(Ix a, b)] -> IxMap a b
-- fromDistinctAscList xs                        = IxMap (IM.fromDistinctAscList (ixAssocsToIntAssocs xs))                      -- [(Ix a, b)] -> IxMap a b
-- insertWithM f (Ix i) b (IxMap m)              = intToIxMapM (IM.insertWithM f i b m)                                         -- (Monad m) => (b -> b -> m b) -> Ix a -> b -> IxMap a b -> m (IxMap a b)
-- insertWithKeyM f (Ix i) b (IxMap m)           = intToIxMapM (IM.insertWithKeyM (ixFunToIntFun1 f) i b m)                     -- (Monad m) => (Ix a -> b -> b -> m b) -> Ix a -> b -> IxMap a b -> m (IxMap a b)
-- updateWithKeyM f (Ix i) (IxMap m)             = intToIxMapM (IM.updateWithKeyM (ixFunToIntFun1 f) i m)                       -- (Monad m) => (Ix a -> b -> m (Maybe b)) -> Ix a -> IxMap a b -> m (IxMap a b)
-- foldM f c (IxMap m)                           = IM.foldM f c m                                                               -- (Monad m) => (b -> c -> m c) -> c -> IxMap a b -> m c
-- foldlM f c (IxMap m)                          = IM.foldlM (ixFunToIntFun1 f) c m                                             -- (Monad m) => (Ix a -> b -> c -> m c) -> c -> IxMap a b -> m c
-- foldlM' f c (IxMap m)                         = IM.foldlM' (ixFunToIntFun1 f) c m                                            -- (Monad m) => (Ix a -> b -> c -> m c) -> c -> IxMap a b -> m c
-- mapM f (IxMap m)                              = intToIxMapM (IM.mapM f m)                                                    -- (Monad m) => (b -> m c) -> IxMap a b -> m (IxMap a c)
-- mapM_ f (IxMap m)                             = IM.mapM_ f m                                                                 -- (Monad m) => (b -> m c) -> IxMap a b -> m ()
-- mapWithKeyM f (IxMap m)                       = intToIxMapM (IM.mapWithKeyM (ixFunToIntFun1 f) m)                            -- (Monad m) => (Ix a -> b -> m c) -> IxMap a b -> m (IxMap a c)
-- mapWithKeyM_ f (IxMap m)                      = IM.mapWithKeyM_ (ixFunToIntFun1 f) m                                         -- (Monad m) => (Ix a -> b -> m c) -> IxMap a b -> m ()
-- intersectionWithM f (IxMap m1) (IxMap m2)     = intToIxMapM (IM.intersectionWithM f m1 m2)                                   -- (Monad m) => (b -> c -> m d) -> IxMap a b -> IxMap a c -> m (IxMap a d)
-- intersectionWithKeyM f (IxMap m1) (IxMap m2)  = intToIxMapM (IM.intersectionWithKeyM (ixFunToIntFun1 f) m1 m2)               -- (Monad m) => (Ix a -> b -> c -> m d) -> IxMap a b -> IxMap a c -> m (IxMap a d)
-- unionWithM f (IxMap m1) (IxMap m2)            = intToIxMapM (IM.unionWithM f m1 m2)                                          -- (Monad m) => (b -> b -> m b) -> IxMap a b -> IxMap a b -> m (IxMap a b)
-- unionWithKeyM f (IxMap m1) (IxMap m2)         = intToIxMapM (IM.unionWithKeyM (ixFunToIntFun1 f) m1 m2)                      -- (Monad m) => (Ix a -> b -> b -> m b) -> IxMap a b -> IxMap a b -> m (IxMap a b)
-- differenceWithM f (IxMap m1) (IxMap m2)       = intToIxMapM (IM.differenceWithM f m1 m2)                                     -- (Monad m) => (b -> c -> m (Maybe b)) -> IxMap a b -> IxMap a c -> m (IxMap a b)
-- differenceWithKeyM f (IxMap m1) (IxMap m2)    = intToIxMapM (IM.differenceWithKeyM (ixFunToIntFun1 f) m1 m2)                 -- (Monad m) => (Ix a -> b -> c -> m (Maybe b)) -> IxMap a b -> IxMap a c -> m (IxMap a b)
-- filterM f (IxMap m)                           = intToIxMapM (IM.filterM f m)                                                 -- (Monad m) => (b -> m Bool) -> IxMap a b -> m (IxMap a b)
-- filterWithKeyM f (IxMap m)                    = intToIxMapM (IM.filterWithKeyM (ixFunToIntFun1 f) m)                         -- (Monad m) => (Ix a -> b -> m Bool) -> IxMap a b -> m (IxMap a b)


-- ixAssocsToIntAssocs :: [(Ix a, b)] -> [(Int, b)]
-- intAssocsToIxAssocs :: [(Int, b)] -> [(Ix a, b)]
-- sndToIxMap :: (o, IntMap b) -> (o, IxMap a b)
-- mbsndToIxMap :: Maybe (o, IntMap b) -> Maybe (o, IxMap a b)
-- mbsndToIxMap' :: Maybe ((Int,b),IntMap b) -> Maybe ((Ix a,b), IxMap a b)
-- bothToIxMap :: (IntMap b, IntMap c) -> (IxMap a b, IxMap a c)
-- listToIntMap :: [IxMap a b] -> [IntMap b]
-- fstTrdToIxMap :: (IntMap b, Maybe b, IntMap b) -> (IxMap a b, Maybe b, IxMap a b)
-- ixFunToIntFun1 :: (Ix a -> b) -> (Int -> b)
-- ixFunToIntFun2 :: (b -> Ix a -> c) -> (b -> Int -> c)
-- fstToIx :: (Int,b) -> (Ix a,b)
-- intListToIxList :: [Int] -> [Ix a]
-- intToIxMapM :: m (IntMap b) -> m (IxMap a b)
-- intToIxMapM = unsafeCoerce
-- mbsndToIxMap = unsafeCoerce
-- mbsndToIxMap' = unsafeCoerce
-- fstToIx = unsafeCoerce
-- intListToIxList = unsafeCoerce
-- ixFunToIntFun1 = unsafeCoerce
-- ixFunToIntFun2 = unsafeCoerce
-- ixAssocsToIntAssocs = unsafeCoerce
-- intAssocsToIxAssocs = unsafeCoerce
-- sndToIxMap = unsafeCoerce
-- bothToIxMap = unsafeCoerce
-- listToIntMap = unsafeCoerce
-- fstTrdToIxMap = unsafeCoerce
{- INLINE ixAssocsToIntAssocs -}
{- INLINE intAssocsToIxAssocs -}
{- INLINE sndToIxMap -}
{- INLINE mbsndToIxMap -}
{- INLINE mbsndToIxMap' -}
{- INLINE bothToIxMap -}
{- INLINE listToIntMap -}
{- INLINE fstTrdToIxMap -}
{- INLINE ixFunToIntFun1 -}
{- INLINE ixFunToIntFun2 -}
{- INLINE fstToIx -}
{- INLINE intListToIxList -}












{-# INLINE (!) #-}
{-# INLINE size #-}
{-# INLINE member #-}
{-# INLINE notMember #-}
{-# INLINE findWithDefault #-}
{-# INLINE mapMaybe #-}
{-# INLINE mapMaybeWithKey #-}
{-# INLINE mapEither #-}
{-# INLINE mapEitherWithKey #-}
{-# INLINE empty #-}
{-# INLINE singleton #-}
{-# INLINE null #-}
{-# INLINE lookup #-}
{-# INLINE insert #-}
{-# INLINE delete #-}
{-# INLINE insertWith #-}
{-# INLINE insertWithKey #-}
{-# INLINE insertLookupWithKey #-}
{-# INLINE adjust #-}
{-# INLINE update #-}
{-# INLINE adjustWithKey #-}
{-# INLINE updateWithKey #-}
{-# INLINE updateLookupWithKey #-}
{-# INLINE alter #-}
{-# INLINE fold #-}
{-# INLINE foldWithKey #-}
{-# INLINE map #-}
{-# INLINE mapWithKey #-}
{-# INLINE mapAccum #-}
{-# INLINE mapAccumWithKey #-}
{-# INLINE mapAccumRWithKey #-}
{-# INLINE intersection #-}
{-# INLINE intersectionWith #-}
{-# INLINE intersectionWithKey #-}
{-# INLINE unions #-}
{-# INLINE unionsWith #-}
{-# INLINE union #-}
{-# INLINE unionWith #-}
{-# INLINE unionWithKey #-}
{-# INLINE difference #-}
{-# INLINE differenceWith #-}
{-# INLINE differenceWithKey #-}
{-# INLINE filter #-}
{-# INLINE filterWithKey #-}
{-# INLINE split #-}
{-# INLINE splitLookup #-}
{-# INLINE updateMinWithKey #-}
{-# INLINE updateMaxWithKey #-}
{-# INLINE maxViewWithKey #-}
{-# INLINE minViewWithKey #-}
{-# INLINE updateMax #-}
{-# INLINE updateMin #-}
{-# INLINE maxView #-}
{-# INLINE minView #-}
{-# INLINE deleteFindMax #-}
{-# INLINE deleteFindMin #-}
{-# INLINE findMin #-}
{-# INLINE findMax #-}
{-# INLINE deleteMin #-}
{-# INLINE deleteMax #-}
{-# INLINE isProperSubmapOf #-}
{-# INLINE isProperSubmapOfBy #-}
{-# INLINE isSubmapOf #-}
{-# INLINE isSubmapOfBy #-}
{-# INLINE partition #-}
{-# INLINE partitionWithKey #-}
{-# INLINE elems #-}
{-# INLINE keys #-}
{-# INLINE keysSet #-}
{-# INLINE assocs #-}
{-# INLINE toList #-}
{-# INLINE toAscList #-}
{-# INLINE fromList #-}
{-# INLINE fromListWith #-}
{-# INLINE fromListWithKey #-}
{-# INLINE fromAscList #-}
{-# INLINE fromAscListWith #-}
{-# INLINE fromAscListWithKey #-}
{-# INLINE fromDistinctAscList #-}
{-# INLINE insertWithM #-}
{-# INLINE insertWithKeyM #-}
{-# INLINE updateWithKeyM #-}
{-# INLINE foldM #-}
{-# INLINE foldlM #-}
{-# INLINE foldlM' #-}
{-# INLINE mapM #-}
{-# INLINE mapM_ #-}
{-# INLINE mapWithKeyM #-}
{-# INLINE mapWithKeyM_ #-}
{-# INLINE intersectionWithM #-}
{-# INLINE intersectionWithKeyM #-}
{-# INLINE unionWithM #-}
{-# INLINE unionWithKeyM #-}
{-# INLINE differenceWithM #-}
{-# INLINE differenceWithKeyM #-}
{-# INLINE filterM #-}
{-# INLINE filterWithKeyM #-}








{-
-- IntMap
keysSet :: IntMap a -> IntSet
(!) :: IntMap b -> Int -> b
size :: IntMap b -> Int
member :: Int -> IntMap b -> Bool
notMember :: Int -> IntMap b -> Bool
findWithDefault :: b -> Int -> IntMap b -> b
mapMaybe :: (b -> Maybe c) -> IntMap b -> IntMap c
mapMaybeWithKey :: (Int -> b -> Maybe c) -> IntMap b -> IntMap c
mapEither :: (b -> Either c d) -> IntMap b -> (IntMap c, IntMap d)
mapEitherWithKey :: (Int -> b -> Either c d) -> IntMap b -> (IntMap c, IntMap d)
empty :: IntMap b
singleton :: Int -> b -> IntMap b
null :: IntMap b -> Bool
lookup :: Int -> IntMap b -> Maybe b
insert :: Int -> b -> IntMap b -> IntMap b
delete :: Int -> IntMap b -> IntMap b
insertWith :: (b -> b -> b) -> Int -> b -> IntMap b -> IntMap b
insertWithKey :: (Int -> b -> b -> b) -> Int -> b -> IntMap b -> IntMap b
insertLookupWithKey :: (Int -> b -> b -> b) -> Int -> b -> IntMap b -> (Maybe b, IntMap b)
adjust :: (b -> b) -> Int -> IntMap b -> IntMap b
update :: (b -> Maybe b) -> Int -> IntMap b -> IntMap b
adjustWithKey :: (Int -> b -> b) -> Int -> IntMap b -> IntMap b
updateWithKey :: (Int -> b -> Maybe b) -> Int -> IntMap b -> IntMap b
updateLookupWithKey :: (Int -> b -> Maybe b) -> Int -> IntMap b -> (Maybe b, IntMap b)
alter :: (Maybe b -> Maybe b) -> Int -> IntMap b -> IntMap b
fold :: (b -> c -> c) -> c -> IntMap b -> c
foldWithKey :: (Int -> b -> c -> c) -> c -> IntMap b -> c
map :: (b -> c) -> IntMap b -> IntMap c
mapWithKey :: (Int -> b -> c) -> IntMap b -> IntMap c
mapAccum :: (b -> c -> (b, d)) -> b -> IntMap c -> (b, IntMap d)
mapAccumWithKey :: (b -> Int -> c -> (b, d)) -> b -> IntMap c -> (b, IntMap d)
mapAccumRWithKey :: (b -> Int -> c -> (b, d)) -> b -> IntMap c -> (b, IntMap d)
intersection :: IntMap b -> IntMap b -> IntMap b
intersectionWith :: (b -> c -> d) -> IntMap b -> IntMap c -> IntMap d
intersectionWithKey :: (Int -> b -> c -> d) -> IntMap b -> IntMap c -> IntMap d
unions :: [IntMap b] -> IntMap b
unionsWith :: (b -> b -> b) -> [] (IntMap b) -> IntMap b
union :: IntMap b -> IntMap b -> IntMap b
unionWith :: (b -> b -> b) -> IntMap b -> IntMap b -> IntMap b
unionWithKey :: (Int -> b -> b -> b) -> IntMap b -> IntMap b -> IntMap b
difference :: IntMap b -> IntMap c -> IntMap b
differenceWith :: (b -> c -> Maybe b) -> IntMap b -> IntMap c -> IntMap b
differenceWithKey :: (Int -> b -> c -> Maybe b) -> IntMap b -> IntMap c -> IntMap b
filter :: (b -> Bool) -> IntMap b -> IntMap b
filterWithKey :: (Int -> b -> Bool) -> IntMap b -> IntMap b
split :: Int -> IntMap b -> (IntMap b, IntMap b)
splitLookup :: Int -> IntMap b -> (IntMap b, Maybe b, IntMap b)
updateMinWithKey :: (Int -> b -> b) -> IntMap b -> IntMap b
updateMaxWithKey :: (Int -> b -> b) -> IntMap b -> IntMap b
maxViewWithKey :: IntMap b -> Maybe ((Int,b), IntMap b)
minViewWithKey :: IntMap b -> Maybe ((Int,b), IntMap b)
updateMax :: (b -> b) -> IntMap b -> IntMap b
updateMin :: (b -> b) -> IntMap b -> IntMap b
maxView :: IntMap b -> Maybe (b, IntMap b)
minView :: IntMap b -> Maybe (b, IntMap b)
deleteFindMax :: IntMap b -> (b, IntMap b)
deleteFindMin :: IntMap b -> (b, IntMap b)
findMin :: IntMap b -> (Int,b)
findMax :: IntMap b -> (Int,b)
deleteMin :: IntMap b -> IntMap b
deleteMax :: IntMap b -> IntMap b
isProperSubmapOf :: (Eq b) => IntMap b -> IntMap b -> Bool
isProperSubmapOfBy :: (b -> c -> Bool) -> IntMap b -> IntMap c -> Bool
isSubmapOf :: (Eq b) => IntMap b -> IntMap b -> Bool
isSubmapOfBy :: (b -> c -> Bool) -> IntMap b -> IntMap c -> Bool
partition :: (b -> Bool) -> IntMap b -> (IntMap b, IntMap b)
partitionWithKey :: (Int -> b -> Bool) -> IntMap b -> (IntMap b, IntMap b)
elems :: IntMap b -> [b]
keys :: IntMap b -> [Int]
keysSet :: IntMap b -> IntSet
assocs :: IntMap b -> [(Int, b)]
toList :: IntMap b -> [(Int, b)]
toAscList :: IntMap b -> [(Int, b)]
fromList :: [(Int, b)] -> IntMap b
fromListWith :: (b -> b -> b) -> [(Int, b)] -> IntMap b
fromListWithKey :: (Int -> b -> b -> b) -> [(Int, b)] -> IntMap b
fromAscList :: [(Int, b)] -> IntMap b
fromAscListWith :: (b -> b -> b) -> [(Int, b)] -> IntMap b
fromAscListWithKey :: (Int -> b -> b -> b) -> [(Int, b)] -> IntMap b
fromDistinctAscList :: [(Int, b)] -> IntMap b
showTree :: (Show b) => IntMap b -> String
showTreeWith :: (Show b) => Bool -> Bool -> IntMap b -> String

insertWithM :: (Monad m) => (b -> b -> m b) -> Int -> b -> IntMap b -> m (IntMap b)
insertWithKeyM :: (Monad m) => (Int -> b -> b -> m b) -> Int -> b -> IntMap b -> m (IntMap b)
updateWithKeyM :: (Monad m) => (Int -> b -> m (Maybe b)) -> Int -> IntMap b -> m (IntMap b)
foldM :: (Monad m) => (b -> c -> m c) -> c -> IntMap b -> m c
foldlM :: (Monad m) => (Int -> b -> c -> m c) -> c -> IntMap b -> m c
foldlM' :: (Monad m) => (Int -> b -> c -> m c) -> c -> IntMap b -> m c
mapM :: (Monad m) => (b -> m c) -> IntMap b -> m (IntMap c)
mapM_ :: (Monad m) => (b -> m c) -> IntMap b -> m ()
mapWithKeyM :: (Monad m) => (Int -> b -> m c) -> IntMap b -> m (IntMap c)
mapWithKeyM_ :: (Monad m) => (Int -> b -> m c) -> IntMap b -> m ()
intersectionWithM :: (Monad m) => (b -> c -> m d) -> IntMap b -> IntMap c -> m (IntMap d)
intersectionWithKeyM :: (Monad m) => (Int -> b -> c -> m d) -> IntMap b -> IntMap c -> m (IntMap d)
unionWithM :: (Monad m) => (b -> b -> m b) -> IntMap b -> IntMap b -> m (IntMap b)
unionWithKeyM :: (Monad m) => (Int -> b -> b -> m b) -> IntMap b -> IntMap b -> m (IntMap b)
differenceWithM :: (Monad m) => (b -> c -> m (Maybe b)) -> IntMap b -> IntMap c -> m (IntMap b)
differenceWithKeyM :: (Monad m) => (Int -> b -> c -> m (Maybe b)) -> IntMap b -> IntMap c -> m (IntMap b)
filterM :: (Monad m) => (b -> m Bool) -> IntMap b -> m (IntMap b)
filterWithKeyM :: (Monad m) => (Int -> b -> m Bool) -> IntMap b -> m (IntMap b)
-}


