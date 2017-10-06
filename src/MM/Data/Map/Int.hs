{-# LANGUAGE CPP,BangPatterns #-}

module MM.Data.Map.Int (
   IntMap,(!),null,size,member,notMember
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
  ,maxView,minView,findMin,findMax,deleteMin
  ,deleteMax,deleteFindMin,deleteFindMax,updateMin
  ,updateMax,updateMinWithKey,updateMaxWithKey
  ,minKeyWithDefault,maxKeyWithDefault
  ,minViewWithKey,maxViewWithKey,showTree,showTreeWith
  ,insertWithM,insertWithKeyM,updateWithKeyM,foldM
  ,foldlM,foldlM',mapM,mapM_,mapWithKeyM
  ,mapWithKeyM_,intersectionWithM,intersectionWithKeyM
  ,unionWithM,unionWithKeyM,differenceWithM
  ,differenceWithKeyM,filterM,filterWithKeyM
) where

import Data.Binary
import Prelude hiding (lookup,map,filter,foldr,foldl,null,mapM,mapM_)
import Data.Bits
import qualified MM.Data.Set.Int as IntSet
import Data.Monoid (Monoid(..))
import Data.Maybe (fromMaybe)
import Control.Applicative (Applicative(pure, (<*>)), (<$>))
import Control.Monad (liftM)
import Text.Read hiding (get)
import Data.Word
import Unsafe.Coerce(unsafeCoerce)

-----------------------------------------------------------------------------

keysSet :: IntMap a -> IntSet.IntSet
keysSet m = IntSet.fromDistinctAscList (keys m)

(!) :: IntMap b -> Int -> b
(!) m k = find' k m

instance (Binary b) => Binary (IntMap b) where
  put (Nil) = putWord8 0
  put (Tip k a) = putWord8 1 >> put k >> put a
  put (Bin p m l r) = putWord8 2 >> put p >> put m >> put l >> put r
  get
    = do tag <- getWord8
         case tag of
           0 -> return Nil
           1 -> Tip <$> get <*> get
           _ -> Bin <$> get <*> get <*> get <*> get

instance Monoid (IntMap b) where
  mempty = empty
  mappend = union
  mconcat = unions

size :: IntMap b -> Int
size t
  = case t of
      Bin _ _ l r -> size l + size r
      Tip _ _ -> 1
      Nil -> 0

member :: Int -> IntMap b -> Bool
member k m
  = case lookup k m of
      Nothing -> False
      Just _ -> True

notMember :: Int -> IntMap b -> Bool
notMember k m = not $ member k m

find' :: Int -> IntMap b -> b
find' k m
  = case lookup k m of
      Nothing -> error
                   ("IntMap.find: "++show k++" is not in "++show(keys m))
      Just x -> x

findWithDefault :: b -> Int -> IntMap b -> b
findWithDefault def k m
  = case lookup k m of
      Nothing -> def
      Just x -> x

mapMaybe :: (b -> Maybe c) -> IntMap b -> IntMap c
mapMaybe f m = mapMaybeWithKey (\ _ x -> f x) m
mapMaybeWithKey :: (Int -> b -> Maybe c) -> IntMap b -> IntMap c
mapMaybeWithKey f (Bin p m l r)
  = bin p m (mapMaybeWithKey f l) (mapMaybeWithKey f r)
mapMaybeWithKey f (Tip k x)
  = case f k x of
      Just y -> Tip k y
      Nothing -> Nil
mapMaybeWithKey _ (Nil) = Nil
mapEither :: (b -> Either c d) -> IntMap b -> (IntMap c, IntMap d)
mapEither f m = mapEitherWithKey (\ _ x -> f x) m
mapEitherWithKey :: (Int -> b -> Either c d) -> IntMap b -> (IntMap c, IntMap d)
mapEitherWithKey f (Bin p m l r) = (bin p m l1 r1, bin p m l2 r2)
  where (l1, l2) = mapEitherWithKey f l
        (r1, r2) = mapEitherWithKey f r
mapEitherWithKey f (Tip k x)
  = case f k x of
      Left y -> (Tip k y, Nil)
      Right z -> (Nil, Tip k z)
mapEitherWithKey _ (Nil) = (Nil, Nil)

-----------------------------------------------------------------------------

type Nat = Word
type Prefix = Int
type Mask = Int
-- type Key = Int

natFromInt :: Int -> Nat
natFromInt i = fromIntegral i

intFromNat :: Nat -> Int
intFromNat w = fromIntegral w

-----------------------------------------------------------------------------

data IntMap b
  = Bin !Prefix
        !Mask
        !(IntMap b)
        !(IntMap b)
  | Tip !Int b
  | Nil

empty :: IntMap b
empty = Nil

singleton :: Int -> b -> IntMap b
singleton k x = Tip k x

null :: IntMap b -> Bool
null Nil  = True
null _    = False

-----------------------------------------------------------------------------

join :: Prefix -> IntMap b
     -> Prefix -> IntMap b -> IntMap b
join p1 t1 p2 t2
  | !m <- branchMask p1 p2
  , !p <- mask p1 m
  = case zero p1 m of
      True  -> Bin p m t1 t2
      False -> Bin p m t2 t1

bin :: Prefix -> Mask
    -> IntMap b
    -> IntMap b
    -> IntMap b
bin p m l r
  | Nil <- r  = l
  | Nil <- l  = r
  | otherwise = Bin p m l r

-----------------------------------------------------------------------------

zero :: Int -> Mask -> Bool
zero i m = (natFromInt i) .&. (natFromInt m) == 0

nomatch :: Int -> Prefix -> Mask -> Bool
nomatch i p m = (mask i m) /= p

match :: Int -> Prefix -> Mask -> Bool
match i p m = (mask i m) == p

mask :: Int -> Mask -> Prefix
mask i m = maskW (natFromInt i) (natFromInt m)

zeroN :: Nat -> Nat -> Bool
zeroN i m = (i .&. m) == 0

maskW :: Nat -> Nat -> Prefix
maskW i m
  | !n <- complement (m-1)
  , !l <- n`xor`m
  , !p <- i .&. l
  = intFromNat p

shorter :: Mask -> Mask -> Bool
shorter m1 m2 = natFromInt m2 < natFromInt m1

branchMask :: Prefix -> Prefix -> Mask
branchMask p1 p2
  | !n1  <- natFromInt p1
  , !n2  <- natFromInt p2
  , !n   <- n1 `xor` n2
  , !m   <- highestBitMask n
  = intFromNat m

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

lookup :: Int -> IntMap b -> Maybe b
lookup k t | !nk <- natFromInt k = lookupN nk t

lookupN :: Nat -> IntMap b -> Maybe b
lookupN k t = case t of
  Bin _ m l r
    | zeroN k (natFromInt m)  -> lookupN k l
    | otherwise               -> lookupN k r
  Tip kx x
    | k==natFromInt kx        -> Just x
    | otherwise               -> Nothing
  Nil                         -> Nothing

insert :: Int -> b -> IntMap b -> IntMap b
insert k x t = case t of
  Bin p m l r
    | nomatch k p m -> join (k) (Tip k x) p t
    | zero k m      -> Bin p m (insert k x l) r
    | otherwise     -> Bin p m l (insert k x r)
  Tip ky _
    | k==ky         -> Tip k x
    | otherwise     -> join (k) (Tip k x) (ky) t
  Nil               -> Tip k x

delete :: Int -> IntMap b -> IntMap b
delete k t = case t of
  Bin p m l r
    | nomatch k p m -> t
    | zero k m      -> bin p m (delete k l) r
    | otherwise     -> bin p m l (delete k r)
  Tip ky _
    | k==ky         -> Nil
    | otherwise     -> t
  Nil               -> Nil

-----------------------------------------------------------------------------

insertWith :: (b -> b -> b) -> Int -> b -> IntMap b -> IntMap b
insertWith f = insertWithKey (const f)

insertWithKey :: (Int -> b -> b -> b) -> Int -> b -> IntMap b -> IntMap b
insertWithKey f k x t = case t of
  Bin p m l r
    | nomatch k p m -> join (k) (Tip k x) p t
    | zero k m      -> Bin p m (insertWithKey f k x l) r
    | otherwise     -> Bin p m l (insertWithKey f k x r)
  Tip ky y
    | k == ky       -> Tip k (f k x y)
    | otherwise     -> join (k) (Tip k x) (ky) t
  Nil               -> Tip k x

insertLookupWithKey :: (Int -> b -> b -> b) -> Int -> b -> IntMap b -> (Maybe b, IntMap b)
insertLookupWithKey f k x t = go f k x t (,)
  where go f k x t cont = case t of
          Bin p m l r
            | nomatch k p m -> cont Nothing (join (k) (Tip k x) p t)
            | zero k m      -> go f k x l (\found l-> cont found (Bin p m l r))
            | otherwise     -> go f k x r (\found r-> cont found (Bin p m l r))
          Tip ky y
            | k==ky         -> cont (Just y) (Tip k (f k x y))
            | otherwise     -> cont Nothing (join (k) (Tip k x) (ky) t)
          Nil               -> cont Nothing (Tip k x)

insertWithM :: (Monad m) => (b -> b -> m b) -> Int -> b -> IntMap b -> m (IntMap b)
insertWithM f = insertWithKeyM (const f)

insertWithKeyM :: (Monad m) => (Int -> b -> b -> m b) -> Int -> b -> IntMap b -> m (IntMap b)
insertWithKeyM f k x t = case t of
  Bin p m l r
    | nomatch k p m -> return (join (k) (Tip k x) p t)
    | zero k m      -> do l <- insertWithKeyM f k x l
                          return (Bin p m l r)
    | otherwise     -> do r <- insertWithKeyM f k x r
                          return (Bin p m l r)
  Tip ky y
    | k == ky       -> do x <- f k x y
                          return (Tip k x)
    | otherwise     -> return (join (k) (Tip k x) (ky) t)
  Nil               -> return (Tip k x)

adjust :: (b -> b) -> Int -> IntMap b -> IntMap b
adjust f = adjustWithKey (const f)

update :: (b -> Maybe b) -> Int -> IntMap b -> IntMap b
update f = updateWithKey (const f)

adjustWithKey :: (Int -> b -> b) -> Int -> IntMap b -> IntMap b
adjustWithKey f k t = case t of
  Bin p m l r
    | nomatch k p m -> t
    | zero k m      -> bin p m (adjustWithKey f k l) r
    | otherwise     -> bin p m l (adjustWithKey f k r)
  Tip ky y
    | k==ky         -> Tip ky (f k y)
    | otherwise     -> t
  Nil               -> Nil

updateWithKey :: (Int -> b -> Maybe b) -> Int -> IntMap b -> IntMap b
updateWithKey f k t = case t of
  Bin p m l r
    | nomatch k p m -> t
    | zero k m      -> bin p m (updateWithKey f k l) r
    | otherwise     -> bin p m l (updateWithKey f k r)
  Tip ky y
    | k==ky         -> maybe Nil (Tip ky) (f k y)
    | otherwise     -> t
  Nil               -> Nil

updateWithKeyM :: (Monad m) => (Int -> b -> m (Maybe b)) -> Int -> IntMap b -> m (IntMap b)
updateWithKeyM f k t = case t of
  Bin p m l r
    | nomatch k p m -> return t
    | zero k m      -> do !l <- updateWithKeyM f k l
                          return (bin p m l r)
    | otherwise     -> do !r <- updateWithKeyM f k r
                          return (bin p m l r)
  Tip ky y
    | k==ky         -> do o <- f k y
                          case o of
                            Nothing-> return Nil
                            Just y-> return (Tip ky y)
    | otherwise     -> return t
  Nil               -> return Nil

updateLookupWithKey :: (Int -> b -> Maybe b) -> Int -> IntMap b -> (Maybe b, IntMap b)
updateLookupWithKey f k t = go f k t (,)
  where go f k t cont = case t of
          Bin p m l r
            | nomatch k p m -> cont Nothing t
            | zero k m      -> go f k l (\found l-> cont found (bin p m l r))
            | otherwise     -> go f k r (\found r-> cont found (bin p m l r))
          Tip ky y
            | k==ky         -> maybe (cont (Just y) Nil)
                                    (cont (Just y) . Tip ky)
                                    (f k y)
            | otherwise     -> cont Nothing t
          Nil               -> cont Nothing Nil

-- -- Should be this!
-- alter :: Maybe b -> (b -> Maybe b) -> Int -> IntMap b -> IntMap b
alter :: (Maybe b -> Maybe b) -> Int -> IntMap b -> IntMap b
alter f k t = case f Nothing of
  Nothing-> alter_fno (f . Just) k t
  Just b-> alter_fyes b (f . Just) k t

alter_fno :: (b -> Maybe b) -> Int -> IntMap b -> IntMap b
alter_fno f k t = case t of
  Bin p m l r
    | nomatch k p m -> t
    | zero k m      -> bin p m (alter_fno f k l) r
    | otherwise     -> bin p m l (alter_fno f k r)
  Tip ky y
    | k==ky         -> maybe Nil (Tip ky) (f y)
    | otherwise     -> Tip ky y
  Nil               -> Nil

alter_fyes :: b -> (b -> Maybe b) -> Int -> IntMap b -> IntMap b
alter_fyes b f k t = case t of
  Bin p m l r
    | nomatch k p m -> join (k) (Tip k b) p t
    | zero k m      -> bin p m (alter_fyes b f k l) r
    | otherwise     -> bin p m l (alter_fyes b f k r)
  Tip ky y
    | k==ky         -> maybe Nil (Tip ky) (f y)
    | otherwise     -> join (k) (Tip k b) (ky) t
  Nil               -> Tip k b

foldWithKey2 :: (Int -> b -> c -> d -> (# c,d #))
              -> c -> d -> IntMap b -> (# c,d #)
foldWithKey2 = foldl2
foldWithKey3 :: (Int -> b -> c -> d -> e -> (# c,d,e #))
              -> c -> d -> e -> IntMap b -> (# c,d,e #)
foldWithKey3 = foldl3
foldl3 :: (Int -> b -> c -> d -> e -> (# c,d,e #))
        -> c -> d -> e -> IntMap b -> (# c,d,e #)
foldl3 f c d e t = case t of
  Bin 0 m l r
    | m < 0
    , (# !c,!d,!e #) <- foldl3' f c d e l
    -> foldl3' f c d e r -- put negative numbers before.
  Bin _ _ _ _ -> foldl3' f c d e t
  Tip k x -> f k x c d e
  Nil -> (# c,d,e #)
foldl3' :: (Int -> b -> c -> d -> e -> (# c,d,e #))
         -> c -> d -> e -> IntMap b -> (# c,d,e #)
foldl3' f c d e t = case t of
  Bin _ _ l r
    | (# !c,!d,!e #) <- foldl3' f c d e r
    -> foldl3' f c d e l
  Tip k x -> f k x c d e
  Nil -> (# c,d,e #)
foldl2 :: (Int -> b -> c -> d -> (# c,d #))
        -> c -> d -> IntMap b -> (# c,d #)
foldl2 f c d t = case t of
  Bin 0 m l r
    | m < 0
    , (# !c,!d #) <- foldl2' f c d l
    -> foldl2' f c d r -- put negative numbers before.
  Bin _ _ _ _ -> foldl2' f c d t
  Tip k x -> f k x c d
  Nil -> (# c,d #)
foldl2' :: (Int -> b -> c -> d -> (# c,d #))
          -> c -> d -> IntMap b -> (# c,d #)
foldl2' f c d t = case t of
  Bin _ _ l r
    | (# !c,!d #) <- foldl2' f c d r
    -> foldl2' f c d l
  Tip k x -> f k x c d
  Nil -> (# c,d #)

-----------------------------------------------------------------------------

fold :: (b -> c -> c) -> c -> IntMap b -> c
fold f = foldl (const f)

foldM :: (Monad m) => (b -> c -> m c) -> c -> IntMap b -> m c
foldM f = foldlM (const f)

foldWithKey :: (Int -> b -> c -> c) -> c -> IntMap b -> c
foldWithKey = foldl

foldWithKeyLazy :: (Int -> b -> c -> c) -> c -> IntMap b -> c
foldWithKeyLazy = foldr

------------------------------------------------

foldlM :: (Monad m) => (Int -> b -> c -> m c) -> c -> IntMap b -> m c
foldlM f z t = case t of
  Bin 0 m l r
    | m < 0 -> do
        !z <- foldlM' f z l
        foldlM' f z r -- put negative numbers before.
  Bin _ _ _ _ -> foldlM' f z t
  Tip k x -> f k x z
  Nil -> return z

foldlM' :: (Monad m) => (Int -> b -> c -> m c) -> c -> IntMap b -> m c
foldlM' f z t = case t of
  Bin _ _ l r -> do
    !z <- foldlM' f z r
    foldlM' f z l
  Tip k x -> f k x z
  Nil -> return z

foldl :: (Int -> b -> c -> c) -> c -> IntMap b -> c
foldl f z t = case t of
  Bin 0 m l r
    | m < 0
    , !z <- foldl' f z l
    -> foldl' f z r -- put negative numbers before.
  Bin _ _ _ _ -> foldl' f z t
  Tip k x -> f k x z
  Nil -> z

foldl' :: (Int -> b -> c -> c) -> c -> IntMap b -> c
foldl' f z t = case t of
  Bin _ _ l r
    | !z <- foldl' f z r
    -> foldl' f z l
  Tip k x -> f k x z
  Nil -> z

foldr :: (Int -> b -> c -> c) -> c -> IntMap b -> c
foldr f z t = case t of
  Bin 0 m l r
    | m < 0
    -> foldr' f (foldr' f z l) r  -- put negative numbers before.
  Bin _ _ _ _ -> foldr' f z t
  Tip k x     -> f k x z
  Nil         -> z

foldr' :: (Int -> b -> c -> c) -> c -> IntMap b -> c
foldr' f z t = case t of
  Bin _ _ l r -> foldr' f (foldr' f z r) l
  Tip k x     -> f k x z
  Nil         -> z

-----------------------------------------------------------------------------

map :: (b -> c) -> IntMap b -> IntMap c
map f = mapWithKey (const f)

mapWithKey :: (Int -> b -> c) -> IntMap b -> IntMap c
mapWithKey f t = case t of
  Bin p m l r
    | !l <- mapWithKey f l
    , !r <- mapWithKey f r
    -> Bin p m l r
  Tip k x -> Tip k (f k x)
  Nil -> Nil

mapM :: (Monad m) => (b -> m c) -> IntMap b -> m (IntMap c)
mapM f = mapWithKeyM (const f)

mapM_ :: (Monad m) => (b -> m c) -> IntMap b -> m ()
mapM_ f = mapWithKeyM_ (const f)

mapWithKeyM :: (Monad m) => (Int -> b -> m c) -> IntMap b -> m (IntMap c)
mapWithKeyM f t = case t of
  Bin p m l r -> do
    !l <- mapWithKeyM f l
    !r <- mapWithKeyM f r
    return (Bin p m l r)
  Tip k x -> do
    x <- f k x
    return (Tip k x)
  Nil -> return Nil

mapWithKeyM_ :: (Monad m) => (Int -> b -> m c) -> IntMap b -> m ()
mapWithKeyM_ f t = case t of
  Bin p m l r -> do
    mapWithKeyM_ f l
    mapWithKeyM_ f r
  Tip k x -> do
    _ <- f k x
    return ()
  Nil -> return ()

mapAccum :: (b -> c -> (b, d)) -> b -> IntMap c -> (b, IntMap d)
mapAccum f = mapAccumWithKey (\a _ x -> f a x)

mapAccumWithKey :: (b -> Int -> c -> (b, d)) -> b -> IntMap c -> (b, IntMap d)
mapAccumWithKey = mapAccumL

mapAccumL :: (b -> Int -> c -> (b, d)) -> b -> IntMap c -> (b, IntMap d)
mapAccumL f a t = case t of
  Bin p m l r -> let (a1, l') = mapAccumL f a l
                     (a2, r') = mapAccumL f a1 r
                  in (a2, Bin p m l' r')
  Tip k x -> let (a', x') = f a k x in (a', Tip k x')
  Nil -> (a, Nil)

mapAccumRWithKey :: (b -> Int -> c -> (b, d)) -> b -> IntMap c -> (b, IntMap d)
mapAccumRWithKey f a t = case t of
  Bin p m l r -> let  (a1, r') = mapAccumRWithKey f a r
                      (a2, l') = mapAccumRWithKey f a1 l
                  in (a2, Bin p m l' r')
  Tip k x -> let (a', x') = f a k x in (a', Tip k x')
  Nil -> (a, Nil)

-----------------------------------------------------------------------------

intersections :: [IntMap b] -> IntMap b
intersections [] = empty
intersections (x:xs) = foldlStrict intersection x xs

intersection :: IntMap b -> IntMap c -> IntMap b
intersection t1@(Bin p1 m1 l1 r1) t2@(Bin p2 m2 l2 r2)
  | shorter m1 m2 = case () of
      _ | nomatch p2 p1 m1 -> Nil
        | zero p2 m1       -> intersection l1 t2
        | otherwise             -> intersection r1 t2
  | shorter m2 m1 = case () of
      _ | nomatch p1 p2 m2 -> Nil
        | zero p1 m2       -> intersection t1 l2
        | otherwise             -> intersection t1 r2
  | p1==p2
  , !l <- intersection l1 l2
  , !r <- intersection r1 r2    = bin p1 m1 l r
  | otherwise                   = Nil
intersection t1@(Tip k _) t2
  | member k t2                 = t1
  | otherwise                   = Nil
intersection t (Tip k _)
  | Just y <- lookup k t        = Tip k y
  | otherwise                   = Nil
intersection Nil _              = Nil
intersection _ Nil              = Nil

intersectionWith :: (b -> c -> d) -> IntMap b -> IntMap c -> IntMap d
intersectionWith f = intersectionWithKey (const f)

intersectionWithM :: (Monad m) => (b -> c -> m d) -> IntMap b -> IntMap c -> m (IntMap d)
intersectionWithM f = intersectionWithKeyM (const f)

intersectionWithKey :: (Int -> b -> c -> d) -> IntMap b -> IntMap c -> IntMap d
intersectionWithKey f t1@(Bin p1 m1 l1 r1) t2@(Bin p2 m2 l2 r2)
  | shorter m1 m2 = case () of
      _ | nomatch p2 p1 m1       -> Nil
        | zero p2 m1             -> intersectionWithKey f l1 t2
        | otherwise                   -> intersectionWithKey f r1 t2
  | shorter m2 m1 = case () of
      _ | nomatch p1 p2 m2       -> Nil
        | zero p1 m2             -> intersectionWithKey f t1 l2
        | otherwise                   -> intersectionWithKey f t1 r2
  | p1==p2
  , !l <- intersectionWithKey f l1 l2
  , !r <- intersectionWithKey f r1 r2 = bin p1 m1 l r
  | otherwise                         = Nil
intersectionWithKey f (Tip k x) t2
  | Just y <- lookup k t2             = Tip k (f k x y)
  | otherwise                         = Nil
intersectionWithKey f t1 (Tip k y)
  | Just x <- lookup k t1             = Tip k (f k x y)
  | otherwise                         = Nil
intersectionWithKey _ Nil _           = Nil
intersectionWithKey _ _ Nil           = Nil

intersectionWithKeyM :: (Monad m) => (Int -> b -> c -> m d) -> IntMap b -> IntMap c -> m (IntMap d)
intersectionWithKeyM f t1@(Bin p1 m1 l1 r1) t2@(Bin p2 m2 l2 r2)
  | shorter m1 m2 = case () of
      _ | nomatch p2 p1 m1         -> return Nil
        | zero p2 m1               -> intersectionWithKeyM f l1 t2
        | otherwise                     -> intersectionWithKeyM f r1 t2
  | shorter m2 m1 = case () of
      _ | nomatch p1 p2 m2         -> return Nil
        | zero p1 m2               -> intersectionWithKeyM f t1 l2
        | otherwise                     -> intersectionWithKeyM f t1 r2
  | p1==p2                              = do !l <- intersectionWithKeyM f l1 l2
                                             !r <- intersectionWithKeyM f r1 r2
                                             return (bin p1 m1 l r)
  | otherwise                           = return Nil
intersectionWithKeyM f (Tip k x) t2
  | Just y <- lookup k t2               = do x <- f k x y
                                             return (Tip k x)
  | otherwise                           = return Nil
intersectionWithKeyM f t1 (Tip k y)
  | Just x <- lookup k t1               = do x <- f k x y
                                             return (Tip k x)
  | otherwise                           = return Nil
intersectionWithKeyM _ Nil _            = return Nil
intersectionWithKeyM _ _ Nil            = return Nil

-----------------------------------------------------------------------------

unions :: [IntMap b] -> IntMap b
unions xs = foldlStrict union empty xs

unionsWith :: (b -> b -> b) -> [] (IntMap b) -> IntMap b
unionsWith f ts = foldlStrict (unionWith f) empty ts

union :: IntMap b -> IntMap b -> IntMap b
union t1@(Bin p1 m1 l1 r1) t2@(Bin p2 m2 l2 r2)
  | shorter m1 m2 = case () of
      _   | nomatch p2 p1 m1 -> join p1 t1 p2 t2
          | zero p2 m1
          , !l <- union l1 t2     -> Bin p1 m1 l r1
          | !r <- union r1 t2     -> Bin p1 m1 l1 r
  | shorter m2 m1 = case () of
      _ | nomatch p1 p2 m2   -> join p1 t1 p2 t2
        | zero p1 m2
        , !l <- union t1 l2       -> Bin p2 m2 l r2
        | !r <- union t1 r2       -> Bin p2 m2 l2 r
  | p1==p2
  , !l <- union l1 l2
  , !r <- union r1 r2             = Bin p1 m1 l r
  | otherwise                     = join p1 t1 p2 t2
union (Tip k x) t                 = insert k x t
union t (Tip k x)                 = insertWith (flip const) k x t
union Nil t                       = t
union t Nil                       = t

unionWith :: (b -> b -> b) -> IntMap b -> IntMap b -> IntMap b
unionWith f = unionWithKey (const f)

unionWithM :: (Monad m) => (b -> b -> m b) -> IntMap b -> IntMap b -> m (IntMap b)
unionWithM f = unionWithKeyM (const f)

unionWithKey :: (Int -> b -> b -> b) -> IntMap b -> IntMap b -> IntMap b
unionWithKey f t1@(Bin p1 m1 l1 r1) t2@(Bin p2 m2 l2 r2)
  | shorter m1 m2 = case () of
      _   | nomatch p2 p1 m1       -> join p1 t1 p2 t2
          | zero p2 m1
          , !l <- unionWithKey f l1 t2  -> Bin p1 m1 l r1
          | !r <- unionWithKey f r1 t2  -> Bin p1 m1 l1 r
  | shorter m2 m1 = case () of
      _ | nomatch p1 p2 m2         -> join p1 t1 p2 t2
        | zero p1 m2
        , !l <- unionWithKey f t1 l2    -> Bin p2 m2 l r2
        | !r <- unionWithKey f t1 r2    -> Bin p2 m2 l2 r
  | p1==p2
  , !l <- unionWithKey f l1 l2
  , !r <- unionWithKey f r1 r2          = Bin p1 m1 l r
  | otherwise                           = join p1 t1 p2 t2
unionWithKey f (Tip k x) t              = insertWithKey f k x t
unionWithKey f t (Tip k x)              = insertWithKey (flip . f) k x t
unionWithKey _ Nil t                    = t
unionWithKey _ t Nil                    = t

unionWithKeyM :: (Monad m) => (Int -> b -> b -> m b) -> IntMap b -> IntMap b -> m (IntMap b)
unionWithKeyM f t1@(Bin p1 m1 l1 r1) t2@(Bin p2 m2 l2 r2)
  | shorter m1 m2 = case () of
      _   | nomatch p2 p1 m1              -> return (join p1 t1 p2 t2)
          | zero p2 m1                    -> do !l <- unionWithKeyM f l1 t2
                                                return (Bin p1 m1 l r1)
          | otherwise                     -> do !r <- unionWithKeyM f r1 t2
                                                return (Bin p1 m1 l1 r)
  | shorter m2 m1 = case () of
      _ | nomatch p1 p2 m2                -> return (join p1 t1 p2 t2)
        | zero p1 m2                      -> do !l <- unionWithKeyM f t1 l2
                                                return (Bin p2 m2 l r2)
        | otherwise                       -> do !r <- unionWithKeyM f t1 r2
                                                return (Bin p2 m2 l2 r)
  | p1==p2                                = do !l <- unionWithKeyM f l1 l2
                                               !r <- unionWithKeyM f r1 r2
                                               return (Bin p1 m1 l r)
  | otherwise                             = return (join p1 t1 p2 t2)
unionWithKeyM f (Tip k x) t               = insertWithKeyM f k x t
unionWithKeyM f t (Tip k x)               = insertWithKeyM (flip . f) k x t
unionWithKeyM _ Nil t                     = return t
unionWithKeyM _ t Nil                     = return t

-----------------------------------------------------------------------------

difference :: IntMap b -> IntMap c -> IntMap b
difference t1@(Bin p1 m1 l1 r1) t2@(Bin p2 m2 l2 r2)
  | shorter m1 m2 = case () of
      _ | nomatch p2 p1 m1   -> t1
        | zero p2 m1
        , !l <- difference l1 t2  -> bin p1 m1 l r1
        | !r <- difference r1 t2  -> bin p1 m1 l1 r
  | shorter m2 m1 = case () of
      _ | nomatch p1 p2 m2   -> t1
        | zero p1 m2         -> difference t1 l2
        | otherwise               -> difference t1 r2
  | p1==p2
  , !l <- difference l1 l2
  , !r <- difference r1 r2        = bin p1 m1 l r
  | otherwise                     = t1
difference t1@(Tip k x) t2
  | member k t2                   = Nil
  | otherwise                     = t1
difference Nil _                  = Nil
difference t (Tip k y)            = delete k t
difference t Nil                  = t

differenceWith :: (b -> c -> Maybe b) -> IntMap b -> IntMap c -> IntMap b
differenceWith f = differenceWithKey (const f)

differenceWithM :: (Monad m) => (b -> c -> m (Maybe b)) -> IntMap b -> IntMap c -> m (IntMap b)
differenceWithM f = differenceWithKeyM (const f)

differenceWithKey :: (Int -> b -> c -> Maybe b) -> IntMap b -> IntMap c -> IntMap b
differenceWithKey f t1@(Bin p1 m1 l1 r1) t2@(Bin p2 m2 l2 r2)
  | shorter m1 m2 = case () of
      _ | nomatch p2 p1 m1           -> t1
        | zero p2 m1
        , !l <- differenceWithKey f l1 t2 -> bin p1 m1 l r1
        | !r <- differenceWithKey f r1 t2 -> bin p1 m1 l1 r
  | shorter m2 m1 = case () of
      _ | nomatch p1 p2 m2           -> t1
        | zero p1 m2                 -> differenceWithKey f t1 l2
        | otherwise                       -> differenceWithKey f t1 r2
  | p1==p2
  , !l <- differenceWithKey f l1 l2
  , !r <- differenceWithKey f r1 r2       = bin p1 m1 l r
  | otherwise                             = t1
differenceWithKey f t1@(Tip k x) t2
  | Just y <- lookup k t2                 = maybe Nil (Tip k) (f k x y)
  | otherwise                             = t1
differenceWithKey _ Nil _                 = Nil
differenceWithKey f t (Tip k y)           = updateWithKey (\k x -> f k x y) k t
differenceWithKey _ t Nil                 = t

differenceWithKeyM :: (Monad m) => (Int -> b -> c -> m (Maybe b)) -> IntMap b -> IntMap c -> m (IntMap b)
differenceWithKeyM f t1@(Bin p1 m1 l1 r1) t2@(Bin p2 m2 l2 r2)
  | shorter m1 m2 = case () of
      _ | nomatch p2 p1 m1                  -> return t1
        | zero p2 m1                        -> do !l <- differenceWithKeyM f l1 t2
                                                  return (bin p1 m1 l r1)
        | otherwise                         -> do !r <- differenceWithKeyM f r1 t2
                                                  return (bin p1 m1 l1 r)
  | shorter m2 m1 = case () of
      _ | nomatch p1 p2 m2             -> return t1
        | zero p1 m2                   -> differenceWithKeyM f t1 l2
        | otherwise                         -> differenceWithKeyM f t1 r2
  | p1==p2                                  = do !l <- differenceWithKeyM f l1 l2
                                                 !r <- differenceWithKeyM f r1 r2
                                                 return (bin p1 m1 l r)
  | otherwise                               = return t1
differenceWithKeyM f t1@(Tip k x) t2
  | Just y <- lookup k t2                   = do o <- f k x y
                                                 case o of
                                                  Nothing-> return Nil
                                                  Just x-> return (Tip k x)
  | otherwise                               = return t1
differenceWithKeyM _ Nil _                  = return Nil
differenceWithKeyM f t (Tip k y)            = updateWithKeyM (\k x -> f k x y) k t
differenceWithKeyM _ t Nil                  = return t

-----------------------------------------------------------------------------

filter :: (b -> Bool) -> IntMap b -> IntMap b
filter p = filterWithKey (const p)

filterWithKey :: (Int -> b -> Bool) -> IntMap b -> IntMap b
filterWithKey predicate t = case t of
  Bin p m l r
    | !l <- filterWithKey predicate l
    , !r <- filterWithKey predicate r
    -> bin p m l r
  Tip k x
    | predicate k x -> t
    | otherwise     -> Nil
  Nil               -> Nil

filterM :: (Monad m) => (b -> m Bool) -> IntMap b -> m (IntMap b)
filterM p = filterWithKeyM (const p)

filterWithKeyM :: (Monad m) => (Int -> b -> m Bool) -> IntMap b -> m (IntMap b)
filterWithKeyM predicate t = case t of
  Bin p m l r -> do
    !l <- filterWithKeyM predicate l
    !r <- filterWithKeyM predicate r
    return (bin p m l r)
  Tip k x -> do
    o <- predicate k x
    case o of
      True-> return t
      False-> return Nil
  Nil -> return Nil

-----------------------------------------------------------------------------

split :: Int -> IntMap b -> (IntMap b, IntMap b)
split k t
  = case t of
      Bin _ m l r | m < 0 ->
                    (if k >= 0 then let (lt, gt) = split' k l in (union r lt, gt) else
                       let (lt, gt) = split' k r in (lt, union gt l))
                  | otherwise -> split' k t
      Tip ky _ | k > ky -> (t, Nil)
               | k < ky -> (Nil, t)
               | otherwise -> (Nil, Nil)
      Nil -> (Nil, Nil)

split' :: Int -> IntMap b -> (IntMap b, IntMap b)
split' k t
  = case t of
      Bin p m l r | nomatch k p m -> if k > p then (t, Nil) else (Nil, t)
                  | zero k m -> let (lt, gt) = split k l in (lt, union gt r)
                  | otherwise -> let (lt, gt) = split k r in (union l lt, gt)
      Tip ky _ | k > ky -> (t, Nil)
               | k < ky -> (Nil, t)
               | otherwise -> (Nil, Nil)
      Nil -> (Nil, Nil)

splitLookup :: Int -> IntMap b -> (IntMap b, Maybe b, IntMap b)
splitLookup k t
  = case t of
      Bin _ m l r | m < 0 ->
                    (if k >= 0 then
                       let (lt, found, gt) = splitLookup' k l in (union r lt, found, gt)
                       else
                       let (lt, found, gt) = splitLookup' k r in (lt, found, union gt l))
                  | otherwise -> splitLookup' k t
      Tip ky y | k > ky -> (t, Nothing, Nil)
               | k < ky -> (Nil, Nothing, t)
               | otherwise -> (Nil, Just y, Nil)
      Nil -> (Nil, Nothing, Nil)

splitLookup' :: Int -> IntMap b -> (IntMap b, Maybe b, IntMap b)
splitLookup' k t
  = case t of
      Bin p m l r | nomatch k p m ->
                    if k > p then (t, Nothing, Nil) else (Nil, Nothing, t)
                  | zero k m ->
                    let (lt, found, gt) = splitLookup k l in (lt, found, union gt r)
                  | otherwise ->
                    let (lt, found, gt) = splitLookup k r in (union l lt, found, gt)
      Tip ky y | k > ky -> (t, Nothing, Nil)
               | k < ky -> (Nil, Nothing, t)
               | otherwise -> (Nil, Just y, Nil)
      Nil -> (Nil, Nothing, Nil)

updateMinWithKey :: (Int -> b -> b) -> IntMap b -> IntMap b
updateMinWithKey f t
  = case t of
      Bin p m l r | m < 0 ->
                    let t' = updateMinWithKeyUnsigned f r in Bin p m l t'
      Bin p m l r -> let t' = updateMinWithKeyUnsigned f l in
                       Bin p m t' r
      Tip k y -> Tip k (f k y)
      Nil -> error "maxView: empty map has no maximal element"

updateMinWithKeyUnsigned ::
                         (Int -> b -> b) -> IntMap b -> IntMap b
updateMinWithKeyUnsigned f t
  = case t of
      Bin p m l r -> let t' = updateMinWithKeyUnsigned f l in
                       Bin p m t' r
      Tip k y -> Tip k (f k y)
      Nil -> error "updateMinWithKeyUnsigned Nil"

updateMaxWithKey :: (Int -> b -> b) -> IntMap b -> IntMap b
updateMaxWithKey f t
  = case t of
      Bin p m l r | m < 0 ->
                    let t' = updateMaxWithKeyUnsigned f l in Bin p m t' r
      Bin p m l r -> let t' = updateMaxWithKeyUnsigned f r in
                       Bin p m l t'
      Tip k y -> Tip k (f k y)
      Nil -> error "maxView: empty map has no maximal element"

updateMaxWithKeyUnsigned ::
                         (Int -> b -> b) -> IntMap b -> IntMap b
updateMaxWithKeyUnsigned f t
  = case t of
      Bin p m l r -> let t' = updateMaxWithKeyUnsigned f r in
                       Bin p m l t'
      Tip k y -> Tip k (f k y)
      Nil -> error "updateMaxWithKeyUnsigned Nil"

maxViewWithKey :: IntMap b -> Maybe ((Int,b), IntMap b)
maxViewWithKey t
  = case t of
      Bin p m l r | m < 0 ->
                    let (result, t') = maxViewUnsigned l in Just (result, bin p m t' r)
      Bin p m l r -> let (result, t') = maxViewUnsigned r in
                       Just (result, bin p m l t')
      Tip k y -> Just ((k, y), Nil)
      Nil -> Nothing

maxViewUnsigned :: IntMap b -> ((Int,b), IntMap b)
maxViewUnsigned t
  = case t of
      Bin p m l r -> let (result, t') = maxViewUnsigned r in
                       (result, bin p m l t')
      Tip k y -> ((k, y), Nil)
      Nil -> error "maxViewUnsigned Nil"

minViewWithKey :: IntMap b -> Maybe ((Int,b), IntMap b)
minViewWithKey t
  = case t of
      Bin p m l r | m < 0 ->
                    let (result, t') = minViewUnsigned r in Just (result, bin p m l t')
      Bin p m l r -> let (result, t') = minViewUnsigned l in
                       Just (result, bin p m t' r)
      Tip k y -> Just ((k, y), Nil)
      Nil -> Nothing

minViewUnsigned :: IntMap b -> ((Int,b), IntMap b)
minViewUnsigned t
  = case t of
      Bin p m l r -> let (result, t') = minViewUnsigned l in
                       (result, bin p m t' r)
      Tip k y -> ((k, y), Nil)
      Nil -> error "minViewUnsigned Nil"

updateMax :: (b -> b) -> IntMap b -> IntMap b
updateMax f = updateMaxWithKey (const f)

updateMin :: (b -> b) -> IntMap b -> IntMap b
updateMin f = updateMinWithKey (const f)

first :: (a -> c) -> (a, b) -> (c, b)
first f (x, y) = (f x, y)

maxView :: IntMap b -> Maybe (b, IntMap b)
maxView t = liftM (first snd) (maxViewWithKey t)

minView :: IntMap b -> Maybe (b, IntMap b)
minView t = liftM (first snd) (minViewWithKey t)

deleteFindMax :: IntMap b -> (b, IntMap b)
deleteFindMax
  = fromMaybe
      (error "deleteFindMax: empty map has no maximal element")
      . maxView

deleteFindMin :: IntMap b -> (b, IntMap b)
deleteFindMin
  = fromMaybe
      (error "deleteFindMin: empty map has no minimal element")
      . minView

findMin :: IntMap b -> (Int,b)
findMin (Nil) = error $ "findMin: empty map has no minimal element"
findMin (Tip k v) = (k, v)
findMin (Bin _ m l r)
  | m < 0 = find r
  | otherwise = find l
  where find (Tip k v) = (k, v)
        find (Bin _ _ l' _) = find l'
        find (Nil) = error "findMax Nil"

findMax :: IntMap b -> (Int,b)
findMax (Nil) = error $ "findMax: empty map has no maximal element"
findMax (Tip k v) = (k, v)
findMax (Bin _ m l r)
  | m < 0 = find l
  | otherwise = find r
  where find (Tip k v) = (k, v)
        find (Bin _ _ _ r') = find r'
        find (Nil) = error "findMax Nil"

minKeyWithDefault :: Int -> IntMap b -> Int
minKeyWithDefault dflt Nil = dflt
minKeyWithDefault _ (Tip k _) = k
minKeyWithDefault _ (Bin _ m l r)
  | m < 0 = go r
  | otherwise = go l
  where go (Tip k _) = k
        go (Bin _ _ l _) = go l
        go Nil = error "minKeyWithDefault Nil"

maxKeyWithDefault :: Int -> IntMap b -> Int
maxKeyWithDefault dflt Nil = dflt
maxKeyWithDefault _ (Tip k _) = k
maxKeyWithDefault _ (Bin _ m l r)
  | m < 0 = go l
  | otherwise = go r
  where go (Tip k _) = k
        go (Bin _ _ _ r) = go r
        go Nil = error "maxKeyWithDefault Nil"

deleteMin :: IntMap b -> IntMap b
deleteMin
  = maybe (error "deleteMin: empty map has no minimal element") snd .
      minView

deleteMax :: IntMap b -> IntMap b
deleteMax
  = maybe (error "deleteMax: empty map has no maximal element") snd .
      maxView

isProperSubmapOf :: (Eq b) => IntMap b -> IntMap b -> Bool
isProperSubmapOf m1 m2 = isProperSubmapOfBy (==) m1 m2

isProperSubmapOfBy :: (b -> c -> Bool) -> IntMap b -> IntMap c -> Bool
isProperSubmapOfBy predicate t1 t2
  = case submapCmp predicate t1 t2 of
      LT -> True
      _ -> False

submapCmp :: (b -> c -> Bool) -> IntMap b -> IntMap c -> Ordering
submapCmp predicate t1@(Bin p1 m1 l1 r1) (Bin p2 m2 l2 r2)
  | shorter m1 m2 = GT
  | shorter m2 m1 = submapCmpLt
  | p1 == p2 = submapCmpEq
  | otherwise = GT
  where submapCmpLt
          | nomatch p1 p2 m2 = GT
          | zero p1 m2 = submapCmp predicate t1 l2
          | otherwise = submapCmp predicate t1 r2
        submapCmpEq
          = case (submapCmp predicate l1 l2, submapCmp predicate r1 r2) of
              (GT, _) -> GT
              (_, GT) -> GT
              (EQ, EQ) -> EQ
              _ -> LT
submapCmp _ (Bin _ _ _ _) _ = GT
submapCmp predicate (Tip kx x) (Tip ky y)
  | (kx == ky) && predicate x y = EQ
  | otherwise = GT
submapCmp predicate (Tip k x) t
  = case lookup k t of
      Just y | predicate x y -> LT
      _ -> GT
submapCmp _ (Nil) (Nil) = EQ
submapCmp _ (Nil) _ = LT

isSubmapOf :: (Eq b) => IntMap b -> IntMap b -> Bool
isSubmapOf m1 m2 = isSubmapOfBy (==) m1 m2

isSubmapOfBy :: (b -> c -> Bool) -> IntMap b -> IntMap c -> Bool
isSubmapOfBy predicate t1@(Bin p1 m1 l1 r1) (Bin p2 m2 l2 r2)
  | shorter m1 m2 = False
  | shorter m2 m1 =
    match p1 p2 m2 &&
      (if zero p1 m2 then isSubmapOfBy predicate t1 l2 else
         isSubmapOfBy predicate t1 r2)
  | otherwise =
    (p1 == p2) &&
      isSubmapOfBy predicate l1 l2 && isSubmapOfBy predicate r1 r2
isSubmapOfBy _ (Bin _ _ _ _) _ = False
isSubmapOfBy predicate (Tip k x) t
  = case lookup k t of
      Just y -> predicate x y
      Nothing -> False
isSubmapOfBy _ (Nil) _ = True

partition :: (b -> Bool) -> IntMap b -> (IntMap b, IntMap b)
partition p m = partitionWithKey (\ _ x -> p x) m

partitionWithKey :: (Int -> b -> Bool) -> IntMap b -> (IntMap b, IntMap b)
partitionWithKey predicate t
  = case t of
      Bin p m l r -> let (l1, l2) = partitionWithKey predicate l
                         (r1, r2) = partitionWithKey predicate r
                       in (bin p m l1 r1, bin p m l2 r2)
      Tip k x | predicate k x -> (t, Nil)
              | otherwise -> (Nil, t)
      Nil -> (Nil, Nil)

elems :: IntMap b -> [b]
elems m = foldWithKey (\ _ x xs -> x : xs) [] m

keys :: IntMap b -> [Int]
keys m = foldWithKey (\ k _ ks -> k : ks) [] m

{-
keysSet :: IntMap b -> IntSet.IntSet
keysSet m = IntSet.fromDistinctAscList (keys m)
-}

assocs :: IntMap b -> [(Int, b)]
assocs m = toList m

toList :: IntMap b -> [(Int, b)]
toList t = foldWithKey (\ k x xs -> (k, x) : xs) [] t

toAscList :: IntMap b -> [(Int, b)]
toAscList t
  = let (pos, neg)
          = span (\ (k, _) -> k >= 0) (foldr (\ k x xs -> (k, x) : xs) [] t)
      in neg ++ pos

fromList :: [(Int, b)] -> IntMap b
fromList xs = foldlStrict ins empty xs
  where ins t (k, x) = insert k x t

fromListWith :: (b -> b -> b) -> [(Int, b)] -> IntMap b
fromListWith f xs = fromListWithKey (\ _ x y -> f x y) xs

fromListWithKey :: (Int -> b -> b -> b) -> [(Int, b)] -> IntMap b
fromListWithKey f xs = foldlStrict ins empty xs
  where ins t (k, x) = insertWithKey f k x t

fromAscList :: [(Int, b)] -> IntMap b
fromAscList xs = fromAscListWithKey (\ _ x _ -> x) xs

fromAscListWith :: (b -> b -> b) -> [(Int, b)] -> IntMap b
fromAscListWith f xs = fromAscListWithKey (\ _ x y -> f x y) xs

fromAscListWithKey :: (Int -> b -> b -> b) -> [(Int, b)] -> IntMap b
fromAscListWithKey _ [] = Nil
fromAscListWithKey f (x0 : xs0)
  = fromDistinctAscList (combineEq x0 xs0)
  where combineEq z [] = [z]
        combineEq z@(kz, zz) (x@(kx, xx) : xs)
          | kx == kz = let yy = f kx xx zz in combineEq (kx, yy) xs
          | otherwise = z : combineEq x xs

fromDistinctAscList :: [(Int, b)] -> IntMap b
fromDistinctAscList [] = Nil
fromDistinctAscList (z0 : zs0) = work z0 zs0 Nada
  where work (kx, vx) [] stk = finish (kx) (Tip kx vx) stk
        work (kx, vx) (z@(kz, _) : zs) stk
          = reduce z zs (branchMask (kx) (kz)) (kx) (Tip kx vx) stk

        reduce ::
               (Int,b) ->
                 [] (Int,b) -> Mask -> Prefix -> IntMap b -> Stack a b -> IntMap b
        reduce z zs _ px tx (Nada) = work z zs (Push px tx Nada)
        reduce z zs m px tx stk@(Push py ty stk')
          = let mxy = branchMask px py
                pxy = mask px mxy
              in
              if shorter m mxy then reduce z zs m pxy (Bin pxy mxy ty tx) stk'
                else work z zs (Push px tx stk)
        finish _ t (Nada) = t
        finish px tx (Push py ty stk) = finish p (join py ty px tx) stk
          where m = branchMask px py
                p = mask px m

data Stack a b
  = Push {-# UNPACK #-} !Prefix !(IntMap b) !(Stack a b)
  | Nada

foldlStrict :: (a -> b -> a) -> a -> [b] -> a
foldlStrict f z [] = z
foldlStrict f z (x:xs)
  | !z <- f z x
  = foldlStrict f z xs

-----------------------------------------------------------------------------

instance (Eq b) => Eq (IntMap b) where
  t1 == t2 = equal t1 t2
  t1 /= t2 = nequal t1 t2

equal :: (Eq b) => IntMap b -> IntMap b -> Bool
equal (Bin p1 m1 l1 r1) (Bin p2 m2 l2 r2)
  = (m1 == m2) && (p1 == p2) && (equal l1 l2) && (equal r1 r2)
equal (Tip kx x) (Tip ky y) = (kx == ky) && (x == y)
equal (Nil) (Nil) = True
equal _ _ = False

nequal :: (Eq b) => IntMap b -> IntMap b -> Bool
nequal (Bin p1 m1 l1 r1) (Bin p2 m2 l2 r2)
  = (m1 /= m2) || (p1 /= p2) || (nequal l1 l2) || (nequal r1 r2)
nequal (Tip kx x) (Tip ky y) = (kx /= ky) || (x /= y)
nequal (Nil) (Nil) = False
nequal _ _ = True

instance (Ord b) => Ord (IntMap b) where
  compare m1 m2 = compare (toList m1) (toList m2)

instance Functor IntMap where
  fmap = map

-----------------------------------------------------------------------------

#if 0
instance (Show b) => Show (IntMap b) where
  showsPrec d m
    = showParen (d > 10) $ showString "fromList " . shows (toList m)

instance (Read b) => Read (IntMap b) where
  readPrec
    = parens $
        prec 10 $
          do Ident "fromList" <- lexP
             xs <- readPrec
             return (fromList xs)
  readListPrec = readListPrecDefault
#endif

instance (Show b) => Show (IntMap b) where
  showsPrec _ Nil = showString "mempty"
  showsPrec _ o   = shows (toList o)

instance (Read b) => Read (IntMap b) where
  readPrec = (parens . choice)
    [do Ident "mempty" <- lexP; return mempty
    ,do xs <- readPrec; return (fromList xs)]
  readListPrec = readListPrecDefault

instance Show (IntMap ()) where
  showsPrec _ Nil = showString "mempty"
  showsPrec _ o   = shows (fmap fst (toList o))

instance Read (IntMap ()) where
  readPrec = (parens . choice)
    [do Ident "mempty" <- lexP; return mempty
    ,do xs <- readPrec; return (fromList (zip xs (repeat ())))]
  readListPrec = readListPrecDefault

-----------------------------------------------------------------------------

showTree :: (Show b) => IntMap b -> String
showTree s = showTreeWith True False s

showTreeWith :: (Show b) => Bool -> Bool -> IntMap b -> String
showTreeWith hang wide t
  | hang = (showsTreeHang wide [] t) ""
  | otherwise = (showsTree wide [] [] t) ""

showsTree :: (Show b) => Bool -> [] String -> [] String -> IntMap b -> ShowS
showsTree wide lbars rbars t
  = case t of
      Bin p m l r -> showsTree wide (withBar rbars) (withEmpty rbars) r .
                       showWide wide rbars .
                         showsBars lbars .
                           showString (showBin p m) .
                             showString "\n" .
                               showWide wide lbars .
                                 showsTree wide (withEmpty lbars) (withBar lbars) l
      Tip k x -> showsBars lbars .
                   showString " " .
                     shows k . showString ":=" . shows x . showString "\n"
      Nil -> showsBars lbars . showString "|\n"

showsTreeHang :: (Show b) => Bool -> [] String -> IntMap b -> ShowS
showsTreeHang wide bars t
  = case t of
      Bin p m l r -> showsBars bars .
                       showString (showBin p m) .
                         showString "\n" .
                           showWide wide bars .
                             showsTreeHang wide (withBar bars) l .
                               showWide wide bars . showsTreeHang wide (withEmpty bars) r
      Tip k x -> showsBars bars .
                   showString " " .
                     shows k . showString ":=" . shows x . showString "\n"
      Nil -> showsBars bars . showString "|\n"

showBin :: Prefix -> Mask -> String
showBin _ _ = "*"

showWide :: Bool -> [] String -> String -> String
showWide wide bars
  | wide = showString (concat (reverse bars)) . showString "|\n"
  | otherwise = id

showsBars :: [] String -> ShowS
showsBars bars
  = case bars of
      [] -> id
      _ -> showString (concat (reverse (tail bars))) . showString node

node :: String
node = "+--"

withBar, withEmpty :: [] String -> [] String
withBar bars = "|  " : bars
withEmpty bars = "   " : bars

-----------------------------------------------------------------------------
