{-# LANGUAGE CPP #-}

module MM.Data.Class.UnionFind.Int (
   UF(..)
  ,RepM(..)
  ,UFM(..)
) where

import MM.Data.Map.Int(IntMap)
import MM.Data.Set.Int(IntSet)
import Data.Monoid(Monoid(..))
import MM.Data.Class.Maps(GetSetAddM)

-----------------------------------------------------------------------------

class UF uf b | uf -> b where
  find :: uf -> Int -> (# Int, b, uf #)
  rep  :: uf -> Int -> (# Int,    uf #)
  get  :: uf -> Int -> (#      b, uf #)
  tryRep :: uf -> Int -> (# Maybe (Int), uf #)
  tryFind :: uf -> Int -> (# Maybe (Int, b), uf #)
  ind  :: uf -> Int -> Int -> uf
  keys :: uf -> [Int]
  roots :: uf -> [Int]
  lookup :: uf -> Int -> Maybe (b, uf)
  unsafeInsert :: uf -> Int -> b -> uf
  insert :: uf -> Int -> b -> uf
  insertWith :: (b -> b -> b) -> uf -> Int -> b -> uf
  insertWithRep :: (Int -> b -> b -> b) -> uf -> Int -> b -> uf
  insertWithM :: (Monad m) => (b -> b -> m b) -> uf -> Int -> b -> m (uf)
  insertWithRepM :: (Monad m) => (Int -> b -> b -> m b) -> uf -> Int -> b -> m (uf)
  update :: (b -> b) -> uf -> Int -> uf
  updateWithRep :: (Int -> b -> b) -> uf -> Int -> uf
  updateM :: (Monad m) => (b -> m b) -> uf -> Int -> m (uf)
  updateWithRepM :: (Monad m) => (Int -> b -> m b) -> uf -> Int -> m (uf)
  fold :: (Int -> b -> c -> c) -> c -> uf -> (c, uf)
  foldM :: (Monad m) => (Int -> b -> c -> m c) -> c -> uf -> m (c, uf)
  union :: uf -> Int -> Int -> (# Int, uf #)
  unionWith :: (b -> b -> b) -> uf -> Int -> Int -> (# Int, uf #)
  unionWithRep :: (Int -> b -> b -> b) -> uf -> Int -> Int -> (# Int, uf #)
  unionAndSet :: uf -> Int -> Int -> b -> (# Int, uf #)
  unionAndSetWithRep :: uf -> Int -> Int -> (Int -> b) -> (# Int, uf #)
  unionToLeft :: uf -> Int -> Int -> (# Int, uf #)
  unionToLeftAndSet :: uf -> Int -> Int -> b -> (# Int, uf #)
  unionToLeftAndSetWithRep :: uf -> Int -> Int -> (Int -> b) -> (# Int, uf #)
  unionToLeftWith :: (b -> b -> b) -> uf -> Int -> Int -> (# Int, uf #)
  unionToLeftWithRep :: (Int -> b -> b -> b) -> uf -> Int -> Int -> (# Int, uf #)
  union_ :: uf -> Int -> Int -> uf
  unionWith_ :: (b -> b -> b) -> uf -> Int -> Int -> uf
  unionWithRep_ :: (Int -> b -> b -> b) -> uf -> Int -> Int -> uf
  unionAndSet_ :: uf -> Int -> Int -> b -> uf
  unionAndSetWithRep_ :: uf -> Int -> Int -> (Int -> b) -> uf
  unionToLeft_ :: uf -> Int -> Int -> uf
  unionToLeftAndSet_ :: uf -> Int -> Int -> b -> uf
  unionToLeftAndSetWithRep_ :: uf -> Int -> Int -> (Int -> b) -> uf
  unionToLeftWith_ :: (b -> b -> b) -> uf -> Int -> Int -> uf
  unionToLeftWithRep_ :: (Int -> b -> b -> b) -> uf -> Int -> Int -> uf
  support :: uf -> (IntMap (Int), uf)
  collect :: uf -> (IntMap (Int), uf)
  collect_ :: uf -> (IntMap (Int), uf)
  quotient :: uf -> (IntMap (Int), IntMap b)
  quotient_ :: uf -> (IntMap (Int), IntMap b)
  compressAndIndex :: uf -> (IntMap (Int), uf)
  supportAndSize :: uf -> (Int, IntMap (Int), uf)
  collectAndReindexFrom :: Int -> uf -> (Int, IntMap (Int), uf)
  quotientAndReindexFrom :: Int -> uf -> (Int, IntMap (Int), IntMap b)
  clearInds :: uf -> Int -> uf
  getInds :: uf -> Int -> IntSet
  setInds :: uf -> Int -> IntSet -> uf
  mapInds :: (IntSet -> IntSet) -> uf -> Int -> uf
  getAndClearInds :: uf -> Int -> (# IntSet, uf #)
  popDirty :: uf -> (# Maybe (Int), uf #)
  popDirty uf = (# Nothing,uf #)
  getInds uf _ = mempty
  setInds uf _ _ = uf
  clearInds uf i
    = setInds uf i mempty
  mapInds f uf i
    | inds <- getInds uf i
    , !inds <- f inds
    = setInds uf i inds
  getAndClearInds uf i
    | inds <- getInds uf i
    , !uf <- clearInds uf i
    = (# inds,uf #)
  union_ uf i j | (# _,uf #) <- union uf i j = uf
  unionWith_ f uf i j | (# _,uf #) <- unionWith f uf i j = uf
  unionWithRep_ f uf i j | (# _,uf #) <- unionWithRep f uf i j = uf
  unionAndSet_ uf i j new | (# _,uf #) <- unionAndSet uf i j new = uf
  unionAndSetWithRep_ uf i j f | (# _,uf #) <- unionAndSetWithRep uf i j f = uf
  unionToLeft_ uf i j | (# _,uf #) <- unionToLeft uf i j = uf
  unionToLeftAndSet_ uf i j new | (# _,uf #) <- unionToLeftAndSet uf i j new = uf
  unionToLeftAndSetWithRep_ uf i j f | (# _,uf #) <- unionToLeftAndSetWithRep uf i j f = uf
  unionToLeftWith_ f uf i j | (# _,uf #) <- unionToLeftWith f uf i j = uf
  unionToLeftWithRep_ f uf i j | (# _,uf #) <- unionToLeftWithRep f uf i j = uf
  tryFind uf i
    | (# Just i,uf #) <- tryRep uf i
    , (# b,     uf #) <- get uf i
    = (# Just (i,b),uf #)
    | otherwise = (# Nothing,uf #)

-----------------------------------------------------------------------------

class (Monad m) => RepM m where
  repM :: Int -> m Int
  tryRepM :: Int -> m (Maybe Int)

class (RepM m, GetSetAddM m Int b) => UFM m b | m -> b where
  findM :: Int -> m (Int, b)
  tryFindM :: Int -> m (Maybe (Int, b))
  indM :: Int -> Int -> m ()
  unionM :: Int -> Int -> m (Int)
  unionToLeftM :: Int -> Int -> m (Int)
  unionAndSetM :: Int -> Int -> b -> m (Int)
  unionToLeftAndSetM :: Int -> Int -> b -> m (Int)
  unionWithM :: (b -> b -> b) -> Int -> Int -> m (Int)
  unionWithRepM :: (Int -> b -> b -> b) -> Int -> Int -> m (Int)
  unionToLeftWithM :: (b -> b -> b) -> Int -> Int -> m (Int)
  unionToLeftWithRepM :: (Int -> b -> b -> b) -> Int -> Int -> m (Int)
  unionM_ :: Int -> Int -> m ()
  unionToLeftM_ :: Int -> Int -> m ()
  unionAndSetM_ :: Int -> Int -> b -> m ()
  unionToLeftAndSetM_ :: Int -> Int -> b -> m ()
  unionWithM_ :: (b -> b -> b) -> Int -> Int -> m ()
  unionWithRepM_ :: (Int -> b -> b -> b) -> Int -> Int -> m ()
  unionToLeftWithM_ :: (b -> b -> b) -> Int -> Int -> m ()
  unionToLeftWithRepM_ :: (Int -> b -> b -> b) -> Int -> Int -> m ()
  clearIndsM        :: Int -> m ()
  getIndsM          :: Int -> m (IntSet)
  setIndsM          :: Int -> IntSet -> m ()
  mapIndsM          :: (IntSet -> IntSet) -> Int -> m ()
  getAndClearIndsM  :: Int -> m (IntSet)
  getDirtyM         :: m (IntSet)
  popDirtyM         :: m (Maybe (Int))
  tryFindM i = do
    o <- tryRepM i
    case o of
      Just i-> do
        x <- findM i
        return (Just x)
      Nothing-> return Nothing
  unionM_ i j = unionM i j >> return ()
  unionToLeftM_ i j = unionToLeftM i j >> return ()
  unionAndSetM_ i j a = unionAndSetM i j a >> return ()
  unionToLeftAndSetM_ i j a = unionToLeftAndSetM i j a >> return ()
  unionWithM_ f i j = unionWithM f i j >> return ()
  unionWithRepM_ f i j = unionWithRepM f i j >> return ()
  unionToLeftWithM_ f i j = unionToLeftWithM f i j >> return ()
  unionToLeftWithRepM_ f i j = unionToLeftWithRepM f i j >> return ()
  getDirtyM = return mempty
  popDirtyM = return Nothing
  getIndsM _ = return mempty
  setIndsM _ _ = return ()
  clearIndsM i = setIndsM i mempty
  mapIndsM f i = do
    inds <- getIndsM i
    let !inds2 = f inds
    setIndsM i inds2
  getAndClearIndsM i = do
    inds <- getIndsM i
    clearIndsM i
    return inds

-----------------------------------------------------------------------------
