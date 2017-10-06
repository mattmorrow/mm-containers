{-# LANGUAGE CPP #-}

module MM.Data.Class.UnionFind.Ix (
   UF(..)
  ,RepM(..)
  ,UFM(..)
) where

import MM.Data.Types.Ix
import MM.Data.Map.Ix(IxMap,Index)
import MM.Data.Set.Ix(IxSet)
import Data.Monoid(Monoid(..))
import MM.Data.Class.Maps(GetSetAddM)

-----------------------------------------------------------------------------

#if 0
class (Monad m) => SubsumeM m a where
  subsumeM :: Ix a -> Ix a -> m (Ix a)
class (Monad m) => UnifyM m a where
  unifyM :: Ix a -> Ix a -> m (Ix a)
  unifyM_ :: Ix a -> Ix a -> m ()
  unifyM_ i j = unifyM i j >> return ()
class (Monad m) => AntiUnifyM m a where
  antiUnifyM :: Ix a -> Ix a -> m (Ix a)
  antiUnifyM_ :: Ix a -> Ix a -> m ()
  antiUnifyM_ i j = antiUnifyM i j >> return ()
#endif

-----------------------------------------------------------------------------

class (Monad m) => RepM m a where
  repM :: Ix a -> m (Ix a)
  tryRepM :: Ix a -> m (Maybe (Ix a))
  -- repM i = do (i,_) <- findM i; return i

#if 0
class (RepM m a) => FindM m a b where
  findM :: Ix a -> m (Ix a, b)
  tryFindM :: Ix a -> m (Maybe (Ix a, b))
  tryFindM i = do
    o <- tryRepM i
    case o of
      Just i-> do
        x <- findM i
        return (Just x)
      Nothing-> return Nothing
#endif

-----------------------------------------------------------------------------

#if 0
class (Monad m) => UnionM m a where
  indM :: Ix a -> Ix a -> m ()
  unionM :: Ix a -> Ix a -> m (Ix a)
  unionToLeftM :: Ix a -> Ix a -> m (Ix a)
  unionM_ :: Ix a -> Ix a -> m ()
  unionToLeftM_ :: Ix a -> Ix a -> m ()
  unionM_ i j = unionM i j >> return ()
  unionToLeftM_ i j = unionToLeftM i j >> return ()

class (UnionM m a, FindM m a b) => UFM m a b where
  unionAndSetM :: Ix a -> Ix a -> b -> m (Ix a)
  unionToLeftAndSetM :: Ix a -> Ix a -> b -> m (Ix a)
  unionWithM :: (b -> b -> b) -> Ix a -> Ix a -> m (Ix a)
  unionWithRepM :: (Ix a -> b -> b -> b) -> Ix a -> Ix a -> m (Ix a)
  unionToLeftWithM :: (b -> b -> b) -> Ix a -> Ix a -> m (Ix a)
  unionToLeftWithRepM :: (Ix a -> b -> b -> b) -> Ix a -> Ix a -> m (Ix a)
  unionAndSetM_ :: Ix a -> Ix a -> b -> m ()
  unionToLeftAndSetM_ :: Ix a -> Ix a -> b -> m ()
  unionWithM_ :: (b -> b -> b) -> Ix a -> Ix a -> m ()
  unionWithRepM_ :: (Ix a -> b -> b -> b) -> Ix a -> Ix a -> m ()
  unionToLeftWithM_ :: (b -> b -> b) -> Ix a -> Ix a -> m ()
  unionToLeftWithRepM_ :: (Ix a -> b -> b -> b) -> Ix a -> Ix a -> m ()
  unionAndSetM_ i j a = unionAndSetM i j a >> return ()
  unionToLeftAndSetM_ i j a = unionToLeftAndSetM i j a >> return ()
  unionWithM_ f i j = unionWithM f i j >> return ()
  unionWithRepM_ f i j = unionWithRepM f i j >> return ()
  unionToLeftWithM_ f i j = unionToLeftWithM f i j >> return ()
  unionToLeftWithRepM_ f i j = unionToLeftWithRepM f i j >> return ()

class (Monad m) => OutOfDateM m a where
  getDirtyM         :: m (IxSet a)
  popDirtyM         :: m (Maybe (Ix a))
  getIndsM          :: Ix a -> m (IxSet a)
  setIndsM          :: Ix a -> IxSet a -> m ()
  clearIndsM        :: Ix a -> m ()
  getAndClearIndsM  :: Ix a -> m (IxSet a)
  mapIndsM          :: (IxSet a -> IxSet a) -> Ix a -> m ()
  getDirtyM = return mempty
  popDirtyM = return Nothing
  getIndsM _ = return mempty
  setIndsM _ _ = return ()
  clearIndsM i = setIndsM i mempty
  getAndClearIndsM i = do
    inds <- getIndsM i
    clearIndsM i
    return inds
  mapIndsM f i = do
    inds <- getIndsM i
    let !inds2 = f inds
    setIndsM i inds2
#endif

-----------------------------------------------------------------------------

-- class (FindM m a b, GetSetAddM m a b) => UFM m a b | m a -> b where

-- | .
class (RepM m a, GetSetAddM m (Ix a) b) => UFM m a b | m a -> b where
  findM :: Ix a -> m (Ix a, b)
  tryFindM :: Ix a -> m (Maybe (Ix a, b))
  indM :: Ix a -> Ix a -> m ()
  unionM :: Ix a -> Ix a -> m (Ix a)
  unionToLeftM :: Ix a -> Ix a -> m (Ix a)
  unionAndSetM :: Ix a -> Ix a -> b -> m (Ix a)
  unionToLeftAndSetM :: Ix a -> Ix a -> b -> m (Ix a)
  unionWithM :: (b -> b -> b) -> Ix a -> Ix a -> m (Ix a)
  unionWithRepM :: (Ix a -> b -> b -> b) -> Ix a -> Ix a -> m (Ix a)
  unionToLeftWithM :: (b -> b -> b) -> Ix a -> Ix a -> m (Ix a)
  unionToLeftWithRepM :: (Ix a -> b -> b -> b) -> Ix a -> Ix a -> m (Ix a)
  unionM_ :: Ix a -> Ix a -> m ()
  unionToLeftM_ :: Ix a -> Ix a -> m ()
  unionAndSetM_ :: Ix a -> Ix a -> b -> m ()
  unionToLeftAndSetM_ :: Ix a -> Ix a -> b -> m ()
  unionWithM_ :: (b -> b -> b) -> Ix a -> Ix a -> m ()
  unionWithRepM_ :: (Ix a -> b -> b -> b) -> Ix a -> Ix a -> m ()
  unionToLeftWithM_ :: (b -> b -> b) -> Ix a -> Ix a -> m ()
  unionToLeftWithRepM_ :: (Ix a -> b -> b -> b) -> Ix a -> Ix a -> m ()
  clearIndsM        :: Ix a -> m ()
  getIndsM          :: Ix a -> m (IxSet a)
  setIndsM          :: Ix a -> IxSet a -> m ()
  mapIndsM          :: (IxSet a -> IxSet a) -> Ix a -> m ()
  getAndClearIndsM  :: Ix a -> m (IxSet a)
  getDirtyM         :: m (IxSet a)
  popDirtyM         :: m (Maybe (Ix a))
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

#if 0
  supportM :: m (IxMap a (Ix a))
  collectM :: m (IxMap a (Ix a))
  quotientM :: m (IxMap a (Ix a), IxMap a b)
  compressAndIndexM :: m (IxMap a (Ix a))
  supportAndSizeM :: m (Int, IxMap a (Ix a))
  collectAndReindexFromM :: Int -> m (Int, IxMap a (Ix a))
  quotientAndReindexFromM :: Int -> m (Int, IxMap a (Ix a), IxMap a b)
#endif

-----------------------------------------------------------------------------

-- compress :: uf -> uf
-- getDirty :: uf -> IxSet a
-- getDirty _ = mempty
-- toUF :: [(Ix a, b)] -> uf
-- fromIxMap :: IxMap a b -> uf
-- singleton :: Ix a -> b -> uf
-- foldWith :: (uf -> Ix a -> b -> c -> (c, uf)) -> c -> uf -> (c, uf)
-- foldWithM :: (Monad m) => (uf -> Ix a -> b -> c -> m (c, uf)) -> c -> uf -> m (c, uf)

-----------------------------------------------------------------------------

-- elems :: uf -> [b]
-- , b -> a where

-----------------------------------------------------------------------------

class UF uf a b | uf a -> b where
  find :: uf -> Ix a -> (# Ix a, b, uf #)
  rep  :: uf -> Ix a -> (# Ix a,    uf #)
  get  :: uf -> Ix a -> (#       b, uf #)
  tryRep :: uf -> Ix a -> (# Maybe (Ix a), uf #)
  tryFind :: uf -> Ix a -> (# Maybe (Ix a, b), uf #)
  ind  :: uf -> Ix a -> Ix a -> uf
  keys :: uf -> [Ix a]
  roots :: uf -> [Ix a]
  -- dumpUF :: uf -> [(Ix a, Either (Ix a) b)]
  lookup :: uf -> Ix a -> Maybe (b, uf)
  unsafeInsert :: uf -> Ix a -> b -> uf
  insert :: uf -> Ix a -> b -> uf
  insertWith :: (b -> b -> b) -> uf -> Ix a -> b -> uf
  insertWithRep :: (Ix a -> b -> b -> b) -> uf -> Ix a -> b -> uf
  insertWithM :: (Monad m) => (b -> b -> m b) -> uf -> Ix a -> b -> m (uf)
  insertWithRepM :: (Monad m) => (Ix a -> b -> b -> m b) -> uf -> Ix a -> b -> m (uf)
  update :: (b -> b) -> uf -> Ix a -> uf
  updateWithRep :: (Ix a -> b -> b) -> uf -> Ix a -> uf
  updateM :: (Monad m) => (b -> m b) -> uf -> Ix a -> m (uf)
  updateWithRepM :: (Monad m) => (Ix a -> b -> m b) -> uf -> Ix a -> m (uf)
  fold :: (Ix a -> b -> c -> c) -> c -> uf -> (c, uf)
  foldM :: (Monad m) => (Ix a -> b -> c -> m c) -> c -> uf -> m (c, uf)
  union :: uf -> Ix a -> Ix a -> (# Ix a, uf #)
  unionWith :: (b -> b -> b) -> uf -> Ix a -> Ix a -> (# Ix a, uf #)
  unionWithRep :: (Ix a -> b -> b -> b) -> uf -> Ix a -> Ix a -> (# Ix a, uf #)
  unionAndSet :: uf -> Ix a -> Ix a -> b -> (# Ix a, uf #)
  unionAndSetWithRep :: uf -> Ix a -> Ix a -> (Ix a -> b) -> (# Ix a, uf #)
  unionToLeft :: uf -> Ix a -> Ix a -> (# Ix a, uf #)
  unionToLeftAndSet :: uf -> Ix a -> Ix a -> b -> (# Ix a, uf #)
  unionToLeftAndSetWithRep :: uf -> Ix a -> Ix a -> (Ix a -> b) -> (# Ix a, uf #)
  unionToLeftWith :: (b -> b -> b) -> uf -> Ix a -> Ix a -> (# Ix a, uf #)
  unionToLeftWithRep :: (Ix a -> b -> b -> b) -> uf -> Ix a -> Ix a -> (# Ix a, uf #)
  union_ :: uf -> Ix a -> Ix a -> uf
  unionWith_ :: (b -> b -> b) -> uf -> Ix a -> Ix a -> uf
  unionWithRep_ :: (Ix a -> b -> b -> b) -> uf -> Ix a -> Ix a -> uf
  unionAndSet_ :: uf -> Ix a -> Ix a -> b -> uf
  unionAndSetWithRep_ :: uf -> Ix a -> Ix a -> (Ix a -> b) -> uf
  unionToLeft_ :: uf -> Ix a -> Ix a -> uf
  unionToLeftAndSet_ :: uf -> Ix a -> Ix a -> b -> uf
  unionToLeftAndSetWithRep_ :: uf -> Ix a -> Ix a -> (Ix a -> b) -> uf
  unionToLeftWith_ :: (b -> b -> b) -> uf -> Ix a -> Ix a -> uf
  unionToLeftWithRep_ :: (Ix a -> b -> b -> b) -> uf -> Ix a -> Ix a -> uf

  support :: uf -> (IxMap a (Ix a), uf)
  collect :: uf -> (IxMap a (Ix a), uf)
  collect_ :: uf -> (IxMap a (Ix a), uf)
  quotient :: uf -> (IxMap a (Ix a), IxMap a b)
  quotient_ :: uf -> (IxMap a (Ix a), IxMap a b)

  -- quotient :: uf -> (IxMap a (Ix b), IxMap b b)

  compressAndIndex :: uf -> (IxMap a (Ix a), uf)
  supportAndSize :: uf -> (Int, IxMap a (Ix a), uf)
  collectAndReindexFrom :: Int -> uf -> (Int, IxMap a (Ix a), uf)

  -- quotientAndReindexFrom :: Int -> uf -> (Int, IxMap a (Ix b), IxMap b b)
  quotientAndReindexFrom :: Int -> uf -> (Int, IxMap a (Ix a), IxMap a b)

  clearInds :: uf -> Ix a -> uf
  getInds :: uf -> Ix a -> IxSet a
  setInds :: uf -> Ix a -> IxSet a -> uf
  mapInds :: (IxSet a -> IxSet a) -> uf -> Ix a -> uf
  getAndClearInds :: uf -> Ix a -> (# IxSet a, uf #)
  popDirty :: uf -> (# Maybe (Ix a), uf #)
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
