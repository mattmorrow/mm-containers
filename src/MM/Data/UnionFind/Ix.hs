{-# LANGUAGE CPP #-}

module MM.Data.Quotient.UF (
   UF,U,newU

   ,joinWith

   ,ppUF,ppUF',ppU,statsMaxIndChain
  ,getInternalRep,castUF,maxKey,fromIxMap,fromList,toList,find
  ,get,rep,tryRep,ind,keys,roots,elems,singleton,lookup,unsafeInsert
  ,insert,insertWith,insertWithRep,insertWithM,insertWithRepM
  ,update,updateWithRep,updateM,updateWithRepM,fold,foldWith
  ,foldWith_,foldM,foldWithM,union,unionWith,unionWithRep
  ,unionAndSet,unionAndSetWithRep,unionToLeft,unionToLeftAndSet
  ,unionToLeftAndSetWithRep,unionToLeftWith,unionToLeftWithRep
  ,union_,unionWith_,unionWithRep_,unionAndSet_
  ,unionAndSetWithRep_,unionToLeft_,unionToLeftAndSet_
  ,unionToLeftAndSetWithRep_,unionToLeftWith_,unionToLeftWithRep_
  ,compress,compressAndIndex,support,supportAndSize,collect,collect_
  ,collectAndReindexFrom,quotient,quotientAndReindexFrom
  ,quotient_,quotient',quotientAndReindexFrom'
  ,getInds,setInds,mapInds,getAndClearInds,popDirty,clearInds
) where

import Prelude hiding (lookup)
import MM.Data.Ix.Types
import MM.Data.Ix.Map(IxMap,Index)
import MM.Data.Ix.Set(IxSet)
import qualified MM.Data.Ix.Map as Ix
import qualified MM.Data.Ix.Set as IxS
import Data.Monoid(Monoid(..))
import Data.List(foldl')
import qualified MM.Data.Class.UF as UF
import MM.Data.Class.Empty(Empty(..))
import MM.Data.Class.Lattice(Join(..))
import Unsafe.Coerce

-- import qualified MM.Control.Monad.S.U as S
-- import MM.Data.Class.UF(UFM(..))
import Control.Monad(ap)

import qualified Data.Binary as Bin
instance (Bin.Binary b) => Bin.Binary (UF a b) where
  put (UF o) = Bin.put o
  get = do o <- Bin.get; return (UF o)
instance (Bin.Binary t2) => Bin.Binary (U t1 t2) where
  put (NODE x1 x2) = Bin.putWord8 0 >> (Bin.put x1 >> Bin.put x2)
  put (IND x1) = Bin.putWord8 1 >> Bin.put x1
  get
    = Bin.getWord8 >>=
        (\ tag_ ->
           case tag_ of
             0 -> ap (ap (return NODE) Bin.get) Bin.get
             1 -> ap (return IND) Bin.get)

-----------------------------------------------------------------------------

maxKey :: UF a b -> Ix a
maxKey (UF ix) = Ix.maxKeyWithDefault (Ix 0) ix

-----------------------------------------------------------------------------

newtype UF a b = UF
  {unUF :: IxMap a (U a b)}
  deriving(Eq,Ord,Read,Show)

type Rank = Int
data U a b
  = NODE !Rank b    -- union-by-rank
  | IND !(Ix a)     -- path-compress
  deriving(Eq,Ord,Read,Show)

ind :: UF a b -> Ix a -> Ix a -> UF a b
ind uf@(UF o) from to
  | Just{} <- Ix.lookup from o
  = unionToLeft_ uf to from
  | (# to,r,b,UF o #) <- findR uf to
  , !o <- case r < 1 of
            True-> Ix.insert to (NODE 1 b) o
            False-> o
  , !o <- Ix.insert from (IND to) o
  = UF o

getInternalRep :: UF a b -> IxMap a (U a b)
getInternalRep (UF o) = o

castUF :: UF a b -> UF c b
castUF = unsafeCoerce

newU :: b -> U a b
{-# INLINE newU #-}
newU = NODE 0

uToE :: U a b -> Either (Ix a) b
{-# INLINE uToE #-}
uToE (IND i) = Left i
uToE (NODE _ b) = Right b

instance Functor (UF a) where
  fmap f (UF o) = UF (fmap (fmap f) o)

instance Functor (U a) where
  fmap f (NODE r b) = NODE r (f b)
  fmap f (IND i) = IND i

instance Empty (UF a b) where
  empty = UF empty
  isEmpty (UF o) = isEmpty o

instance Monoid (UF a b) where
  mempty = empty
  mappend a b = joinWith const a b

-----------------------------------------------------------------------------

fromIxMap :: IxMap a b -> UF a b
fromIxMap m = UF (fmap newU m)

fromList :: [(Ix a, b)] -> UF a b
fromList xs = UF (foldl' (\o (i,b)->Ix.insert i (newU b) o) mempty xs)

toList :: UF a b -> [(Ix a, Either (Ix a) b)]
toList (UF o) = Ix.toList (fmap uToE o)

-----------------------------------------------------------------------------

find :: UF a b -> Ix a -> (# Ix a, b, UF a b #)
rep  :: UF a b -> Ix a -> (# Ix a,    UF a b #)
get  :: UF a b -> Ix a -> (#       b, UF a b #)
{- INLINE find -}
{- INLINE rep -}
{- INLINE get -}
find o i | (# i,_,b,uf #) <- findR o i = (# i,b,uf #)
rep  o i | (# i,_,_,uf #) <- findR o i = (# i,  uf #)
get  o i | (# _,_,b,uf #) <- findR o i = (#   b,uf #)

-----------------------------------------------------------------------------

keys :: UF a b -> [Ix a]
keys (UF o) = Ix.keys o

roots :: UF a b -> [Ix a]
roots (UF o) = [i | (i,NODE{}) <- Ix.toList o]

elems :: UF a b -> [b]
elems (UF o) = [b | NODE _ b <- Ix.elems o]

singleton :: Ix a -> b -> UF a b
singleton i b = UF (Ix.singleton i (newU b))

-----------------------------------------------------------------------------

lookup :: UF a b -> Ix a -> Maybe (b, UF a b)
lookup uf@(UF o) i =
  case Ix.lookup i o of
    Nothing-> Nothing
    Just (NODE _ b)-> Just (b,uf)
    Just (IND{})
      | (# b,uf #) <- get uf i
      -> Just (b,uf)

unsafeInsert :: UF a b -> Ix a -> b -> UF a b
unsafeInsert (UF o) i new
  | !node <- newU new
  = UF (Ix.insert i node o)

insert :: UF a b -> Ix a -> b -> UF a b
insert uf@(UF o) i new =
  case Ix.lookup i o of
    Nothing
      | n <- newU new -> UF (Ix.insert i n o)
    Just (NODE r _)-> UF (Ix.insert i (NODE r new) o)
    Just (IND{})
      | (# i,r,_,UF o #) <- findR uf i
      -> UF (Ix.insert i (NODE r new) o)

insertWith :: (b -> b -> b) -> UF a b -> Ix a -> b -> UF a b
insertWith f uf@(UF o) i new =
  case Ix.lookup i o of
    Nothing
      | n <- newU new -> UF (Ix.insert i n o)
    Just (NODE r old)
      | b <- f new old
      -> UF (Ix.insert i (NODE r b) o)
    Just (IND{})
      | (# i,r,old,UF o #) <- findR uf i
      , b <- f new old
      -> UF (Ix.insert i (NODE r b) o)

insertWithRep :: (Ix a -> b -> b -> b) -> UF a b -> Ix a -> b -> UF a b
insertWithRep f uf@(UF o) i new =
  case Ix.lookup i o of
    Nothing
      | n <- newU new -> UF (Ix.insert i n o)
    Just (NODE r old)
      | b <- f i new old
      -> UF (Ix.insert i (NODE r b) o)
    Just (IND{})
      | (# i,r,old,UF o #) <- findR uf i
      , b <- f i new old
      -> UF (Ix.insert i (NODE r b) o)

insertWithM :: (Monad m) => (b -> b -> m b) -> UF a b -> Ix a -> b -> m (UF a b)
insertWithM f uf@(UF o) i new =
  case Ix.lookup i o of
    Nothing
      | n <- newU new
      , !uf <- UF (Ix.insert i n o)
      -> return uf
    Just (NODE r old)-> do
      b <- f new old
      let !uf2 = UF (Ix.insert i (NODE r b) o)
      return uf2
    Just (IND{})
      | (# i,r,old,UF o #) <- findR uf i
      -> do b <- f new old
            let !uf2 = UF (Ix.insert i (NODE r b) o)
            return uf2

insertWithRepM :: (Monad m) => (Ix a -> b -> b -> m b) -> UF a b -> Ix a -> b -> m (UF a b)
insertWithRepM f uf@(UF o) i new =
  case Ix.lookup i o of
    Nothing
      | n <- newU new
      , !uf <- UF (Ix.insert i n o)
      -> return uf
    Just (NODE r old)-> do
      b <- f i new old
      let !uf2 = UF (Ix.insert i (NODE r b) o)
      return uf2
    Just (IND{})
      | (# i,r,old,UF o #) <- findR uf i
      -> do b <- f i new old
            let !uf2 = UF (Ix.insert i (NODE r b) o)
            return uf2

update :: (b -> b) -> UF a b -> Ix a -> UF a b
update f uf i
  | (# i,r,b,UF o #) <- findR uf i
  , b <- f b
  = UF (Ix.insert i (NODE r b) o)

updateWithRep :: (Ix a -> b -> b) -> UF a b -> Ix a -> UF a b
updateWithRep f uf i
  | (# i,r,b,UF o #) <- findR uf i
  , b <- f i b
  = UF (Ix.insert i (NODE r b) o)

updateM :: (Monad m) => (b -> m b) -> UF a b -> Ix a -> m (UF a b)
updateM f uf i
  | (# i,r,b,UF o #) <- findR uf i = do
      b <- f b
      let !uf2 = UF (Ix.insert i (NODE r b) o)
      return uf2

updateWithRepM :: (Monad m) => (Ix a -> b -> m b) -> UF a b -> Ix a -> m (UF a b)
updateWithRepM f uf i
  | (# i,r,b,UF o #) <- findR uf i = do
      b <- f i b
      let !uf2 = UF (Ix.insert i (NODE r b) o)
      return uf2

-----------------------------------------------------------------------------

fold :: (Ix a -> b -> c -> c) -> c -> UF a b -> (c, UF a b)
fold f c uf@(UF o) = go mempty uf c (Ix.keys o)
  where go seen uf c [] = (c, uf)
        go seen uf c (i:is)
          | i`IxS.member`seen
          = go seen uf c is
          | (# j,b,!uf #) <- find uf i
          = case i==j of
              True
                | !c  <- f i b c
                -> go seen uf c is
              False
                | !seen <- j`IxS.insert`seen
                , !c  <- f j b c
                -> go seen uf c is

foldWith :: (UF a b -> Ix a -> b -> c -> (c, UF a b)) -> c -> UF a b -> (c, UF a b)
foldWith f c uf@(UF o) = go mempty uf c (Ix.keys o)
  where go seen uf c [] = (c,uf)
        go seen uf c (i:is)
          | i`IxS.member`seen
          = go seen uf c is
          | (# j,b,!uf #) <- find uf i
          = case i==j of
              True
                | (!c,!uf) <- f uf i b c
                -> go seen uf c is
              False
                | !seen <- j`IxS.insert`seen
                , (!c,!uf) <- f uf j b c
                -> go seen uf c is

foldWith_ :: (UF a b -> Ix a -> b -> UF a b) -> UF a b -> UF a b
foldWith_ f uf@(UF o) = go mempty uf (Ix.keys o)
  where go seen uf [] = uf
        go seen uf (i:is)
          | i`IxS.member`seen
          = go seen uf is
          | (# j,b,!uf #) <- find uf i
          = case i==j of
              True
                | !uf <- f uf i b
                -> go seen uf is
              False
                | !seen <- j`IxS.insert`seen
                , !uf <- f uf j b
                -> go seen uf is

foldM :: (Monad m) => (Ix a -> b -> c -> m c) -> c -> UF a b -> m (c, UF a b)
{- INLINE foldM -}
foldM f c uf@(UF o) = go mempty uf c (Ix.keys o)
  where go seen uf c [] = return (c, uf)
        go seen uf c (i:is)
          | i`IxS.member`seen
          = go seen uf c is
          | (# j,b,!uf #) <- find uf i
          = case i==j of
              True-> do
                !c  <- f i b c
                go seen uf c is
              False
                | !seen <- j`IxS.insert`seen-> do
                  !c  <- f j b c
                  go seen uf c is

foldWithM :: (Monad m) => (UF a b -> Ix a -> b -> c -> m (c, UF a b)) -> c -> UF a b -> m (c, UF a b)
{- INLINE foldWithM -}
foldWithM f c uf@(UF o) = go mempty uf c (Ix.keys o)
  where go seen uf acc [] = return (c,uf)
        go seen uf acc (i:is)
          | i`IxS.member`seen
          = go seen uf c is
          | (# j,b,!uf #) <- find uf i
          = case i==j of
              True-> do
                  !(!c,!uf) <- f uf i b c
                  go seen uf c is
              False
                | !seen <- j`IxS.insert`seen-> do
                  !(!c,!uf) <- f uf j b c
                  go seen uf c is

-----------------------------------------------------------------------------

findR :: UF a b -> Ix a -> (# Ix a, Rank, b, UF a b #)
{- INLINE findR -}
findR uf@(UF o) i =
  case o Ix.! i of
    IND j->
      case o Ix.! j of
        IND k-> go [j,i] k
        NODE r s-> (# j,r,s,uf #)
    NODE r s -> (# i,r,s,uf #)
  where go acc i =
          case o Ix.! i of
            IND j-> go (i:acc) j
            NODE r s
              | ind <- IND i
              , !o  <- foldl'
                  (\o j-> Ix.insert j ind o)
                   (o) (acc)
              -> (# i,r,s,UF o #)

tryRep :: UF a b -> Ix a -> (# Maybe (Ix a), UF a b #)
tryRep uf@(UF o) i =
  case Ix.lookup i o of
    Nothing-> (# Nothing, uf #)
    Just(IND j)->
      case o Ix.! j of
        IND k-> go [j,i] k
        NODE{}-> (# Just j,uf #)
    Just(NODE{})-> (# Just i,uf #)
  where go acc i =
          case o Ix.! i of
            IND j-> go (i:acc) j
            NODE r s
              | ind <- IND i
              , !o  <- foldl'
                  (\o j-> Ix.insert j ind o)
                   (o) (acc)
              -> (# Just i,UF o #)

-----------------------------------------------------------------------------

union_ :: UF a b -> Ix a -> Ix a -> UF a b
unionWith_ :: (b -> b -> b) -> UF a b -> Ix a -> Ix a -> UF a b
unionWithRep_ :: (Ix a -> b -> b -> b) -> UF a b -> Ix a -> Ix a -> UF a b
unionAndSet_ :: UF a b -> Ix a -> Ix a -> b -> UF a b
unionAndSetWithRep_ :: UF a b -> Ix a -> Ix a -> (Ix a -> b) -> UF a b
unionToLeft_ :: UF a b -> Ix a -> Ix a -> UF a b
unionToLeftAndSet_ :: UF a b -> Ix a -> Ix a -> b -> UF a b
unionToLeftAndSetWithRep_ :: UF a b -> Ix a -> Ix a -> (Ix a -> b) -> UF a b
unionToLeftWith_ :: (b -> b -> b) -> UF a b -> Ix a -> Ix a -> UF a b
unionToLeftWithRep_ :: (Ix a -> b -> b -> b) -> UF a b -> Ix a -> Ix a -> UF a b
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

-----------------------------------------------------------------------------

union :: UF a b -> Ix a -> Ix a -> (# Ix a, UF a b #)
{- INLINE union -}
union uf i j
  | (# ii,ri,si,uf #) <- findR uf i
  , (# jj,rj,_ ,uf #) <- findR uf j
  = case ii==jj of
      True-> (# ii,uf #)
      False
        | UF o <- uf
        -> case compare ri rj of
            LT
              | !o <- Ix.insert ii (IND jj) o -> (# jj,UF o #)
            EQ
              | !o <- Ix.insert jj (IND ii) o
              , !o <- Ix.insert ii (NODE (ri+1) si) o -> (# ii,UF o #)
            GT
              | !o <- Ix.insert jj (IND ii) o -> (# ii,UF o #)

unionWith :: (b -> b -> b) -> UF a b -> Ix a -> Ix a -> (# Ix a, UF a b #)
{- INLINE unionWith -}
unionWith f uf i j
  | (# ii,ri,si,uf #) <- findR uf i
  , (# jj,rj,sj,uf #) <- findR uf j
  = case ii==jj of
      True-> (# ii,uf #)
      False
        | UF o <- uf
        , new <- f si sj
        -> case compare ri rj of
            LT
              | !o <- Ix.insert ii (IND jj) o
              , !o <- Ix.insert jj (NODE (rj+0) new) o -> (# jj,UF o #)
            EQ
              | !o <- Ix.insert jj (IND ii) o
              , !o <- Ix.insert ii (NODE (ri+1) new) o -> (# ii,UF o #)
            GT
              | !o <- Ix.insert jj (IND ii) o
              , !o <- Ix.insert ii (NODE (ri+0) new) o -> (# ii,UF o #)

unionWithRep :: (Ix a -> b -> b -> b) -> UF a b -> Ix a -> Ix a -> (# Ix a, UF a b #)
{- INLINE unionWithRep -}
unionWithRep f uf i j
  | (# ii,ri,si,uf #) <- findR uf i
  , (# jj,rj,sj,uf #) <- findR uf j
  = case ii==jj of
      True-> (# ii,uf #)
      False
        | UF o <- uf
        -> case compare ri rj of
            LT
              | new <- f jj si sj
              , !o <- Ix.insert ii (IND jj) o
              , !o <- Ix.insert jj (NODE (rj+0) new) o -> (# jj,UF o #)
            EQ
              | new <- f ii si sj
              , !o <- Ix.insert jj (IND ii) o
              , !o <- Ix.insert ii (NODE (ri+1) new) o -> (# ii,UF o #)
            GT
              | new <- f ii si sj
              , !o <- Ix.insert jj (IND ii) o
              , !o <- Ix.insert ii (NODE (ri+0) new) o -> (# ii,UF o #)

unionAndSet :: UF a b -> Ix a -> Ix a -> b -> (# Ix a, UF a b #)
{- INLINE unionAndSet -}
unionAndSet uf i j new
  | (# ii,ri,_,uf #) <- findR uf i
  , (# jj,rj,_,uf #) <- findR uf j
  = case ii==jj of
      True-> (# ii,uf #)
      False
        | UF o <- uf
        -> case compare ri rj of
            LT
              | !o <- Ix.insert ii (IND jj) o
              , !o <- Ix.insert jj (NODE (rj+0) new) o -> (# jj,UF o #)
            EQ
              | !o <- Ix.insert jj (IND ii) o
              , !o <- Ix.insert ii (NODE (ri+1) new) o -> (# ii,UF o #)
            GT
              | !o <- Ix.insert jj (IND ii) o
              , !o <- Ix.insert ii (NODE (ri+0) new) o -> (# ii,UF o #)

unionAndSetWithRep :: UF a b -> Ix a -> Ix a -> (Ix a -> b) -> (# Ix a, UF a b #)
{- INLINE unionAndSetWithRep -}
unionAndSetWithRep uf i j f
  | (# ii,ri,_,uf #) <- findR uf i
  , (# jj,rj,_,uf #) <- findR uf j
  = case ii==jj of
      True-> (# ii,uf #)
      False
        | UF o <- uf
        -> case compare ri rj of
            LT
              | new <- f jj
              , !o <- Ix.insert ii (IND jj) o
              , !o <- Ix.insert jj (NODE (rj+0) new) o -> (# jj,UF o #)
            EQ
              | new <- f ii
              , !o <- Ix.insert jj (IND ii) o
              , !o <- Ix.insert ii (NODE (ri+1) new) o -> (# ii,UF o #)
            GT
              | new <- f ii
              , !o <- Ix.insert jj (IND ii) o
              , !o <- Ix.insert ii (NODE (ri+0) new) o -> (# ii,UF o #)

-- | Makes the *second* (right) @Ix a@
--  point to the *first* (left) @Ix a@, regardless
--  of rank. The ranks are also updated appropriately.
unionToLeft :: UF a b -> Ix a -> Ix a -> (# Ix a, UF a b #)
{- INLINE unionToLeft -}
unionToLeft uf i j
  | (# ii,ri,si,uf #) <- findR uf i
  , (# jj,rj,_, uf #) <- findR uf j
  = case ii==jj of
      True-> (# ii,uf #)
      False
        | UF o <- uf
        -> case compare ri rj of
            LT
              | !o <- Ix.insert jj (IND ii) o
              , !o <- Ix.insert ii (NODE (rj+1) si) o -> (# ii,UF o #)
            EQ
              | !o <- Ix.insert jj (IND ii) o
              , !o <- Ix.insert ii (NODE (ri+1) si) o -> (# ii,UF o #)
            GT
              | !o <- Ix.insert jj (IND ii) o -> (# ii,UF o #)

-- | Makes the *second* (right) @Ix a@
--  point to the *first* (left) @Ix a@, regardless
--  of rank. The ranks are also updated appropriately.
unionToLeftAndSet :: UF a b -> Ix a -> Ix a -> b -> (# Ix a, UF a b #)
{- INLINE unionToLeftAndSet -}
unionToLeftAndSet uf i j new
  | (# ii,ri,_,uf #) <- findR uf i
  , (# jj,rj,_,uf #) <- findR uf j
  = case ii==jj of
      True-> (# ii,uf #)
      False
        | UF o <- uf
        -> case compare ri rj of
            LT
              | !o <- Ix.insert jj (IND ii) o
              , !o <- Ix.insert ii (NODE (rj+1) new) o -> (# ii,UF o #)
            EQ
              | !o <- Ix.insert jj (IND ii) o
              , !o <- Ix.insert ii (NODE (ri+1) new) o -> (# ii,UF o #)
            GT
              | !o <- Ix.insert jj (IND ii) o
              , !o <- Ix.insert ii (NODE (ri+0) new) o -> (# ii,UF o #)

unionToLeftAndSetWithRep :: UF a b -> Ix a -> Ix a -> (Ix a -> b) -> (# Ix a, UF a b #)
{- INLINE unionToLeftAndSetWithRep -}
unionToLeftAndSetWithRep uf i j f
  | (# ii,ri,_,uf #) <- findR uf i
  , (# jj,rj,_,uf #) <- findR uf j
  = case ii==jj of
      True-> (# ii,uf #)
      False
        | UF o <- uf
        , new <- f ii
        -> case compare ri rj of
            LT
              | !o <- Ix.insert jj (IND ii) o
              , !o <- Ix.insert ii (NODE (rj+1) new) o -> (# ii,UF o #)
            EQ
              | !o <- Ix.insert jj (IND ii) o
              , !o <- Ix.insert ii (NODE (ri+1) new) o -> (# ii,UF o #)
            GT
              | !o <- Ix.insert jj (IND ii) o
              , !o <- Ix.insert ii (NODE (ri+0) new) o -> (# ii,UF o #)

unionToLeftWith :: (b -> b -> b) -> UF a b -> Ix a -> Ix a -> (# Ix a, UF a b #)
{- INLINE unionToLeftWith -}
unionToLeftWith f uf i j
  | (# ii,ri,si,uf #) <- findR uf i
  , (# jj,rj,sj,uf #) <- findR uf j
  = case ii==jj of
      True-> (# ii,uf #)
      False
        | UF o <- uf
        , new <- f si sj
        -> case compare ri rj of
            LT
              | !o <- Ix.insert jj (IND ii) o
              , !o <- Ix.insert ii (NODE (rj+1) new) o -> (# ii, UF o #)
            EQ
              | !o <- Ix.insert jj (IND ii) o
              , !o <- Ix.insert ii (NODE (ri+1) new) o -> (# ii, UF o #)
            GT
              | !o <- Ix.insert jj (IND ii) o
              , !o <- Ix.insert ii (NODE (ri+0) new) o -> (# ii, UF o #)

unionToLeftWithRep :: (Ix a -> b -> b -> b) -> UF a b -> Ix a -> Ix a -> (# Ix a, UF a b #)
{- INLINE unionToLeftWithRep -}
unionToLeftWithRep f uf i j
  | (# ii,ri,si,uf #) <- findR uf i
  , (# jj,rj,sj,uf #) <- findR uf j
  = case ii==jj of
      True-> (# ii,uf #)
      False
        | UF o <- uf
        , new <- f ii si sj
        -> case compare ri rj of
            LT
              | !o <- Ix.insert jj (IND ii) o
              , !o <- Ix.insert ii (NODE (rj+1) new) o -> (# ii, UF o #)
            EQ
              | !o <- Ix.insert jj (IND ii) o
              , !o <- Ix.insert ii (NODE (ri+1) new) o -> (# ii, UF o #)
            GT
              | !o <- Ix.insert jj (IND ii) o
              , !o <- Ix.insert ii (NODE (ri+0) new) o -> (# ii, UF o #)

{-
unionWithM :: (Monad m) => (b -> b -> m b) -> UF a b -> Ix a -> Ix a -> m (UF a b)
unionWithM f uf i j
  | (# ii,ri,si,uf #) <- findR uf i
  , (# jj,rj,sj,uf #) <- findR uf j
  = case ii==jj of
      True-> return uf
      False
        | UF o <- uf -> do
          new <- f si sj
          case compare ri rj of
            LT
              | !o <- Ix.insert ii (IND jj) o
              , !o <- Ix.insert jj (NODE (rj+0) new) o -> return (UF o)
            EQ
              | !o <- Ix.insert jj (IND ii) o
              , !o <- Ix.insert ii (NODE (ri+1) new) o -> return (UF o)
            GT
              | !o <- Ix.insert jj (IND ii) o
              , !o <- Ix.insert ii (NODE (ri+0) new) o -> return (UF o)

unionWithRepM :: (Monad m) => (Ix a -> b -> b -> m b) -> UF a b -> Ix a -> Ix a -> m (UF a b)
unionWithRepM f uf i j
  | (# ii,ri,si,uf #) <- findR uf i
  , (# jj,rj,sj,uf #) <- findR uf j
  = case ii==jj of
      True-> return uf
      False
        | UF o <- uf
        -> case compare ri rj of
            LT
              | !o <- Ix.insert ii (IND jj) o -> do
                new <- f jj si sj
                case () of
                  () | !o <- Ix.insert jj (NODE (rj+0) new) o
                    -> return (UF o)
            EQ
              | !o <- Ix.insert jj (IND ii) o-> do
                new <- f ii si sj
                case () of
                  () | !o <- Ix.insert ii (NODE (ri+1) new) o
                    -> return (UF o)
            GT
              | !o <- Ix.insert jj (IND ii) o-> do
                new <- f ii si sj
                case () of
                  () | !o <- Ix.insert ii (NODE (ri+0) new) o
                    -> return (UF o)

unionToLeftWithM :: (Monad m) => (b -> b -> m b) -> UF a b -> Ix a -> Ix a -> m (UF a b)
unionToLeftWithM f uf i j
  | (# ii,ri,si,uf #) <- findR uf i
  , (# jj,rj,sj,uf #) <- findR uf j
  = case ii==jj of
      True-> return uf
      False
        | UF o <- uf -> do
          new <- f si sj
          case compare ri rj of
            LT
              | !o <- Ix.insert jj (IND ii) o
              , !o <- Ix.insert ii (NODE (rj+1) new) o -> return (UF o)
            EQ
              | !o <- Ix.insert jj (IND ii) o
              , !o <- Ix.insert ii (NODE (ri+1) new) o -> return (UF o)
            GT
              | !o <- Ix.insert jj (IND ii) o
              , !o <- Ix.insert ii (NODE (ri+0) new) o -> return (UF o)

unionToLeftWithRepM :: (Monad m) => (Ix a -> b -> b -> m b) -> UF a b -> Ix a -> Ix a -> m (UF a b)
unionToLeftWithRepM f uf i j
  | (# ii,ri,si,uf #) <- findR uf i
  , (# jj,rj,sj,uf #) <- findR uf j
  = case ii==jj of
      True-> return uf
      False
        | UF o <- uf -> do
          new <- f ii si sj
          case compare ri rj of
            LT
              | !o <- Ix.insert jj (IND ii) o
              , !o <- Ix.insert ii (NODE (rj+1) new) o -> return (UF o)
            EQ
              | !o <- Ix.insert jj (IND ii) o
              , !o <- Ix.insert ii (NODE (ri+1) new) o -> return (UF o)
            GT
              | !o <- Ix.insert jj (IND ii) o
              , !o <- Ix.insert ii (NODE (ri+0) new) o -> return (UF o)
-}

-----------------------------------------------------------------------------

unionWith' :: (UF a b -> Ix a -> b -> b -> (b, UF a b)) -> UF a b -> Ix a -> Ix a -> UF a b
unionWith' f uf i j
  | (# ii,ri,si,uf #) <- findR uf i
  , (# jj,rj,sj,uf #) <- findR uf j
  = case ii==jj of
      True-> uf
      False
        | UF o <- uf
        -> case compare ri rj of
            LT->
              let !o2 = Ix.insert ii (IND jj) o
                  o3 = Ix.insert jj (NODE (rj+0) s2) o2
                  (s2,out) = f (UF o3) jj si sj
                  UF !o4 = out in UF o4
            EQ->
              let !o2 = Ix.insert jj (IND ii) o
                  o3 = Ix.insert ii (NODE (ri+1) s2) o2
                  (s2,out) = f (UF o3) ii si sj
                  UF !o4 = out in UF o4
            GT->
              let !o2 = Ix.insert jj (IND ii) o
                  o3 = Ix.insert ii (NODE (ri+0) s2) o2
                  (s2,out) = f (UF o3) ii si sj
                  UF !o4 = out in UF o4

-----------------------------------------------------------------------------

compress :: UF a b -> UF a b
compress uf@(UF o) = go mempty uf (Ix.keys o)
  where go seen uf [] = uf
        go seen uf (i:is)
          | i`IxS.member`seen
          = go seen uf is
          | (# j,_,!uf #) <- find uf i
          = case i==j of
              True
                -> go seen uf is
              False
                | !seen <- j`IxS.insert`seen
                -> go seen uf is

compressAndIndex :: UF a b -> (IxMap a (Ix a), UF a b)
compressAndIndex uf@(UF o) = go mempty uf mempty (Ix.keys o)
  where go seen uf ix [] = (ix,uf)
        go seen uf ix (i:is)
          | i`IxS.member`seen
          = go seen uf ix is
          | (# j,_,!uf #) <- find uf i
          = case i==j of
              True
                | !ix   <- Ix.insert i i ix
                -> go seen uf ix is
              False
                | !seen <- j`IxS.insert`seen
                , !ix   <- Ix.insert i j ix
                , !ix   <- Ix.insert j j ix
                -> go seen uf ix is

support :: UF a b -> (IxMap a (Ix a), UF a b)
support uf@(UF o) = go mempty uf mempty (Ix.keys o)
  where go seen uf ix [] = (ix,uf)
        go seen uf ix (i:is)
          | i`IxS.member`seen
          = go seen uf ix is
          | (# j,_,!uf #) <- find uf i
          = case i==j of
              True
                -> go seen uf ix is
              False
                | !seen <- j`IxS.insert`seen
                , !ix   <- Ix.insert i j ix
                -> go seen uf ix is

supportAndSize :: UF a b -> (Int, IxMap a (Ix a), UF a b)
supportAndSize uf@(UF o) = go 0 mempty uf mempty (Ix.keys o)
  where go n seen uf ix [] = (n,ix,uf)
        go n seen uf ix (i:is)
          | i`IxS.member`seen
          = go n seen uf ix is
          | (# j,_,!uf #) <- find uf i
          = case i==j of
              True
                -> go n seen uf ix is
              False
                | !seen <- j`IxS.insert`seen
                , !ix   <- Ix.insert i j ix
                , !n    <- n + 1
                -> go n seen uf ix is

-----------------------------------------------------------------------------

-- | \"Garbage\" collection. Only representatives are retained in the
--  returned @UF a b@. The @IxMap a (Ix a)@ gives a mapping of old @Ix a@s
--  in the incoming @UF a b@ to new @Ix a@s in the returned @UF a b@.
collect :: UF a b -> (IxMap a (Ix a), UF a b)
collect uf@(UF o) = go mempty mempty uf mempty (Ix.keys o)
  where go new seen uf ix [] = (ix,UF new)
        go new seen uf ix (i:is)
          | i`IxS.member`seen
          = go new seen uf ix is
          | (# j,b,!uf #) <- find uf i
          = case i==j of
              True
                | !node <- newU b
                , !new  <- Ix.insert i node new
                , !ix   <- Ix.insert i i ix
                , !seen <- j`IxS.insert`seen
                -> go new seen uf ix is
              False
                | j`IxS.member`seen
                , !ix <- Ix.insert i j ix
                -> go new seen uf ix is
                | !node <- newU b
                , !new  <- Ix.insert j node new
                , !ix   <- Ix.insert i j ix
                , !ix   <- Ix.insert j j ix
                , !seen <- j`IxS.insert`seen
                -> go new seen uf ix is

collect_ :: UF a b -> (IxMap a (Ix a), UF a b)
collect_ uf@(UF o) = go mempty mempty uf mempty (Ix.keys o)
  where go new seen uf ix [] = (ix,UF new)
        go new seen uf ix (i:is)
          | i`IxS.member`seen
          = go new seen uf ix is
          | (# j,b,!uf #) <- find uf i
          = case i==j of
              True
                | !node <- newU b
                , !new  <- Ix.insert i node new
                , !seen <- j`IxS.insert`seen
                -> go new seen uf ix is
              False
                | j`IxS.member`seen
                , !ix <- Ix.insert i j ix
                -> go new seen uf ix is
                | !node <- newU b
                , !new  <- Ix.insert j node new
                , !ix   <- Ix.insert i j ix
                , !seen <- j`IxS.insert`seen
                -> go new seen uf ix is

collectAndReindexFrom :: Int -> UF a b -> (Int, IxMap a (Ix a), UF a b)
collectAndReindexFrom !n uf@(UF o) = go n mempty mempty uf mempty (Ix.keys o)
  where go n new seen uf ix [] = (n,ix,UF new)
        go n new seen uf ix (i:is)
          | i`IxS.member`seen
          = go n new seen uf ix is
          | (# j,b,!uf #) <- find uf i
          = case i==j of
              True
                | jnew  <- Ix n
                , !n    <- n+1
                , !node <- newU b
                , !new  <- Ix.insert jnew node new
                , !ix   <- Ix.insert j jnew ix
                , !seen <- j`IxS.insert`seen
                -> go n new seen uf ix is
              False
                | j`IxS.member`seen
                , !jnew <- ix Ix.! j
                , !ix   <- Ix.insert i jnew ix
                -> go n new seen uf ix is
                | jnew  <- Ix n
                , !n    <- n+1
                , !node <- newU b
                , !new  <- Ix.insert jnew node new
                , !ix   <- Ix.insert i jnew ix
                , !ix   <- Ix.insert j jnew ix
                , !seen <- j`IxS.insert`seen
                -> go n new seen uf ix is

-----------------------------------------------------------------------------

quotient' :: UF a b -> (IxMap a (Ix b), IxMap b b)
quotient' o
  | (f,g) <- quotient o
  , f     <- castIxArg f
  , g     <- Ix.castIxMap g
  = (f,g)

quotientAndReindexFrom' :: Int -> UF a b -> (Int, IxMap a (Ix b), IxMap b b)
quotientAndReindexFrom' n o
  | (n,f,g) <- quotientAndReindexFrom n o
  , f       <- castIxArg f
  , g       <- Ix.castIxMap g
  = (n,f,g)


-- | Identical to @collect@, except an
--  @IxMap a b@ is returned instead of a @UF a b@.
quotient :: UF a b -> (IxMap a (Ix a), IxMap a b)
quotient uf@(UF o) = go mempty mempty uf mempty (Ix.keys o)
  where go new seen uf ix [] = (ix,new)
        go new seen uf ix (i:is)
          | i`IxS.member`seen
          = go new seen uf ix is
          | (# j,b,!uf #) <- find uf i
          = case i==j of
              True
                | !new  <- Ix.insert i b new
                , !ix   <- Ix.insert i i ix
                , !seen <- j`IxS.insert`seen
                -> go new seen uf ix is
              False
                | j`IxS.member`seen
                , !ix <- Ix.insert i (castIx j) ix
                -> go new seen uf ix is
                | !new  <- Ix.insert (castIx j) b new
                , !ix   <- Ix.insert i j ix
                , !ix   <- Ix.insert j j ix
                , !seen <- j`IxS.insert`seen
                -> go new seen uf ix is

-- | Identical to @collect_@, except an
--  @IxMap a b@ is returned instead of a @UF a b@.
quotient_ :: UF a b -> (IxMap a (Ix a), IxMap a b)
quotient_ uf@(UF o) = go mempty mempty uf mempty (Ix.keys o)
  where go new seen uf ix [] = (ix,new)
        go new seen uf ix (i:is)
          | i`IxS.member`seen
          = go new seen uf ix is
          | (# j,b,!uf #) <- find uf i
          = case i==j of
              True
                | !new  <- Ix.insert i b new
                , !seen <- j`IxS.insert`seen
                -> go new seen uf ix is
              False
                | j`IxS.member`seen
                , !ix <- Ix.insert i (castIx j) ix
                -> go new seen uf ix is
                | !new  <- Ix.insert (castIx j) b new
                , !ix   <- Ix.insert i j ix
                , !seen <- j`IxS.insert`seen
                -> go new seen uf ix is


-- | Identical to @collectAndReindexFrom@,
--  except an @IxMap a b@ is returned instead of a @UF a b@.
quotientAndReindexFrom :: Int -> UF a b -> (Int, IxMap a (Ix a), IxMap a b)
quotientAndReindexFrom !n uf@(UF o) = go n mempty mempty uf mempty (Ix.keys o)
  where go n new seen uf ix [] = (n,ix,new)
        go n new seen uf ix (i:is)
          | i`IxS.member`seen
          = go n new seen uf ix is
          | (# j,b,!uf #) <- find uf i
          = case i==j of
              True
                | jnew  <- Ix n
                , !n    <- n+1
                , !new  <- Ix.insert jnew b new
                , !ix   <- Ix.insert j jnew ix
                , !seen <- j`IxS.insert`seen
                -> go n new seen uf ix is
              False
                | j`IxS.member`seen
                , !jnew <- ix Ix.! j
                , !ix   <- Ix.insert i jnew ix
                -> go n new seen uf ix is
                | jnew  <- Ix n
                , !n    <- n+1
                , !new  <- Ix.insert jnew b new
                , !ix   <- Ix.insert i jnew ix
                , !ix   <- Ix.insert j jnew ix
                , !seen <- j`IxS.insert`seen
                -> go n new seen uf ix is

-----------------------------------------------------------------------------

instance (Join b) => Join (UF a b) where
  (\/) a b = joinWith (\/) a b

joinWith :: (b -> b -> b) -> UF a b -> UF a b -> UF a b
joinWith op (UF f) uf
  | (rn,UF g) <- collect_ uf
  , !u <- g`Ix.difference`f
  , !v <- g`Ix.intersection`f
  , !new <- UF (f`Ix.union`u)
  , !new <- Ix.foldWithKey one new v
  , !new <- Ix.foldWithKey two new rn = new
  where
        one i (NODE _ b2) new
          | (# i,r,b1,UF o #) <- findR new i
          , !b <- op b1 b2
          , !node <- NODE r b
          , !o <- Ix.insert i node o
          = UF o

        two i j new
          | (# Just i,new #) <- tryRep new i
          = union_ new i j
          | (# j,r,b,UF o #) <- findR new j
          , !o <- case r < 1 of
                    True-> Ix.insert j (NODE 1 b) o
                    False-> o
          , !o <- Ix.insert i (IND j) o
          = UF o

-----------------------------------------------------------------------------

getInds :: UF a b -> Ix a -> IxSet a
getInds uf _ = mempty

setInds :: UF a b -> Ix a -> IxSet a -> UF a b
setInds uf _ _ = uf


getAndClearInds :: UF a b -> Ix a -> (# IxSet a, UF a b #)
getAndClearInds uf i
  | inds <- getInds uf i
  , !uf <- clearInds uf i
  = (# inds,uf #)

mapInds :: (IxSet a -> IxSet a) -> UF a b -> Ix a -> UF a b
mapInds f uf i
  | inds <- getInds uf i
  , !inds <- f inds
  = setInds uf i inds

popDirty :: UF a b -> (# Maybe (Ix a), UF a b #)
popDirty uf = (# Nothing,uf #)

clearInds :: UF a b -> Ix a -> UF a b
clearInds uf i = setInds uf i mempty

-----------------------------------------------------------------------------

statsMaxIndChain :: UF a b -> Int
statsMaxIndChain (UF o) = fold 0 mempty (Ix.keys o)
  where fold mx seen [] = mx
        fold mx seen (i:is)
          | (# seen,mx #) <- go mx 0 seen i
          = fold mx seen is
        go mx n seen i
          | IND j <- o Ix.! i
          , !n <- n+1
          , !seen <- i`IxS.insert`seen
          = go mx n seen j
          | !mx <- max mx n
          , !seen <- i`IxS.insert`seen
          = (# seen,mx #)

-----------------------------------------------------------------------------

ppUF :: (Show b) => UF a b -> [(String,[String])]
ppUF (UF m) =
  Ix.foldWithKey (\i node acc->
    case node of
      NODE{}-> (show node,[]):acc
      IND j-> (show node,[show(m Ix.! j)]):acc) [] m

ppUF' :: (Show b) => UF a b -> [(String,[String])]
ppUF' (UF m) =
  Ix.foldWithKey (\i node acc->
    case node of
      NODE{}-> (show node,[]):acc
      IND j-> (ppU i node,[ppU j (m Ix.! j)]):acc) [] m

ppU :: (Show b) => Ix a -> U a b -> String
ppU (Ix i) o@(IND{}) = show i ++ ":IND"
ppU (Ix i) o@(NODE{}) = show i ++ ":" ++ show o

-----------------------------------------------------------------------------

instance UF.UF (UF a b) a b where
  find = find
  rep = rep
  get = get
  tryRep = tryRep
  ind = ind
  keys = keys
  roots = roots
  -- elems = elems
  -- dumpUF = toList
  -- toUF = fromList
  -- fromUF = toList
  -- fromIxMap = fromIxMap
  -- singleton = singleton
  lookup = lookup
  unsafeInsert = unsafeInsert
  insert = insert
  insertWith = insertWith
  insertWithRep = insertWithRep
  insertWithM = insertWithM
  insertWithRepM = insertWithRepM
  update = update
  updateWithRep = updateWithRep
  updateM = updateM
  updateWithRepM = updateWithRepM
  fold = fold
  -- foldWith = foldWith
  foldM = foldM
  -- foldWithM = foldWithM
  union = union
  unionWith = unionWith
  unionWithRep = unionWithRep
  unionAndSet = unionAndSet
  unionAndSetWithRep = unionAndSetWithRep
  unionToLeft = unionToLeft
  unionToLeftAndSet = unionToLeftAndSet
  unionToLeftAndSetWithRep = unionToLeftAndSetWithRep
  unionToLeftWith = unionToLeftWith
  unionToLeftWithRep = unionToLeftWithRep
  union_ = union_
  unionWith_ = unionWith_
  unionWithRep_ = unionWithRep_
  unionAndSet_ = unionAndSet_
  unionAndSetWithRep_ = unionAndSetWithRep_
  unionToLeft_ = unionToLeft_
  unionToLeftAndSet_ = unionToLeftAndSet_
  unionToLeftAndSetWithRep_ = unionToLeftAndSetWithRep_
  unionToLeftWith_ = unionToLeftWith_
  unionToLeftWithRep_ = unionToLeftWithRep_
  -- compress = compress
  support = support
  collect = collect
  collect_ = collect_
  quotient = quotient
  quotient_ = quotient_
  compressAndIndex = compressAndIndex
  supportAndSize = supportAndSize
  collectAndReindexFrom = collectAndReindexFrom
  quotientAndReindexFrom = quotientAndReindexFrom

#if 0
instance UFM (S.S (UF a a)) a where
  newM = do
    i <- S.newUniq
    return (Ix i)
  addNewM a = do
    i <- newM
    S.modify (\uf-> unsafeInsert uf i a)
    return i
  setM i a = S.modify
    (\uf-> insert uf i a)
  findM i = do
    uf <- S.get
    case find uf i of
      (# i,a,uf #)-> do
        S.set uf
        return (i,a)
  getM i = do
    uf <- S.get
    case get uf i of
      (# a,uf #)-> do
        S.set uf
        return a
  repM i = do
    uf <- S.get
    case rep uf i of
      (# i,uf #)-> do
        S.set uf
        return i
  unionM i j = do
    uf <- S.get
    case union uf i j of
      (# i,uf #)-> do
        S.set uf
        return i
  unionToLeftM i j = do
    uf <- S.get
    case unionToLeft uf i j of
      (# i,uf #)-> do
        S.set uf
        return i
  unionAndSetM i j b = do
    uf <- S.get
    case unionAndSet uf i j b of
      (# i,uf #)-> do
        S.set uf
        return i
  unionToLeftAndSetM i j b = do
    uf <- S.get
    case unionToLeftAndSet uf i j b of
      (# i,uf #)-> do
        S.set uf
        return i
  unionWithM f i j = do
    uf <- S.get
    case unionWith f uf i j of
      (# i,uf #)-> do
        S.set uf
        return i
  unionWithRepM f i j = do
    uf <- S.get
    case unionWithRep f uf i j of
      (# i,uf #)-> do
        S.set uf
        return i
  unionToLeftWithM f i j = do
    uf <- S.get
    case unionToLeftWith f uf i j of
      (# i,uf #)-> do
        S.set uf
        return i
  unionToLeftWithRepM f i j = do
    uf <- S.get
    case unionToLeftWithRep f uf i j of
      (# i,uf #)-> do
        S.set uf
        return i
{-
  supportM = do
    uf <- S.get
    case support uf of
      (ix,uf)-> do
        S.set uf
        return ix
  collectM = do
    uf <- S.get
    case collect uf of
      (ix,uf)-> do
        S.set uf
        return ix
  quotientM = do
    uf <- S.get
    let !out = quotient uf
    return out
  compressAndIndexM = do
    uf <- S.get
    case compressAndIndex uf of
      (ix,uf)-> do
        S.set uf
        return ix
  supportAndSizeM = do
    uf <- S.get
    case supportAndSize uf of
      (n,ix,uf)-> do
        S.set uf
        return (n,ix)
  collectAndReindexFromM n = do
    uf <- S.get
    case collectAndReindexFrom n uf of
      (n,ix,uf)-> do
        S.set uf
        return (n,ix)
  quotientAndReindexFromM n = do
    uf <- S.get
    let !out = quotientAndReindexFrom n uf
    return out
-}
#endif

-----------------------------------------------------------------------------
