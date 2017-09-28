{-# OPTIONS -pgmP cpp -optP-w #-}
{-# LANGUAGE
      CPP,
      MagicHash,
      UnboxedTuples,
      BangPatterns #-}
#define __D deriving(Eq,Ord,Read,Show)
#define __D_BE deriving(Eq,Ord,Read,Show,Bounded,Enum)

module MM.Data.UnionFind.Int (
   UF,U,newU
  ,joinWith
  ,ppUF,ppUFAlt,ppU,statsMaxIndChain
  ,getInternalRep,castUF,maxKey,fromKeyMap,fromList,toList,find
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
  ,quotient_,quotientAlt,quotientAndReindexFromAlt
  ,getInds,setInds,mapInds,getAndClearInds,popDirty,clearInds
) where

import Prelude hiding(lookup)
import qualified Prelude as P
import Data.Int
import Data.Word
import Data.Bits
import MM.Data.Map.Ord(Map)
import MM.Data.Set.Ord(Set)
import MM.Data.Map.Int(IntMap)
import MM.Data.Set.Int(IntSet)
import qualified MM.Data.Map.Ord as M
import qualified MM.Data.Set.Ord as S
import qualified MM.Data.Map.Int as IM
import qualified MM.Data.Set.Int as IS
import Data.Monoid(Monoid(..))
import Control.Applicative(Applicative(..))
import Data.Function
import Data.List(foldl')
import Control.Monad hiding(foldM)
import Unsafe.Coerce
import qualified Data.Binary as Bin
import qualified MM.Data.Class.UnionFind as UF
import MM.Data.Class.Empty(Empty(..))
import MM.Data.Class.Lattice(Join(..))

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

#define __KEY(a)      Int
#define __UF(a)       UF
#define __U(a)        U
#define __KEYMAP(a,b) IntMap b
#if 0
-- XXX:should be doing this:
#define __KEYMAP(a) IntMap
#endif
#define __KEYSET(a)   IntSet
#define __KMAPPRE(x)  IM.x
#define __KSETPRE(x)  IS.x
#define cast__KEY(i)  i
#define __KEYVAL(i)   i
#define __KEYPAT(i)   i

maxKeyWithDefault :: __KEY(a) -> __KEYMAP(a,b) -> __KEY(a)
maxKeyWithDefault dflt m
  | Just ((k,_),_) <- IM.maxViewWithKey m = k
  | otherwise = dflt

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

instance (Bin.Binary b) => Bin.Binary (__UF(a) b) where
  put (UF o) = Bin.put o
  get = do o <- Bin.get; return (UF o)
instance (Bin.Binary t2) => Bin.Binary (__U(t1) t2) where
  put (NODE x1 x2) = Bin.putWord8 0 >> (Bin.put x1 >> Bin.put x2)
  put (IND x1) = Bin.putWord8 1 >> Bin.put x1
  get
    = Bin.getWord8 >>=
        (\ tag_ ->
           case tag_ of
             0 -> ap (ap (return NODE) Bin.get) Bin.get
             1 -> ap (return IND) Bin.get)

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

maxKey :: __UF(a) b -> __KEY(a)
maxKey (UF ix) = maxKeyWithDefault 0 ix

-----------------------------------------------------------------------------

newtype __UF(a) b = UF
  {unUF :: __KEYMAP(a,(__U(a) b))}
  deriving(Eq,Ord,Read,Show)

type Rank = Int
data __U(a) b
  = NODE !Rank b    -- union-by-rank
  | IND !(__KEY(a))     -- path-compress
  deriving(Eq,Ord,Read,Show)

ind :: __UF(a) b -> __KEY(a) -> __KEY(a) -> __UF(a) b
ind uf@(UF o) from to
  | Just{} <- __KMAPPRE(lookup) from o
  = unionToLeft_ uf to from
  | (# to,r,b,uf #) <- findR uf to
  , UF o <- uf
  , !o <- case r < 1 of
            True-> __KMAPPRE(insert) to (NODE 1 b) o
            False-> o
  , !o <- __KMAPPRE(insert) from (IND to) o
  = UF o

getInternalRep :: __UF(a) b -> __KEYMAP(a,(__U(a) b))
getInternalRep (UF o) = o

castUF :: __UF(a) b -> __UF(c) b
castUF = unsafeCoerce

newU :: b -> __U(a) b
{-# INLINE newU #-}
newU = NODE 0

uToE :: __U(a) b -> Either (__KEY(a)) b
{-# INLINE uToE #-}
uToE (IND i) = Left i
uToE (NODE _ b) = Right b

instance Functor (__UF(a)) where
  fmap f (UF o) = UF (fmap (fmap f) o)

instance Functor (__U(a)) where
  fmap f (NODE r b) = NODE r (f b)
  fmap f (IND i) = IND i

instance Empty (__UF(a) b) where
  empty = UF empty
  isEmpty (UF o) = isEmpty o

instance Monoid (__UF(a) b) where
  mempty = empty
  mappend a b = joinWith const a b

-----------------------------------------------------------------------------

fromKeyMap :: __KEYMAP(a,b) -> __UF(a) b
fromKeyMap m = UF (fmap newU m)

fromList :: [(__KEY(a), b)] -> __UF(a) b
fromList xs = UF (foldl' go mempty xs)
  where go o (i,b) = __KMAPPRE(insert) i (newU b) o

toList :: __UF(a) b -> [(__KEY(a), Either (__KEY(a)) b)]
toList (UF o) = __KMAPPRE(toList) (fmap uToE o)

-----------------------------------------------------------------------------

find :: __UF(a) b -> __KEY(a) -> (# __KEY(a), b, __UF(a) b #)
rep  :: __UF(a) b -> __KEY(a) -> (# __KEY(a),    __UF(a) b #)
get  :: __UF(a) b -> __KEY(a) -> (#       b, __UF(a) b #)
{- INLINE find -}
{- INLINE rep -}
{- INLINE get -}
find o i | (# i,_,b,uf #) <- findR o i = (# i,b,uf #)
rep  o i | (# i,_,_,uf #) <- findR o i = (# i,  uf #)
get  o i | (# _,_,b,uf #) <- findR o i = (#   b,uf #)

-----------------------------------------------------------------------------

keys :: __UF(a) b -> [__KEY(a)]
keys (UF o) = __KMAPPRE(keys) o

roots :: __UF(a) b -> [__KEY(a)]
roots (UF o) = [i | (i,NODE{}) <- __KMAPPRE(toList) o]

elems :: __UF(a) b -> [b]
elems (UF o) = [b | NODE _ b <- __KMAPPRE(elems) o]

singleton :: __KEY(a) -> b -> __UF(a) b
singleton i b = UF (__KMAPPRE(singleton) i (newU b))

-----------------------------------------------------------------------------

lookup :: __UF(a) b -> __KEY(a) -> Maybe (b, __UF(a) b)
lookup uf@(UF o) i =
  case __KMAPPRE(lookup) i o of
    Nothing-> Nothing
    Just (NODE _ b)-> Just (b,uf)
    Just (IND{})
      | (# b,uf #) <- get uf i
      -> Just (b,uf)

unsafeInsert :: __UF(a) b -> __KEY(a) -> b -> __UF(a) b
unsafeInsert (UF o) i new
  | !node <- newU new
  = UF (__KMAPPRE(insert) i node o)

insert :: __UF(a) b -> __KEY(a) -> b -> __UF(a) b
insert uf@(UF o) i new =
  case __KMAPPRE(lookup) i o of
    Nothing
      | n <- newU new -> UF (__KMAPPRE(insert) i n o)
    Just (NODE r _)-> UF (__KMAPPRE(insert) i (NODE r new) o)
    Just (IND{})
      | (# i,r,_,uf #) <- findR uf i
      , UF o <- uf
      -> UF (__KMAPPRE(insert) i (NODE r new) o)

insertWith :: (b -> b -> b) -> __UF(a) b -> __KEY(a) -> b -> __UF(a) b
insertWith f uf@(UF o) i new =
  case __KMAPPRE(lookup) i o of
    Nothing
      | n <- newU new -> UF (__KMAPPRE(insert) i n o)
    Just (NODE r old)
      | b <- f new old
      -> UF (__KMAPPRE(insert) i (NODE r b) o)
    Just (IND{})
      | (# i,r,old,uf #) <- findR uf i
      , UF o <- uf
      , b <- f new old
      -> UF (__KMAPPRE(insert) i (NODE r b) o)

insertWithRep :: (__KEY(a) -> b -> b -> b) -> __UF(a) b -> __KEY(a) -> b -> __UF(a) b
insertWithRep f uf@(UF o) i new =
  case __KMAPPRE(lookup) i o of
    Nothing
      | n <- newU new -> UF (__KMAPPRE(insert) i n o)
    Just (NODE r old)
      | b <- f i new old
      -> UF (__KMAPPRE(insert) i (NODE r b) o)
    Just (IND{})
      | (# i,r,old,uf #) <- findR uf i
      , UF o <- uf
      , b <- f i new old
      -> UF (__KMAPPRE(insert) i (NODE r b) o)

insertWithM :: (Monad m) => (b -> b -> m b) -> __UF(a) b -> __KEY(a) -> b -> m (__UF(a) b)
insertWithM f uf@(UF o) i new =
  case __KMAPPRE(lookup) i o of
    Nothing
      | n <- newU new
      , !uf <- UF (__KMAPPRE(insert) i n o)
      -> return uf
    Just (NODE r old)-> do
      b <- f new old
      let !uf2 = UF (__KMAPPRE(insert) i (NODE r b) o)
      return uf2
    Just (IND{})
      | (# i,r,old,uf #) <- findR uf i
      , UF o <- uf
      -> do b <- f new old
            let !uf2 = UF (__KMAPPRE(insert) i (NODE r b) o)
            return uf2

insertWithRepM :: (Monad m) => (__KEY(a) -> b -> b -> m b) -> __UF(a) b -> __KEY(a) -> b -> m (__UF(a) b)
insertWithRepM f uf@(UF o) i new =
  case __KMAPPRE(lookup) i o of
    Nothing
      | n <- newU new
      , !uf <- UF (__KMAPPRE(insert) i n o)
      -> return uf
    Just (NODE r old)-> do
      b <- f i new old
      let !uf2 = UF (__KMAPPRE(insert) i (NODE r b) o)
      return uf2
    Just (IND{})
      | (# i,r,old,uf #) <- findR uf i
      , UF o <- uf
      -> do b <- f i new old
            let !uf2 = UF (__KMAPPRE(insert) i (NODE r b) o)
            return uf2

update :: (b -> b) -> __UF(a) b -> __KEY(a) -> __UF(a) b
update f uf i
  | (# i,r,b,uf #) <- findR uf i
  , UF o <- uf
  , b <- f b
  = UF (__KMAPPRE(insert) i (NODE r b) o)

updateWithRep :: (__KEY(a) -> b -> b) -> __UF(a) b -> __KEY(a) -> __UF(a) b
updateWithRep f uf i
  | (# i,r,b,uf #) <- findR uf i
  , UF o <- uf
  , b <- f i b
  = UF (__KMAPPRE(insert) i (NODE r b) o)

updateM :: (Monad m) => (b -> m b) -> __UF(a) b -> __KEY(a) -> m (__UF(a) b)
updateM f uf i
  | (# i,r,b,uf #) <- findR uf i
  , UF o <- uf = do
      b <- f b
      let !uf2 = UF (__KMAPPRE(insert) i (NODE r b) o)
      return uf2

updateWithRepM :: (Monad m) => (__KEY(a) -> b -> m b) -> __UF(a) b -> __KEY(a) -> m (__UF(a) b)
updateWithRepM f uf i
  | (# i,r,b,uf #) <- findR uf i
  , UF o <- uf = do
      b <- f i b
      let !uf2 = UF (__KMAPPRE(insert) i (NODE r b) o)
      return uf2

-----------------------------------------------------------------------------

fold :: (__KEY(a) -> b -> c -> c) -> c -> __UF(a) b -> (c, __UF(a) b)
fold f c uf@(UF o) = go mempty uf c (__KMAPPRE(keys) o)
  where go seen uf c [] = (c, uf)
        go seen uf c (i:is)
          | i`__KSETPRE(member)`seen
          = go seen uf c is
          | (# j,b,!uf #) <- find uf i
          = case i==j of
              True
                | !c  <- f i b c
                -> go seen uf c is
              False
                | !seen <- j`__KSETPRE(insert)`seen
                , !c  <- f j b c
                -> go seen uf c is

foldWith :: (__UF(a) b -> __KEY(a) -> b -> c -> (c, __UF(a) b)) -> c -> __UF(a) b -> (c, __UF(a) b)
foldWith f c uf@(UF o) = go mempty uf c (__KMAPPRE(keys) o)
  where go seen uf c [] = (c,uf)
        go seen uf c (i:is)
          | i`__KSETPRE(member)`seen
          = go seen uf c is
          | (# j,b,!uf #) <- find uf i
          = case i==j of
              True
                | (!c,!uf) <- f uf i b c
                -> go seen uf c is
              False
                | !seen <- j`__KSETPRE(insert)`seen
                , (!c,!uf) <- f uf j b c
                -> go seen uf c is

foldWith_ :: (__UF(a) b -> __KEY(a) -> b -> __UF(a) b) -> __UF(a) b -> __UF(a) b
foldWith_ f uf@(UF o) = go mempty uf (__KMAPPRE(keys) o)
  where go seen uf [] = uf
        go seen uf (i:is)
          | i`__KSETPRE(member)`seen
          = go seen uf is
          | (# j,b,!uf #) <- find uf i
          = case i==j of
              True
                | !uf <- f uf i b
                -> go seen uf is
              False
                | !seen <- j`__KSETPRE(insert)`seen
                , !uf <- f uf j b
                -> go seen uf is

foldM :: (Monad m) => (__KEY(a) -> b -> c -> m c) -> c -> __UF(a) b -> m (c, __UF(a) b)
{- INLINE foldM -}
foldM f c uf@(UF o) = go mempty uf c (__KMAPPRE(keys) o)
  where go seen uf c [] = return (c, uf)
        go seen uf c (i:is)
          | i`__KSETPRE(member)`seen
          = go seen uf c is
          | (# j,b,!uf #) <- find uf i
          = case i==j of
              True-> do
                !c  <- f i b c
                go seen uf c is
              False
                | !seen <- j`__KSETPRE(insert)`seen-> do
                  !c  <- f j b c
                  go seen uf c is

foldWithM :: (Monad m) => (__UF(a) b -> __KEY(a) -> b -> c -> m (c, __UF(a) b)) -> c -> __UF(a) b -> m (c, __UF(a) b)
{- INLINE foldWithM -}
foldWithM f c uf@(UF o) = go mempty uf c (__KMAPPRE(keys) o)
  where go seen uf acc [] = return (c,uf)
        go seen uf acc (i:is)
          | i`__KSETPRE(member)`seen
          = go seen uf c is
          | (# j,b,!uf #) <- find uf i
          = case i==j of
              True-> do
                  !(!c,!uf) <- f uf i b c
                  go seen uf c is
              False
                | !seen <- j`__KSETPRE(insert)`seen-> do
                  !(!c,!uf) <- f uf j b c
                  go seen uf c is

-----------------------------------------------------------------------------

findR :: __UF(a) b -> __KEY(a) -> (# __KEY(a), Rank, b, __UF(a) b #)
{- INLINE findR -}
findR uf@(UF o) i =
  case o __KMAPPRE(!) i of
    IND j->
      case o __KMAPPRE(!) j of
        IND k-> go [j,i] k
        NODE r s-> (# j,r,s,uf #)
    NODE r s -> (# i,r,s,uf #)
  where go acc i =
          case o __KMAPPRE(!) i of
            IND j-> go (i:acc) j
            NODE r s
              | ind <- IND i
              , !o  <- foldl'
                  (\o j-> __KMAPPRE(insert) j ind o)
                   (o) (acc)
              -> (# i,r,s,UF o #)

tryRep :: __UF(a) b -> __KEY(a) -> (# Maybe (__KEY(a)), __UF(a) b #)
tryRep uf@(UF o) i =
  case __KMAPPRE(lookup) i o of
    Nothing-> (# Nothing, uf #)
    Just(IND j)->
      case o __KMAPPRE(!) j of
        IND k-> go [j,i] k
        NODE{}-> (# Just j,uf #)
    Just(NODE{})-> (# Just i,uf #)
  where go acc i =
          case o __KMAPPRE(!) i of
            IND j-> go (i:acc) j
            NODE r s
              | ind <- IND i
              , !o  <- foldl'
                  (\o j-> __KMAPPRE(insert) j ind o)
                   (o) (acc)
              -> (# Just i,UF o #)

-----------------------------------------------------------------------------

union_ :: __UF(a) b -> __KEY(a) -> __KEY(a) -> __UF(a) b
unionWith_ :: (b -> b -> b) -> __UF(a) b -> __KEY(a) -> __KEY(a) -> __UF(a) b
unionWithRep_ :: (__KEY(a) -> b -> b -> b) -> __UF(a) b -> __KEY(a) -> __KEY(a) -> __UF(a) b
unionAndSet_ :: __UF(a) b -> __KEY(a) -> __KEY(a) -> b -> __UF(a) b
unionAndSetWithRep_ :: __UF(a) b -> __KEY(a) -> __KEY(a) -> (__KEY(a) -> b) -> __UF(a) b
unionToLeft_ :: __UF(a) b -> __KEY(a) -> __KEY(a) -> __UF(a) b
unionToLeftAndSet_ :: __UF(a) b -> __KEY(a) -> __KEY(a) -> b -> __UF(a) b
unionToLeftAndSetWithRep_ :: __UF(a) b -> __KEY(a) -> __KEY(a) -> (__KEY(a) -> b) -> __UF(a) b
unionToLeftWith_ :: (b -> b -> b) -> __UF(a) b -> __KEY(a) -> __KEY(a) -> __UF(a) b
unionToLeftWithRep_ :: (__KEY(a) -> b -> b -> b) -> __UF(a) b -> __KEY(a) -> __KEY(a) -> __UF(a) b
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

union :: __UF(a) b -> __KEY(a) -> __KEY(a) -> (# __KEY(a), __UF(a) b #)
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
              | !o <- __KMAPPRE(insert) ii (IND jj) o -> (# jj,UF o #)
            EQ
              | !o <- __KMAPPRE(insert) jj (IND ii) o
              , !o <- __KMAPPRE(insert) ii (NODE (ri+1) si) o -> (# ii,UF o #)
            GT
              | !o <- __KMAPPRE(insert) jj (IND ii) o -> (# ii,UF o #)

unionWith :: (b -> b -> b) -> __UF(a) b -> __KEY(a) -> __KEY(a) -> (# __KEY(a), __UF(a) b #)
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
              | !o <- __KMAPPRE(insert) ii (IND jj) o
              , !o <- __KMAPPRE(insert) jj (NODE (rj+0) new) o -> (# jj,UF o #)
            EQ
              | !o <- __KMAPPRE(insert) jj (IND ii) o
              , !o <- __KMAPPRE(insert) ii (NODE (ri+1) new) o -> (# ii,UF o #)
            GT
              | !o <- __KMAPPRE(insert) jj (IND ii) o
              , !o <- __KMAPPRE(insert) ii (NODE (ri+0) new) o -> (# ii,UF o #)

unionWithRep :: (__KEY(a) -> b -> b -> b) -> __UF(a) b -> __KEY(a) -> __KEY(a) -> (# __KEY(a), __UF(a) b #)
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
              , !o <- __KMAPPRE(insert) ii (IND jj) o
              , !o <- __KMAPPRE(insert) jj (NODE (rj+0) new) o -> (# jj,UF o #)
            EQ
              | new <- f ii si sj
              , !o <- __KMAPPRE(insert) jj (IND ii) o
              , !o <- __KMAPPRE(insert) ii (NODE (ri+1) new) o -> (# ii,UF o #)
            GT
              | new <- f ii si sj
              , !o <- __KMAPPRE(insert) jj (IND ii) o
              , !o <- __KMAPPRE(insert) ii (NODE (ri+0) new) o -> (# ii,UF o #)

unionAndSet :: __UF(a) b -> __KEY(a) -> __KEY(a) -> b -> (# __KEY(a), __UF(a) b #)
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
              | !o <- __KMAPPRE(insert) ii (IND jj) o
              , !o <- __KMAPPRE(insert) jj (NODE (rj+0) new) o -> (# jj,UF o #)
            EQ
              | !o <- __KMAPPRE(insert) jj (IND ii) o
              , !o <- __KMAPPRE(insert) ii (NODE (ri+1) new) o -> (# ii,UF o #)
            GT
              | !o <- __KMAPPRE(insert) jj (IND ii) o
              , !o <- __KMAPPRE(insert) ii (NODE (ri+0) new) o -> (# ii,UF o #)

unionAndSetWithRep :: __UF(a) b -> __KEY(a) -> __KEY(a) -> (__KEY(a) -> b) -> (# __KEY(a), __UF(a) b #)
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
              , !o <- __KMAPPRE(insert) ii (IND jj) o
              , !o <- __KMAPPRE(insert) jj (NODE (rj+0) new) o -> (# jj,UF o #)
            EQ
              | new <- f ii
              , !o <- __KMAPPRE(insert) jj (IND ii) o
              , !o <- __KMAPPRE(insert) ii (NODE (ri+1) new) o -> (# ii,UF o #)
            GT
              | new <- f ii
              , !o <- __KMAPPRE(insert) jj (IND ii) o
              , !o <- __KMAPPRE(insert) ii (NODE (ri+0) new) o -> (# ii,UF o #)

-- | Makes the *second* (right) @__KEY(a)@
--  point to the *first* (left) @__KEY(a)@, regardless
--  of rank. The ranks are also updated appropriately.
unionToLeft :: __UF(a) b -> __KEY(a) -> __KEY(a) -> (# __KEY(a), __UF(a) b #)
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
              | !o <- __KMAPPRE(insert) jj (IND ii) o
              , !o <- __KMAPPRE(insert) ii (NODE (rj+1) si) o -> (# ii,UF o #)
            EQ
              | !o <- __KMAPPRE(insert) jj (IND ii) o
              , !o <- __KMAPPRE(insert) ii (NODE (ri+1) si) o -> (# ii,UF o #)
            GT
              | !o <- __KMAPPRE(insert) jj (IND ii) o -> (# ii,UF o #)

-- | Makes the *second* (right) @__KEY(a)@
--  point to the *first* (left) @__KEY(a)@, regardless
--  of rank. The ranks are also updated appropriately.
unionToLeftAndSet :: __UF(a) b -> __KEY(a) -> __KEY(a) -> b -> (# __KEY(a), __UF(a) b #)
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
              | !o <- __KMAPPRE(insert) jj (IND ii) o
              , !o <- __KMAPPRE(insert) ii (NODE (rj+1) new) o -> (# ii,UF o #)
            EQ
              | !o <- __KMAPPRE(insert) jj (IND ii) o
              , !o <- __KMAPPRE(insert) ii (NODE (ri+1) new) o -> (# ii,UF o #)
            GT
              | !o <- __KMAPPRE(insert) jj (IND ii) o
              , !o <- __KMAPPRE(insert) ii (NODE (ri+0) new) o -> (# ii,UF o #)

unionToLeftAndSetWithRep :: __UF(a) b -> __KEY(a) -> __KEY(a) -> (__KEY(a) -> b) -> (# __KEY(a), __UF(a) b #)
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
              | !o <- __KMAPPRE(insert) jj (IND ii) o
              , !o <- __KMAPPRE(insert) ii (NODE (rj+1) new) o -> (# ii,UF o #)
            EQ
              | !o <- __KMAPPRE(insert) jj (IND ii) o
              , !o <- __KMAPPRE(insert) ii (NODE (ri+1) new) o -> (# ii,UF o #)
            GT
              | !o <- __KMAPPRE(insert) jj (IND ii) o
              , !o <- __KMAPPRE(insert) ii (NODE (ri+0) new) o -> (# ii,UF o #)

unionToLeftWith :: (b -> b -> b) -> __UF(a) b -> __KEY(a) -> __KEY(a) -> (# __KEY(a), __UF(a) b #)
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
              | !o <- __KMAPPRE(insert) jj (IND ii) o
              , !o <- __KMAPPRE(insert) ii (NODE (rj+1) new) o -> (# ii, UF o #)
            EQ
              | !o <- __KMAPPRE(insert) jj (IND ii) o
              , !o <- __KMAPPRE(insert) ii (NODE (ri+1) new) o -> (# ii, UF o #)
            GT
              | !o <- __KMAPPRE(insert) jj (IND ii) o
              , !o <- __KMAPPRE(insert) ii (NODE (ri+0) new) o -> (# ii, UF o #)

unionToLeftWithRep :: (__KEY(a) -> b -> b -> b) -> __UF(a) b -> __KEY(a) -> __KEY(a) -> (# __KEY(a), __UF(a) b #)
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
              | !o <- __KMAPPRE(insert) jj (IND ii) o
              , !o <- __KMAPPRE(insert) ii (NODE (rj+1) new) o -> (# ii, UF o #)
            EQ
              | !o <- __KMAPPRE(insert) jj (IND ii) o
              , !o <- __KMAPPRE(insert) ii (NODE (ri+1) new) o -> (# ii, UF o #)
            GT
              | !o <- __KMAPPRE(insert) jj (IND ii) o
              , !o <- __KMAPPRE(insert) ii (NODE (ri+0) new) o -> (# ii, UF o #)

{-
unionWithM :: (Monad m) => (b -> b -> m b) -> __UF(a) b -> __KEY(a) -> __KEY(a) -> m (__UF(a) b)
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
              | !o <- __KMAPPRE(insert) ii (IND jj) o
              , !o <- __KMAPPRE(insert) jj (NODE (rj+0) new) o -> return (UF o)
            EQ
              | !o <- __KMAPPRE(insert) jj (IND ii) o
              , !o <- __KMAPPRE(insert) ii (NODE (ri+1) new) o -> return (UF o)
            GT
              | !o <- __KMAPPRE(insert) jj (IND ii) o
              , !o <- __KMAPPRE(insert) ii (NODE (ri+0) new) o -> return (UF o)

unionWithRepM :: (Monad m) => (__KEY(a) -> b -> b -> m b) -> __UF(a) b -> __KEY(a) -> __KEY(a) -> m (__UF(a) b)
unionWithRepM f uf i j
  | (# ii,ri,si,uf #) <- findR uf i
  , (# jj,rj,sj,uf #) <- findR uf j
  = case ii==jj of
      True-> return uf
      False
        | UF o <- uf
        -> case compare ri rj of
            LT
              | !o <- __KMAPPRE(insert) ii (IND jj) o -> do
                new <- f jj si sj
                case () of
                  () | !o <- __KMAPPRE(insert) jj (NODE (rj+0) new) o
                    -> return (UF o)
            EQ
              | !o <- __KMAPPRE(insert) jj (IND ii) o-> do
                new <- f ii si sj
                case () of
                  () | !o <- __KMAPPRE(insert) ii (NODE (ri+1) new) o
                    -> return (UF o)
            GT
              | !o <- __KMAPPRE(insert) jj (IND ii) o-> do
                new <- f ii si sj
                case () of
                  () | !o <- __KMAPPRE(insert) ii (NODE (ri+0) new) o
                    -> return (UF o)

unionToLeftWithM :: (Monad m) => (b -> b -> m b) -> __UF(a) b -> __KEY(a) -> __KEY(a) -> m (__UF(a) b)
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
              | !o <- __KMAPPRE(insert) jj (IND ii) o
              , !o <- __KMAPPRE(insert) ii (NODE (rj+1) new) o -> return (UF o)
            EQ
              | !o <- __KMAPPRE(insert) jj (IND ii) o
              , !o <- __KMAPPRE(insert) ii (NODE (ri+1) new) o -> return (UF o)
            GT
              | !o <- __KMAPPRE(insert) jj (IND ii) o
              , !o <- __KMAPPRE(insert) ii (NODE (ri+0) new) o -> return (UF o)

unionToLeftWithRepM :: (Monad m) => (__KEY(a) -> b -> b -> m b) -> __UF(a) b -> __KEY(a) -> __KEY(a) -> m (__UF(a) b)
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
              | !o <- __KMAPPRE(insert) jj (IND ii) o
              , !o <- __KMAPPRE(insert) ii (NODE (rj+1) new) o -> return (UF o)
            EQ
              | !o <- __KMAPPRE(insert) jj (IND ii) o
              , !o <- __KMAPPRE(insert) ii (NODE (ri+1) new) o -> return (UF o)
            GT
              | !o <- __KMAPPRE(insert) jj (IND ii) o
              , !o <- __KMAPPRE(insert) ii (NODE (ri+0) new) o -> return (UF o)
-}

-----------------------------------------------------------------------------

unionWithAlt :: (__UF(a) b -> __KEY(a) -> b -> b -> (b, __UF(a) b)) -> __UF(a) b -> __KEY(a) -> __KEY(a) -> __UF(a) b
unionWithAlt f uf i j
  | (# ii,ri,si,uf #) <- findR uf i
  , (# jj,rj,sj,uf #) <- findR uf j
  = case ii==jj of
      True-> uf
      False
        | UF o <- uf
        -> case compare ri rj of
            LT->
              let !o2 = __KMAPPRE(insert) ii (IND jj) o
                  o3 = __KMAPPRE(insert) jj (NODE (rj+0) s2) o2
                  (s2,out) = f (UF o3) jj si sj
                  UF !o4 = out in UF o4
            EQ->
              let !o2 = __KMAPPRE(insert) jj (IND ii) o
                  o3 = __KMAPPRE(insert) ii (NODE (ri+1) s2) o2
                  (s2,out) = f (UF o3) ii si sj
                  UF !o4 = out in UF o4
            GT->
              let !o2 = __KMAPPRE(insert) jj (IND ii) o
                  o3 = __KMAPPRE(insert) ii (NODE (ri+0) s2) o2
                  (s2,out) = f (UF o3) ii si sj
                  UF !o4 = out in UF o4

-----------------------------------------------------------------------------

compress :: __UF(a) b -> __UF(a) b
compress uf@(UF o) = go mempty uf (__KMAPPRE(keys) o)
  where go seen uf [] = uf
        go seen uf (i:is)
          | i`__KSETPRE(member)`seen
          = go seen uf is
          | (# j,_,!uf #) <- find uf i
          = case i==j of
              True
                -> go seen uf is
              False
                | !seen <- j`__KSETPRE(insert)`seen
                -> go seen uf is

compressAndIndex :: __UF(a) b -> (__KEYMAP(a,(__KEY(a))), __UF(a) b)
compressAndIndex uf@(UF o) = go mempty uf mempty (__KMAPPRE(keys) o)
  where go seen uf ix [] = (ix,uf)
        go seen uf ix (i:is)
          | i`__KSETPRE(member)`seen
          = go seen uf ix is
          | (# j,_,!uf #) <- find uf i
          = case i==j of
              True
                | !ix   <- __KMAPPRE(insert) i i ix
                -> go seen uf ix is
              False
                | !seen <- j`__KSETPRE(insert)`seen
                , !ix   <- __KMAPPRE(insert) i j ix
                , !ix   <- __KMAPPRE(insert) j j ix
                -> go seen uf ix is

support :: __UF(a) b -> (__KEYMAP(a,(__KEY(a))), __UF(a) b)
support uf@(UF o) = go mempty uf mempty (__KMAPPRE(keys) o)
  where go seen uf ix [] = (ix,uf)
        go seen uf ix (i:is)
          | i`__KSETPRE(member)`seen
          = go seen uf ix is
          | (# j,_,!uf #) <- find uf i
          = case i==j of
              True
                -> go seen uf ix is
              False
                | !seen <- j`__KSETPRE(insert)`seen
                , !ix   <- __KMAPPRE(insert) i j ix
                -> go seen uf ix is

supportAndSize :: __UF(a) b -> (Int, __KEYMAP(a,(__KEY(a))), __UF(a) b)
supportAndSize uf@(UF o) = go 0 mempty uf mempty (__KMAPPRE(keys) o)
  where go n seen uf ix [] = (n,ix,uf)
        go n seen uf ix (i:is)
          | i`__KSETPRE(member)`seen
          = go n seen uf ix is
          | (# j,_,!uf #) <- find uf i
          = case i==j of
              True
                -> go n seen uf ix is
              False
                | !seen <- j`__KSETPRE(insert)`seen
                , !ix   <- __KMAPPRE(insert) i j ix
                , !n    <- n + 1
                -> go n seen uf ix is

-----------------------------------------------------------------------------

-- | \"Garbage\" collection. Only representatives are retained in the
--  returned @__UF(a) b@. The @IxMap a (__KEY(a))@ gives a mapping of old @__KEY(a)@s
--  in the incoming @__UF(a) b@ to new @__KEY(a)@s in the returned @__UF(a) b@.
collect :: __UF(a) b -> (__KEYMAP(a,(__KEY(a))), __UF(a) b)
collect uf@(UF o) = go mempty mempty uf mempty (__KMAPPRE(keys) o)
  where go new seen uf ix [] = (ix,UF new)
        go new seen uf ix (i:is)
          | i`__KSETPRE(member)`seen
          = go new seen uf ix is
          | (# j,b,!uf #) <- find uf i
          = case i==j of
              True
                | !node <- newU b
                , !new  <- __KMAPPRE(insert) i node new
                , !ix   <- __KMAPPRE(insert) i i ix
                , !seen <- j`__KSETPRE(insert)`seen
                -> go new seen uf ix is
              False
                | j`__KSETPRE(member)`seen
                , !ix <- __KMAPPRE(insert) i j ix
                -> go new seen uf ix is
                | !node <- newU b
                , !new  <- __KMAPPRE(insert) j node new
                , !ix   <- __KMAPPRE(insert) i j ix
                , !ix   <- __KMAPPRE(insert) j j ix
                , !seen <- j`__KSETPRE(insert)`seen
                -> go new seen uf ix is

collect_ :: __UF(a) b -> (__KEYMAP(a,(__KEY(a))), __UF(a) b)
collect_ uf@(UF o) = go mempty mempty uf mempty (__KMAPPRE(keys) o)
  where go new seen uf ix [] = (ix,UF new)
        go new seen uf ix (i:is)
          | i`__KSETPRE(member)`seen
          = go new seen uf ix is
          | (# j,b,!uf #) <- find uf i
          = case i==j of
              True
                | !node <- newU b
                , !new  <- __KMAPPRE(insert) i node new
                , !seen <- j`__KSETPRE(insert)`seen
                -> go new seen uf ix is
              False
                | j`__KSETPRE(member)`seen
                , !ix <- __KMAPPRE(insert) i j ix
                -> go new seen uf ix is
                | !node <- newU b
                , !new  <- __KMAPPRE(insert) j node new
                , !ix   <- __KMAPPRE(insert) i j ix
                , !seen <- j`__KSETPRE(insert)`seen
                -> go new seen uf ix is

collectAndReindexFrom :: Int -> __UF(a) b -> (Int, __KEYMAP(a,(__KEY(a))), __UF(a) b)
collectAndReindexFrom !n uf@(UF o) = go n mempty mempty uf mempty (__KMAPPRE(keys) o)
  where go n new seen uf ix [] = (n,ix,UF new)
        go n new seen uf ix (i:is)
          | i`__KSETPRE(member)`seen
          = go n new seen uf ix is
          | (# j,b,!uf #) <- find uf i
          = case i==j of
              True
                | jnew  <- __KEYVAL(n)
                , !n    <- n + 1
                , !node <- newU b
                , !new  <- __KMAPPRE(insert) jnew node new
                , !ix   <- __KMAPPRE(insert) j jnew ix
                , !seen <- j`__KSETPRE(insert)`seen
                -> go n new seen uf ix is
              False
                | j`__KSETPRE(member)`seen
                , !jnew <- ix __KMAPPRE(!) j
                , !ix   <- __KMAPPRE(insert) i jnew ix
                -> go n new seen uf ix is
                | jnew  <- __KEYVAL(n)
                , !n    <- n + 1
                , !node <- newU b
                , !new  <- __KMAPPRE(insert) jnew node new
                , !ix   <- __KMAPPRE(insert) i jnew ix
                , !ix   <- __KMAPPRE(insert) j jnew ix
                , !seen <- j`__KSETPRE(insert)`seen
                -> go n new seen uf ix is

-----------------------------------------------------------------------------

quotientAlt :: __UF(a) b -> (__KEYMAP(a,(__KEY(b))), __KEYMAP(b,b))
quotientAlt o
  | (f,g) <- quotient o
  --, f     <- castIxArg f
  --, g     <- __KMAPPRE(castIxMap) g
  = (f,g)

quotientAndReindexFromAlt :: Int -> __UF(a) b -> (Int, __KEYMAP(a,(__KEY(b))), __KEYMAP(b,b))
quotientAndReindexFromAlt n o
  | (n,f,g) <- quotientAndReindexFrom n o
  --, f       <- castIxArg f
  --, g       <- __KMAPPRE(castIxMap) g
  = (n,f,g)

-- | Identical to @collect@, except an
--  @IxMap a b@ is returned instead of a @__UF(a) b@.
quotient :: __UF(a) b -> (__KEYMAP(a,(__KEY(a))), __KEYMAP(a,b))
quotient uf@(UF o) = go mempty mempty uf mempty (__KMAPPRE(keys) o)
  where go new seen uf ix [] = (ix,new)
        go new seen uf ix (i:is)
          | i`__KSETPRE(member)`seen
          = go new seen uf ix is
          | (# j,b,!uf #) <- find uf i
          = case i==j of
              True
                | !new  <- __KMAPPRE(insert) i b new
                , !ix   <- __KMAPPRE(insert) i i ix
                , !seen <- j`__KSETPRE(insert)`seen
                -> go new seen uf ix is
              False
                | j`__KSETPRE(member)`seen
                , !ix <- __KMAPPRE(insert) i (cast__KEY(j)) ix
                -> go new seen uf ix is
                | !new  <- __KMAPPRE(insert) (cast__KEY(j)) b new
                , !ix   <- __KMAPPRE(insert) i j ix
                , !ix   <- __KMAPPRE(insert) j j ix
                , !seen <- j`__KSETPRE(insert)`seen
                -> go new seen uf ix is

-- | Identical to @collect_@, except an
--  @IxMap a b@ is returned instead of a @__UF(a) b@.
quotient_ :: __UF(a) b -> (__KEYMAP(a,(__KEY(a))), __KEYMAP(a,b))
quotient_ uf@(UF o) = go mempty mempty uf mempty (__KMAPPRE(keys) o)
  where go new seen uf ix [] = (ix,new)
        go new seen uf ix (i:is)
          | i`__KSETPRE(member)`seen
          = go new seen uf ix is
          | (# j,b,!uf #) <- find uf i
          = case i==j of
              True
                | !new  <- __KMAPPRE(insert) i b new
                , !seen <- j`__KSETPRE(insert)`seen
                -> go new seen uf ix is
              False
                | j`__KSETPRE(member)`seen
                , !ix <- __KMAPPRE(insert) i (cast__KEY(j)) ix
                -> go new seen uf ix is
                | !new  <- __KMAPPRE(insert) (cast__KEY(j)) b new
                , !ix   <- __KMAPPRE(insert) i j ix
                , !seen <- j`__KSETPRE(insert)`seen
                -> go new seen uf ix is

-- | Identical to @collectAndReindexFrom@,
--  except an @IxMap a b@ is returned instead of a @__UF(a) b@.
quotientAndReindexFrom :: Int -> __UF(a) b -> (Int, __KEYMAP(a,(__KEY(a))), __KEYMAP(a,b))
quotientAndReindexFrom !n uf@(UF o) = go n mempty mempty uf mempty (__KMAPPRE(keys) o)
  where go n new seen uf ix [] = (n,ix,new)
        go n new seen uf ix (i:is)
          | i`__KSETPRE(member)`seen
          = go n new seen uf ix is
          | (# j,b,!uf #) <- find uf i
          = case i==j of
              True
                | jnew  <- __KEYVAL(n)
                , !n    <- n + 1
                , !new  <- __KMAPPRE(insert) jnew b new
                , !ix   <- __KMAPPRE(insert) j jnew ix
                , !seen <- j`__KSETPRE(insert)`seen
                -> go n new seen uf ix is
              False
                | j`__KSETPRE(member)`seen
                , !jnew <- ix __KMAPPRE(!) j
                , !ix   <- __KMAPPRE(insert) i jnew ix
                -> go n new seen uf ix is
                | jnew  <- __KEYVAL(n)
                , !n    <- n + 1
                , !new  <- __KMAPPRE(insert) jnew b new
                , !ix   <- __KMAPPRE(insert) i jnew ix
                , !ix   <- __KMAPPRE(insert) j jnew ix
                , !seen <- j`__KSETPRE(insert)`seen
                -> go n new seen uf ix is

-----------------------------------------------------------------------------

instance (Join b) => Join (__UF(a) b) where
  (\/) a b = joinWith (\/) a b

joinWith :: (b -> b -> b) -> __UF(a) b -> __UF(a) b -> __UF(a) b
joinWith op (UF f) uf
  | (rn,UF g) <- collect_ uf
  , !u <- g`__KMAPPRE(difference)`f
  , !v <- g`__KMAPPRE(intersection)`f
  , !new <- UF (f`__KMAPPRE(union)`u)
  , !new <- __KMAPPRE(foldWithKey) one new v
  , !new <- __KMAPPRE(foldWithKey) two new rn = new
  where
        one i (NODE _ b2) new
          | (# i,r,b1,uf #) <- findR new i
          , UF o <- uf
          , !b <- op b1 b2
          , !node <- NODE r b
          , !o <- __KMAPPRE(insert) i node o
          = UF o

        two i j new
          | (# Just i,new #) <- tryRep new i
          = union_ new i j
          | (# j,r,b,uf #) <- findR new j
          , UF o <- uf
          , !o <- case r < 1 of
                    True-> __KMAPPRE(insert) j (NODE 1 b) o
                    False-> o
          , !o <- __KMAPPRE(insert) i (IND j) o
          = UF o

-----------------------------------------------------------------------------

getInds :: __UF(a) b -> __KEY(a) -> __KEYSET(a)
getInds uf _ = mempty

setInds :: __UF(a) b -> __KEY(a) -> __KEYSET(a) -> __UF(a) b
setInds uf _ _ = uf

getAndClearInds :: __UF(a) b -> __KEY(a) -> (# __KEYSET(a), __UF(a) b #)
getAndClearInds uf i
  | inds <- getInds uf i
  , !uf <- clearInds uf i
  = (# inds,uf #)

mapInds :: (__KEYSET(a) -> __KEYSET(a)) -> __UF(a) b -> __KEY(a) -> __UF(a) b
mapInds f uf i
  | inds <- getInds uf i
  , !inds <- f inds
  = setInds uf i inds

popDirty :: __UF(a) b -> (# Maybe (__KEY(a)), __UF(a) b #)
popDirty uf = (# Nothing,uf #)

clearInds :: __UF(a) b -> __KEY(a) -> __UF(a) b
clearInds uf i = setInds uf i mempty

-----------------------------------------------------------------------------

statsMaxIndChain :: __UF(a) b -> Int
statsMaxIndChain (UF o) = fold 0 mempty (__KMAPPRE(keys) o)
  where fold mx seen [] = mx
        fold mx seen (i:is)
          | (# seen,mx #) <- go mx 0 seen i
          = fold mx seen is
        go mx n seen i
          | IND j <- o __KMAPPRE(!) i
          , !n <- n+1
          , !seen <- i`__KSETPRE(insert)`seen
          = go mx n seen j
          | !mx <- max mx n
          , !seen <- i`__KSETPRE(insert)`seen
          = (# seen,mx #)

-----------------------------------------------------------------------------

ppUF :: (Show b) => __UF(a) b -> [(String,[String])]
ppUF (UF m) =
  __KMAPPRE(foldWithKey) (\i node acc->
    case node of
      NODE{}-> (show node,[]):acc
      IND j-> (show node,[show(m __KMAPPRE(!) j)]):acc) [] m

ppUFAlt :: (Show b) => __UF(a) b -> [(String,[String])]
ppUFAlt (UF m) =
  __KMAPPRE(foldWithKey) (\i node acc->
    case node of
      NODE{}-> (show node,[]):acc
      IND j-> (ppU i node,[ppU j (m __KMAPPRE(!) j)]):acc) [] m

ppU :: (Show b) => __KEY(a) -> __U(a) b -> String
ppU (__KEYPAT(i)) o@(IND{}) = show i ++ ":IND"
ppU (__KEYPAT(i)) o@(NODE{}) = show i ++ ":" ++ show o

-----------------------------------------------------------------------------

#if 0
instance UF.UF (__UF(a) b) a b where
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
  -- fromKeyMap = fromKeyMap
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
#endif

-----------------------------------------------------------------------------

