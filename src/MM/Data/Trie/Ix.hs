
module MM.Data.Ix.Trie (
   IxTrie,TrieIx
  ,empty,null,insert,lookup
  ,fromList,toList,toTree
  ,elems,insertWith
) where
import MM.Data.Tree.Rose
import MM.Data.Map.Ord(Map)
import MM.Data.Map.Int(IntMap)
import qualified MM.Data.Map.Ord as M
import qualified MM.Data.Map.Int as IM
import MM.Data.Ix.Map(IxMap)
import MM.Data.Ix.Types(Ix(..),unIx)
import qualified MM.Data.Ix.Map as Ix
import Data.Monoid(Monoid(..))
import Data.List(foldl')
import Control.Applicative(Applicative(..))
import Prelude hiding (lookup,null)
import Control.Monad(ap)


import qualified Data.Binary as Bin
instance (Bin.Binary b) => Bin.Binary (IxTrie a b) where
  put (Trie a b) = Bin.put a >> Bin.put b
  get = do a <- Bin.get; b <- Bin.get; return (Trie a b)




-- | .
type TrieIx a b = Ix (IxTrie a b)
data IxTrie a b = Trie
  !(Maybe b)
  !(IxMap a (IxTrie a b))
  deriving(Eq,Ord,Read,Show)
instance Functor (IxTrie a) where
  fmap f (Trie a m) = Trie (fmap f a)
                           ((fmap . fmap) f m)
instance (Monoid b) => Monoid (IxTrie a b) where
  mempty = Trie mempty mempty
  Trie a b `mappend` Trie c d
    = Trie (a `mappend` c)
            (b `mappend` d)
instance Applicative (IxTrie a) where
  pure a = Trie (pure a) mempty
  Trie a m <*> Trie b n = Trie (a <*> b)
            (Ix.intersectionWith (<*>) m n)
empty :: IxTrie a b
empty = Trie Nothing mempty
null :: IxTrie a b -> Bool
null (Trie Nothing m) = Ix.null m
null _                = False
fromList :: [([Ix a],b)] -> IxTrie a b
fromList = foldl' ((flip . uncurry) insert) empty
toList :: IxTrie a b -> [([Ix a],b)]
toList trie = maybe [] (\a->[([],a)]) a
                            ++ (go [] =<< ts)
  where (a, ts) = toTree trie
        go acc (Node (a,c) ts)
          = maybe (go (c:acc) =<< ts)
                  (\a -> (reverse (c:acc),a)
                          : (go (c:acc) =<< ts)) a
toTree :: IxTrie a b -> (Maybe b,[Tree (Maybe b,Ix a)])
toTree (Trie a m) = (a, go m)
  where reassoc (k, Trie a m) = Node (a,k) (go m)
        go = fmap reassoc . Ix.toList
insert :: [Ix a] -> b -> IxTrie a b -> IxTrie a b
insert [] a (Trie _ m) = Trie (Just a) m
insert (k:ks) a (Trie x m) = let t = maybe empty id (Ix.lookup k m)
                                 t' = insert ks a t
                              in Trie x (Ix.insert k t' m)
lookup :: [Ix a] -> IxTrie a b -> Maybe b
lookup [] (Trie a _) = a
lookup (k:ks) (Trie _ m) = lookup ks =<< Ix.lookup k m


elems :: IxTrie a b -> [b]
elems (Trie Nothing m) = elems =<< Ix.elems m
elems (Trie (Just b) m) = b : (elems =<< Ix.elems m)



insertWith :: (b -> b -> b) -> [Ix a] -> b -> IxTrie a b -> IxTrie a b
insertWith f = go
  where go [] new (Trie Nothing m)
          = Trie (Just new) m
        go [] new (Trie (Just old) m)
          | new <- f new old
          = Trie (Just new) m
        go (k:ks) new (Trie x m)
          | t <- maybe empty id (Ix.lookup k m)
          , t <- go ks new t
          , m <- Ix.insert k t m
          = Trie x m











