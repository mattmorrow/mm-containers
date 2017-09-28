

module MM.Data.Trie.Ord (
   Trie(..)
  ,empty,null
  ,fromList,toList,toTree
  ,insert,lookup
) where

import MM.Data.Tree.Rose
import MM.Data.Map.Ord(Map)
import MM.Data.Map.Int(IntMap)
import qualified MM.Data.Map.Ord as M
import qualified MM.Data.Map.Int as IM
import Data.Monoid(Monoid(..))
import Data.List(foldl')
import Control.Applicative(Applicative(..))
import Prelude hiding (lookup,null)

-- | .
data Trie k a = Trie (Maybe a)
              !(Map k (Trie k a))
  deriving(Eq,Ord,Read,Show)

instance (Ord k) => Functor (Trie k) where
  fmap f (Trie a m) = Trie (fmap f a)
                            ((fmap . fmap) f m)

instance (Ord k, Monoid a) => Monoid (Trie k a) where
  mempty = Trie mempty mempty
  Trie a b `mappend` Trie c d
    = Trie (a `mappend` c)
            (b `mappend` d)

instance (Ord k) => Applicative (Trie k) where
  pure a = Trie (pure a) mempty
  Trie a m <*> Trie b n = Trie (a <*> b)
            (M.intersectionWith (<*>) m n)

empty :: (Ord k) => Trie k a
empty = Trie Nothing mempty

null :: (Ord k) => Trie k a -> Bool
null (Trie Nothing m) = M.null m
null _                = False

fromList :: (Ord k) => [([k], a)] -> Trie k a
fromList = foldl' ((flip . uncurry) insert) empty

toList :: (Ord k) => Trie k a -> [([k], a)]
toList trie = maybe [] (\a->[([],a)]) a
                            ++ (go [] =<< ts)
  where (a, ts) = toTree trie
        go acc (Node (a,c) ts)
          = maybe (go (c:acc) =<< ts)
                  (\a -> (reverse (c:acc),a)
                          : (go (c:acc) =<< ts)) a

toTree :: (Ord k) => Trie k a -> (Maybe a, [Tree (Maybe a, k)])
toTree (Trie a m) = (a, go m)
  where reassoc (k, Trie a m) = Node (a,k) (go m)
        go = fmap reassoc . M.toList

insert :: (Ord k) => [k] -> a -> Trie k a -> Trie k a
insert [] a (Trie _ m) = Trie (Just a) m
insert (k:ks) a (Trie x m) = let t = maybe empty id (M.lookup k m)
                                 t' = insert ks a t
                              in Trie x (M.insert k t' m)

lookup :: (Ord k) => [k] -> Trie k a -> Maybe a
lookup [] (Trie a _) = a
lookup (k:ks) (Trie _ m) = lookup ks =<< M.lookup k m

