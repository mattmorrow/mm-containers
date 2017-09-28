
module MM.Data.Trie.Int (
   IntTrie
  ,empty,null,elems
  ,insert,insertWith,lookup
  ,fromList,toList,toTree
) where
import MM.Data.Tree.Rose
import MM.Data.Map.Int(IntMap)
import qualified MM.Data.Map.Int as IM
import Data.Monoid(Monoid(..))
import Data.List(foldl')
import Control.Applicative(Applicative(..))
import Prelude hiding (lookup,null)


-- | .
data IntTrie b = Trie
  !(Maybe b)
  !(IntMap (IntTrie b))
  deriving(Eq,Ord,Read,Show)

instance Functor IntTrie where
  fmap f (Trie a m) = Trie (fmap f a)
                           ((fmap . fmap) f m)

instance (Monoid b) => Monoid (IntTrie b) where
  mempty = Trie mempty mempty
  Trie a b `mappend` Trie c d
    = Trie (a `mappend` c)
            (b `mappend` d)

instance Applicative IntTrie where
  pure a = Trie (pure a) mempty
  Trie a m <*> Trie b n = Trie (a <*> b)
            (IM.intersectionWith (<*>) m n)

empty :: IntTrie b
empty = Trie Nothing mempty

null :: IntTrie b -> Bool
null (Trie Nothing m) = IM.null m
null _                = False

fromList :: [([Int], b)] -> IntTrie b
fromList = foldl' ((flip . uncurry) insert) empty


toList :: IntTrie b -> [([Int], b)]
toList trie = maybe [] (\a->[([],a)]) a
                            ++ (go [] =<< ts)
  where (a, ts) = toTree trie
        go acc (Node (a,c) ts)
          = maybe (go (c:acc) =<< ts)
                  (\a -> (reverse (c:acc),a)
                          : (go (c:acc) =<< ts)) a

elems :: IntTrie b -> [b]
elems (Trie Nothing m) = elems =<< IM.elems m
elems (Trie (Just b) m) = b : (elems =<< IM.elems m)


toTree :: IntTrie b -> (Maybe b, [Tree (Maybe b,Int)])
toTree (Trie a m) = (a, go m)
  where reassoc (k, Trie a m) = Node (a,k) (go m)
        go = fmap reassoc . IM.toList


insert :: [Int] -> b -> IntTrie b -> IntTrie b
insert [] a (Trie _ m) = Trie (Just a) m
insert (k:ks) a (Trie x m)
  | t <- maybe empty id (IM.lookup k m)
  , t <- insert ks a t
  , m <- IM.insert k t m
  = Trie x m


insertWith :: (b -> b -> b) -> [Int] -> b -> IntTrie b -> IntTrie b
insertWith f = go
  where go [] new (Trie Nothing m)
          = Trie (Just new) m
        go [] new (Trie (Just old) m)
          | new <- f new old
          = Trie (Just new) m
        go (k:ks) new (Trie x m)
          | t <- maybe empty id (IM.lookup k m)
          , t <- go ks new t
          , m <- IM.insert k t m
          = Trie x m



lookup :: [Int] -> IntTrie b -> Maybe b
lookup [] (Trie a _) = a
lookup (k:ks) (Trie _ m) = lookup ks =<< IM.lookup k m



