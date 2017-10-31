#ifndef _MM_CONTAINERS_H

/***************************************************************************/

#define __H #
#define HS_UTUP(a,b)        (__H a,b __H)
#define HS_UTUP3(a,b,c)     (__H a,b,c __H)
#define __PRAGMA(prag,o)    {-__H prag o __H-};

/***************************************************************************/

//
// You MUST use this OPTIONS pragma for most of this file to work:
//
#define __OPTIONS_CPP     __PRAGMA(OPTIONS,-pgmP cpp -optP-w)

#define __INLINE(o)       __PRAGMA(INLINE,o)
#define __NOINLINE(o)     __PRAGMA(NOINLINE,o)

/***************************************************************************/

#define IMPORT_MM_CONTAINERS_BASE\
  import MM.Data.Types.Ix;\
  import MM.Data.Map.Ord(Map);\
  import MM.Data.Set.Ord(Set);\
  import MM.Data.Map.Int(IntMap);\
  import MM.Data.Map.Ix(IxMap);\
  import MM.Data.Set.Int(IntSet);\
  import MM.Data.Set.Ix(IxSet);\
  import MM.Data.Trie.Int(IntTrie);\
  import MM.Data.Trie.Ix(IxTrie);\
  import MM.Data.Tree.Rose(Tree(..));\
  import qualified MM.Data.Map.Ord as OM;\
  import qualified MM.Data.Map.Int as IM;\
  import qualified MM.Data.Map.Ix as Ix;\
  import qualified MM.Data.Set.Ord as OS;\
  import qualified MM.Data.Set.Int as IS;\
  import qualified MM.Data.Set.Ix as IxS;\
  import qualified MM.Data.Trie.Ord as OT;\
  import qualified MM.Data.Trie.Int as IT;\
  import qualified MM.Data.Trie.Ix as IxT;\
  import qualified MM.Data.Tree.Rose as TR;\
  import MM.Data.Class.Base;\
  import qualified MM.Data.Class.Maps as M;

#define IMPORT_MM_CONTAINERS_UF_Ix\
  import MM.Data.UnionFind.Ix(UF);\
  import qualified MM.Data.UnionFind.Ix as U;\
  import qualified MM.Data.Class.UnionFind.Ix as UF;

#define IMPORT_MM_CONTAINERS_UF_Int\
  import MM.Data.UnionFind.Int(UF);\
  import qualified MM.Data.UnionFind.Int as U;\
  import qualified MM.Data.Class.UnionFind.Int as UF;

#define IMPORT_MM_CONTAINERS_EXTERN\
  import Data.Int;\
  import Data.Word;\
  import Data.Char(ord,chr);\
  import Data.Bits;\
  import Foreign.Storable;\
  import Data.Function;\
  import Data.List(foldl',sort,sortBy);\
  import Data.Monoid(Monoid(..));\
  import Control.Applicative((<$>),(<*>));\
  import Control.Monad;

/***************************************************************************/

// {{{
#define INSTANCE_Monoid_1x0(A_t,CON,OP)\
  instance Monoid (A_t) where\
    {mempty = CON\
    ;mappend\
      (CON)\
      (CON)\
      = CON};
#define INSTANCE_Monoid_1x1(A_t,CON,OP)\
  instance Monoid (A_t) where\
    {mempty = CON\
      mempty\
    ;mappend\
      (CON a1)\
      (CON a2)\
      | !a <- OP a1 a2\
      = CON a};
#define INSTANCE_Monoid_1x2(A_t,CON,OP)\
  instance Monoid (A_t) where\
    {mempty = CON\
      mempty mempty\
    ;mappend\
      (CON a1 b1)\
      (CON a2 b2)\
      | !a <- OP a1 a2\
      , !b <- OP b1 b2\
      = CON a b};
#define INSTANCE_Monoid_1x3(A_t,CON,OP)\
  instance Monoid (A_t) where\
    {mempty = CON\
      mempty mempty mempty\
    ;mappend\
      (CON a1 b1 c1)\
      (CON a2 b2 c2)\
      | !a <- OP a1 a2\
      , !b <- OP b1 b2\
      , !c <- OP c1 c2\
      = CON a b c};
#define INSTANCE_Monoid_1x4(A_t,CON,OP)\
  instance Monoid (A_t) where\
    {mempty = CON\
      mempty mempty mempty mempty\
    ;mappend\
      (CON a1 b1 c1 d1)\
      (CON a2 b2 c2 d2)\
      | !a <- OP a1 a2\
      , !b <- OP b1 b2\
      , !c <- OP c1 c2\
      , !d <- OP d1 d2\
      = CON a b c d};
#define INSTANCE_Monoid_1x5(A_t,CON,OP)\
  instance Monoid (A_t) where\
    {mempty = CON\
      mempty mempty mempty mempty\
      mempty\
    ;mappend\
      (CON a1 b1 c1 d1 e1)\
      (CON a2 b2 c2 d2 e2)\
      | !a <- OP a1 a2\
      , !b <- OP b1 b2\
      , !c <- OP c1 c2\
      , !d <- OP d1 d2\
      , !e <- OP e1 e2\
      = CON a b c d e};
#define INSTANCE_Monoid_1x6(A_t,CON,OP)\
  instance Monoid (A_t) where\
    {mempty = CON\
      mempty mempty mempty mempty\
      mempty mempty\
    ;mappend\
      (CON a1 b1 c1 d1 e1 f1)\
      (CON a2 b2 c2 d2 e2 f2)\
      | !a <- OP a1 a2\
      , !b <- OP b1 b2\
      , !c <- OP c1 c2\
      , !d <- OP d1 d2\
      , !e <- OP e1 e2\
      , !f <- OP f1 f2\
      = CON a b c d e f};
#define INSTANCE_Monoid_1x7(A_t,CON,OP)\
  instance Monoid (A_t) where\
    {mempty = CON\
      mempty mempty mempty mempty\
      mempty mempty mempty\
    ;mappend\
      (CON a1 b1 c1 d1 e1 f1 g1)\
      (CON a2 b2 c2 d2 e2 f2 g2)\
      | !a <- OP a1 a2\
      , !b <- OP b1 b2\
      , !c <- OP c1 c2\
      , !d <- OP d1 d2\
      , !e <- OP e1 e2\
      , !f <- OP f1 f2\
      , !g <- OP g1 g2\
      = CON a b c d e f g};
#define INSTANCE_Monoid_1x8(A_t,CON,OP)\
  instance Monoid (A_t) where\
    {mempty = CON\
      mempty mempty mempty mempty\
      mempty mempty mempty mempty\
    ;mappend\
      (CON a1 b1 c1 d1 e1 f1 g1 h1)\
      (CON a2 b2 c2 d2 e2 f2 g2 h2)\
      | !a <- OP a1 a2\
      , !b <- OP b1 b2\
      , !c <- OP c1 c2\
      , !d <- OP d1 d2\
      , !e <- OP e1 e2\
      , !f <- OP f1 f2\
      , !g <- OP g1 g2\
      , !h <- OP h1 h2\
      = CON a b c d e f g h};
#define INSTANCE_Monoid_1x9(A_t,CON,OP)\
  instance Monoid (A_t) where\
    {mempty = CON\
      mempty mempty mempty mempty\
      mempty mempty mempty mempty\
      mempty\
    ;mappend\
      (CON a1 b1 c1 d1 e1 f1 g1 h1 i1)\
      (CON a2 b2 c2 d2 e2 f2 g2 h2 i2)\
      | !a <- OP a1 a2\
      , !b <- OP b1 b2\
      , !c <- OP c1 c2\
      , !d <- OP d1 d2\
      , !e <- OP e1 e2\
      , !f <- OP f1 f2\
      , !g <- OP g1 g2\
      , !h <- OP h1 h2\
      , !i <- OP i1 i2\
      = CON a b c d e f g h i};
#define INSTANCE_Monoid_1x10(A_t,CON,OP)\
  instance Monoid (A_t) where\
    {mempty = CON\
      mempty mempty mempty mempty\
      mempty mempty mempty mempty\
      mempty mempty\
    ;mappend\
      (CON a1 b1 c1 d1 e1 f1 g1 h1 i1 j1)\
      (CON a2 b2 c2 d2 e2 f2 g2 h2 i2 j2)\
      | !a <- OP a1 a2\
      , !b <- OP b1 b2\
      , !c <- OP c1 c2\
      , !d <- OP d1 d2\
      , !e <- OP e1 e2\
      , !f <- OP f1 f2\
      , !g <- OP g1 g2\
      , !h <- OP h1 h2\
      , !i <- OP i1 i2\
      , !j <- OP j1 j2\
      = CON a b c d e f g h i j};
#define INSTANCE_Monoid_1x11(A_t,CON,OP)\
  instance Monoid (A_t) where\
    {mempty = CON\
      mempty mempty mempty mempty\
      mempty mempty mempty mempty\
      mempty mempty mempty\
    ;mappend\
      (CON a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1)\
      (CON a2 b2 c2 d2 e2 f2 g2 h2 i2 j2 k2)\
      | !a <- OP a1 a2\
      , !b <- OP b1 b2\
      , !c <- OP c1 c2\
      , !d <- OP d1 d2\
      , !e <- OP e1 e2\
      , !f <- OP f1 f2\
      , !g <- OP g1 g2\
      , !h <- OP h1 h2\
      , !i <- OP i1 i2\
      , !j <- OP j1 j2\
      , !k <- OP k1 k2\
      = CON a b c d e f g h i j k};
#define INSTANCE_Monoid_1x12(CXT,A_t,CON,OP)\
  instance (CXT) => Monoid (A_t) where\
    {mempty = CON\
      mempty mempty mempty mempty\
      mempty mempty mempty mempty\
      mempty mempty mempty mempty\
    ;mappend\
      (CON a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1)\
      (CON a2 b2 c2 d2 e2 f2 g2 h2 i2 j2 k2 l2)\
      | !a <- OP a1 a2\
      , !b <- OP b1 b2\
      , !c <- OP c1 c2\
      , !d <- OP d1 d2\
      , !e <- OP e1 e2\
      , !f <- OP f1 f2\
      , !g <- OP g1 g2\
      , !h <- OP h1 h2\
      , !i <- OP i1 i2\
      , !j <- OP j1 j2\
      , !k <- OP k1 k2\
      , !l <- OP l1 l2\
      = CON a b c d e f g h i j k l};
// }}}

// {{{
#define INSTANCE_BINOP_1x0(CLASS,METHOD,CXT,A_t,CON,OP)\
  instance (CXT) => CLASS (A_t) where\
    {METHOD\
      (CON)\
      (CON)\
      = CON};
#define INSTANCE_BINOP_1x1(CLASS,METHOD,CXT,A_t,CON,OP)\
  instance (CXT) => CLASS (A_t) where\
    {METHOD\
      (CON a1)\
      (CON a2)\
      | !a <- OP a1 a2\
      = CON a};
#define INSTANCE_BINOP_1x2(CLASS,METHOD,CXT,A_t,CON,OP)\
  instance (CXT) => CLASS (A_t) where\
    {METHOD\
      (CON a1 b1)\
      (CON a2 b2)\
      | !a <- OP a1 a2\
      , !b <- OP b1 b2\
      = CON a b};
#define INSTANCE_BINOP_1x3(CLASS,METHOD,CXT,A_t,CON,OP)\
  instance (CXT) => CLASS (A_t) where\
    {METHOD\
      (CON a1 b1 c1)\
      (CON a2 b2 c2)\
      | !a <- OP a1 a2\
      , !b <- OP b1 b2\
      , !c <- OP c1 c2\
      = CON a b c};
#define INSTANCE_BINOP_1x4(CLASS,METHOD,CXT,A_t,CON,OP)\
  instance (CXT) => CLASS (A_t) where\
    {METHOD\
      (CON a1 b1 c1 d1)\
      (CON a2 b2 c2 d2)\
      | !a <- OP a1 a2\
      , !b <- OP b1 b2\
      , !c <- OP c1 c2\
      , !d <- OP d1 d2\
      = CON a b c d};
#define INSTANCE_BINOP_1x5(CLASS,METHOD,CXT,A_t,CON,OP)\
  instance (CXT) => CLASS (A_t) where\
    {METHOD\
      (CON a1 b1 c1 d1 e1)\
      (CON a2 b2 c2 d2 e2)\
      | !a <- OP a1 a2\
      , !b <- OP b1 b2\
      , !c <- OP c1 c2\
      , !d <- OP d1 d2\
      , !e <- OP e1 e2\
      = CON a b c d e};
#define INSTANCE_BINOP_1x6(CLASS,METHOD,CXT,A_t,CON,OP)\
  instance (CXT) => CLASS (A_t) where\
    {METHOD\
      (CON a1 b1 c1 d1 e1 f1)\
      (CON a2 b2 c2 d2 e2 f2)\
      | !a <- OP a1 a2\
      , !b <- OP b1 b2\
      , !c <- OP c1 c2\
      , !d <- OP d1 d2\
      , !e <- OP e1 e2\
      , !f <- OP f1 f2\
      = CON a b c d e f};
#define INSTANCE_BINOP_1x7(CLASS,METHOD,CXT,A_t,CON,OP)\
  instance (CXT) => CLASS (A_t) where\
    {METHOD\
      (CON a1 b1 c1 d1 e1 f1 g1)\
      (CON a2 b2 c2 d2 e2 f2 g2)\
      | !a <- OP a1 a2\
      , !b <- OP b1 b2\
      , !c <- OP c1 c2\
      , !d <- OP d1 d2\
      , !e <- OP e1 e2\
      , !f <- OP f1 f2\
      , !g <- OP g1 g2\
      = CON a b c d e f g};
#define INSTANCE_BINOP_1x8(CLASS,METHOD,CXT,A_t,CON,OP)\
  instance (CXT) => CLASS (A_t) where\
    {METHOD\
      (CON a1 b1 c1 d1 e1 f1 g1 h1)\
      (CON a2 b2 c2 d2 e2 f2 g2 h2)\
      | !a <- OP a1 a2\
      , !b <- OP b1 b2\
      , !c <- OP c1 c2\
      , !d <- OP d1 d2\
      , !e <- OP e1 e2\
      , !f <- OP f1 f2\
      , !g <- OP g1 g2\
      , !h <- OP h1 h2\
      = CON a b c d e f g h};
#define INSTANCE_BINOP_1x9(CLASS,METHOD,CXT,A_t,CON,OP)\
  instance (CXT) => CLASS (A_t) where\
    {METHOD\
      (CON a1 b1 c1 d1 e1 f1 g1 h1 i1)\
      (CON a2 b2 c2 d2 e2 f2 g2 h2 i2)\
      | !a <- OP a1 a2\
      , !b <- OP b1 b2\
      , !c <- OP c1 c2\
      , !d <- OP d1 d2\
      , !e <- OP e1 e2\
      , !f <- OP f1 f2\
      , !g <- OP g1 g2\
      , !h <- OP h1 h2\
      , !i <- OP i1 i2\
      = CON a b c d e f g h i};
#define INSTANCE_BINOP_1x10(CLASS,METHOD,CXT,A_t,CON,OP)\
  instance (CXT) => CLASS (A_t) where\
    {METHOD\
      (CON a1 b1 c1 d1 e1 f1 g1 h1 i1 j1)\
      (CON a2 b2 c2 d2 e2 f2 g2 h2 i2 j2)\
      | !a <- OP a1 a2\
      , !b <- OP b1 b2\
      , !c <- OP c1 c2\
      , !d <- OP d1 d2\
      , !e <- OP e1 e2\
      , !f <- OP f1 f2\
      , !g <- OP g1 g2\
      , !h <- OP h1 h2\
      , !i <- OP i1 i2\
      , !j <- OP j1 j2\
      = CON a b c d e f g h i j};
#define INSTANCE_BINOP_1x11(CLASS,METHOD,CXT,A_t,CON,OP)\
  instance (CXT) => CLASS (A_t) where\
    {METHOD\
      (CON a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1)\
      (CON a2 b2 c2 d2 e2 f2 g2 h2 i2 j2 k2)\
      | !a <- OP a1 a2\
      , !b <- OP b1 b2\
      , !c <- OP c1 c2\
      , !d <- OP d1 d2\
      , !e <- OP e1 e2\
      , !f <- OP f1 f2\
      , !g <- OP g1 g2\
      , !h <- OP h1 h2\
      , !i <- OP i1 i2\
      , !j <- OP j1 j2\
      , !k <- OP k1 k2\
      = CON a b c d e f g h i j k};
#define INSTANCE_BINOP_1x12(CLASS,METHOD,CXT,A_t,CON,OP)\
  instance (CXT) => CLASS (A_t) where\
    {METHOD\
      (CON a1 b1 c1 d1 e1 f1 g1 h1 i1 j1 k1 l1)\
      (CON a2 b2 c2 d2 e2 f2 g2 h2 i2 j2 k2 l2)\
      | !a <- OP a1 a2\
      , !b <- OP b1 b2\
      , !c <- OP c1 c2\
      , !d <- OP d1 d2\
      , !e <- OP e1 e2\
      , !f <- OP f1 f2\
      , !g <- OP g1 g2\
      , !h <- OP h1 h2\
      , !i <- OP i1 i2\
      , !j <- OP j1 j2\
      , !k <- OP k1 k2\
      , !l <- OP l1 l2\
      = CON a b c d e f g h i j k l};
// }}}

// {{{
#define INSTANCE_Join_1x0(CXT,A_t,CON,OP) INSTANCE_BINOP_1x0(Join,(\/),CXT,A_t,CON,OP)
#define INSTANCE_Join_1x1(CXT,A_t,CON,OP) INSTANCE_BINOP_1x1(Join,(\/),CXT,A_t,CON,OP)
#define INSTANCE_Join_1x2(CXT,A_t,CON,OP) INSTANCE_BINOP_1x2(Join,(\/),CXT,A_t,CON,OP)
#define INSTANCE_Join_1x3(CXT,A_t,CON,OP) INSTANCE_BINOP_1x3(Join,(\/),CXT,A_t,CON,OP)
#define INSTANCE_Join_1x4(CXT,A_t,CON,OP) INSTANCE_BINOP_1x4(Join,(\/),CXT,A_t,CON,OP)
#define INSTANCE_Join_1x5(CXT,A_t,CON,OP) INSTANCE_BINOP_1x5(Join,(\/),CXT,A_t,CON,OP)
#define INSTANCE_Join_1x6(CXT,A_t,CON,OP) INSTANCE_BINOP_1x6(Join,(\/),CXT,A_t,CON,OP)
#define INSTANCE_Join_1x7(CXT,A_t,CON,OP) INSTANCE_BINOP_1x7(Join,(\/),CXT,A_t,CON,OP)
#define INSTANCE_Join_1x8(CXT,A_t,CON,OP) INSTANCE_BINOP_1x8(Join,(\/),CXT,A_t,CON,OP)
#define INSTANCE_Join_1x9(CXT,A_t,CON,OP) INSTANCE_BINOP_1x9(Join,(\/),CXT,A_t,CON,OP)
#define INSTANCE_Join_1x10(CXT,A_t,CON,OP) INSTANCE_BINOP_1x10(Join,(\/),CXT,A_t,CON,OP)
#define INSTANCE_Join_1x11(CXT,A_t,CON,OP) INSTANCE_BINOP_1x11(Join,(\/),CXT,A_t,CON,OP)
#define INSTANCE_Join_1x12(CXT,A_t,CON,OP) INSTANCE_BINOP_1x12(Join,(\/),CXT,A_t,CON,OP)
#define INSTANCE_Join_1x13(CXT,A_t,CON,OP) INSTANCE_BINOP_1x13(Join,(\/),CXT,A_t,CON,OP)
#define INSTANCE_Join_1x14(CXT,A_t,CON,OP) INSTANCE_BINOP_1x14(Join,(\/),CXT,A_t,CON,OP)
#define INSTANCE_Join_1x15(CXT,A_t,CON,OP) INSTANCE_BINOP_1x15(Join,(\/),CXT,A_t,CON,OP)
#define INSTANCE_Join_1x16(CXT,A_t,CON,OP) INSTANCE_BINOP_1x16(Join,(\/),CXT,A_t,CON,OP)
#define INSTANCE_Join_1x17(CXT,A_t,CON,OP) INSTANCE_BINOP_1x17(Join,(\/),CXT,A_t,CON,OP)
#define INSTANCE_Join_1x18(CXT,A_t,CON,OP) INSTANCE_BINOP_1x18(Join,(\/),CXT,A_t,CON,OP)
#define INSTANCE_Join_1x19(CXT,A_t,CON,OP) INSTANCE_BINOP_1x19(Join,(\/),CXT,A_t,CON,OP)
#define INSTANCE_Join_1x20(CXT,A_t,CON,OP) INSTANCE_BINOP_1x20(Join,(\/),CXT,A_t,CON,OP)

#define INSTANCE_Meet_1x0(CXT,A_t,CON,OP) INSTANCE_BINOP_1x0(Meet,(/\),CXT,A_t,CON,OP)
#define INSTANCE_Meet_1x1(CXT,A_t,CON,OP) INSTANCE_BINOP_1x1(Meet,(/\),CXT,A_t,CON,OP)
#define INSTANCE_Meet_1x2(CXT,A_t,CON,OP) INSTANCE_BINOP_1x2(Meet,(/\),CXT,A_t,CON,OP)
#define INSTANCE_Meet_1x3(CXT,A_t,CON,OP) INSTANCE_BINOP_1x3(Meet,(/\),CXT,A_t,CON,OP)
#define INSTANCE_Meet_1x4(CXT,A_t,CON,OP) INSTANCE_BINOP_1x4(Meet,(/\),CXT,A_t,CON,OP)
#define INSTANCE_Meet_1x5(CXT,A_t,CON,OP) INSTANCE_BINOP_1x5(Meet,(/\),CXT,A_t,CON,OP)
#define INSTANCE_Meet_1x6(CXT,A_t,CON,OP) INSTANCE_BINOP_1x6(Meet,(/\),CXT,A_t,CON,OP)
#define INSTANCE_Meet_1x7(CXT,A_t,CON,OP) INSTANCE_BINOP_1x7(Meet,(/\),CXT,A_t,CON,OP)
#define INSTANCE_Meet_1x8(CXT,A_t,CON,OP) INSTANCE_BINOP_1x8(Meet,(/\),CXT,A_t,CON,OP)
#define INSTANCE_Meet_1x9(CXT,A_t,CON,OP) INSTANCE_BINOP_1x9(Meet,(/\),CXT,A_t,CON,OP)
#define INSTANCE_Meet_1x10(CXT,A_t,CON,OP) INSTANCE_BINOP_1x10(Meet,(/\),CXT,A_t,CON,OP)
#define INSTANCE_Meet_1x11(CXT,A_t,CON,OP) INSTANCE_BINOP_1x11(Meet,(/\),CXT,A_t,CON,OP)
#define INSTANCE_Meet_1x12(CXT,A_t,CON,OP) INSTANCE_BINOP_1x12(Meet,(/\),CXT,A_t,CON,OP)
#define INSTANCE_Meet_1x13(CXT,A_t,CON,OP) INSTANCE_BINOP_1x13(Meet,(/\),CXT,A_t,CON,OP)
#define INSTANCE_Meet_1x14(CXT,A_t,CON,OP) INSTANCE_BINOP_1x14(Meet,(/\),CXT,A_t,CON,OP)
#define INSTANCE_Meet_1x15(CXT,A_t,CON,OP) INSTANCE_BINOP_1x15(Meet,(/\),CXT,A_t,CON,OP)
#define INSTANCE_Meet_1x16(CXT,A_t,CON,OP) INSTANCE_BINOP_1x16(Meet,(/\),CXT,A_t,CON,OP)
#define INSTANCE_Meet_1x17(CXT,A_t,CON,OP) INSTANCE_BINOP_1x17(Meet,(/\),CXT,A_t,CON,OP)
#define INSTANCE_Meet_1x18(CXT,A_t,CON,OP) INSTANCE_BINOP_1x18(Meet,(/\),CXT,A_t,CON,OP)
#define INSTANCE_Meet_1x19(CXT,A_t,CON,OP) INSTANCE_BINOP_1x19(Meet,(/\),CXT,A_t,CON,OP)
#define INSTANCE_Meet_1x20(CXT,A_t,CON,OP) INSTANCE_BINOP_1x20(Meet,(/\),CXT,A_t,CON,OP)

#define INSTANCE_Diff_1x0(CXT,A_t,CON,OP) INSTANCE_BINOP_1x0(Diff,(\\),CXT,A_t,CON,OP)
#define INSTANCE_Diff_1x1(CXT,A_t,CON,OP) INSTANCE_BINOP_1x1(Diff,(\\),CXT,A_t,CON,OP)
#define INSTANCE_Diff_1x2(CXT,A_t,CON,OP) INSTANCE_BINOP_1x2(Diff,(\\),CXT,A_t,CON,OP)
#define INSTANCE_Diff_1x3(CXT,A_t,CON,OP) INSTANCE_BINOP_1x3(Diff,(\\),CXT,A_t,CON,OP)
#define INSTANCE_Diff_1x4(CXT,A_t,CON,OP) INSTANCE_BINOP_1x4(Diff,(\\),CXT,A_t,CON,OP)
#define INSTANCE_Diff_1x5(CXT,A_t,CON,OP) INSTANCE_BINOP_1x5(Diff,(\\),CXT,A_t,CON,OP)
#define INSTANCE_Diff_1x6(CXT,A_t,CON,OP) INSTANCE_BINOP_1x6(Diff,(\\),CXT,A_t,CON,OP)
#define INSTANCE_Diff_1x7(CXT,A_t,CON,OP) INSTANCE_BINOP_1x7(Diff,(\\),CXT,A_t,CON,OP)
#define INSTANCE_Diff_1x8(CXT,A_t,CON,OP) INSTANCE_BINOP_1x8(Diff,(\\),CXT,A_t,CON,OP)
#define INSTANCE_Diff_1x9(CXT,A_t,CON,OP) INSTANCE_BINOP_1x9(Diff,(\\),CXT,A_t,CON,OP)
#define INSTANCE_Diff_1x10(CXT,A_t,CON,OP) INSTANCE_BINOP_1x10(Diff,(\\),CXT,A_t,CON,OP)
#define INSTANCE_Diff_1x11(CXT,A_t,CON,OP) INSTANCE_BINOP_1x11(Diff,(\\),CXT,A_t,CON,OP)
#define INSTANCE_Diff_1x12(CXT,A_t,CON,OP) INSTANCE_BINOP_1x12(Diff,(\\),CXT,A_t,CON,OP)
#define INSTANCE_Diff_1x13(CXT,A_t,CON,OP) INSTANCE_BINOP_1x13(Diff,(\\),CXT,A_t,CON,OP)
#define INSTANCE_Diff_1x14(CXT,A_t,CON,OP) INSTANCE_BINOP_1x14(Diff,(\\),CXT,A_t,CON,OP)
#define INSTANCE_Diff_1x15(CXT,A_t,CON,OP) INSTANCE_BINOP_1x15(Diff,(\\),CXT,A_t,CON,OP)
#define INSTANCE_Diff_1x16(CXT,A_t,CON,OP) INSTANCE_BINOP_1x16(Diff,(\\),CXT,A_t,CON,OP)
#define INSTANCE_Diff_1x17(CXT,A_t,CON,OP) INSTANCE_BINOP_1x17(Diff,(\\),CXT,A_t,CON,OP)
#define INSTANCE_Diff_1x18(CXT,A_t,CON,OP) INSTANCE_BINOP_1x18(Diff,(\\),CXT,A_t,CON,OP)
#define INSTANCE_Diff_1x19(CXT,A_t,CON,OP) INSTANCE_BINOP_1x19(Diff,(\\),CXT,A_t,CON,OP)
#define INSTANCE_Diff_1x20(CXT,A_t,CON,OP) INSTANCE_BINOP_1x20(Diff,(\\),CXT,A_t,CON,OP)
// }}}

// {{{
#define INSTANCE_UNIT_1x0(CLASS,METHOD,A_t,CON,IS,VALUE) instance CLASS (A_t) where {is##CLASS = IS;METHOD = CON};
#define INSTANCE_UNIT_1x1(CLASS,METHOD,A_t,CON,IS,VALUE) instance CLASS (A_t) where {is##CLASS = IS;METHOD = CON VALUE};
#define INSTANCE_UNIT_1x2(CLASS,METHOD,A_t,CON,IS,VALUE) instance CLASS (A_t) where {is##CLASS = IS;METHOD = CON VALUE VALUE};
#define INSTANCE_UNIT_1x3(CLASS,METHOD,A_t,CON,IS,VALUE) instance CLASS (A_t) where {is##CLASS = IS;METHOD = CON VALUE VALUE VALUE};
#define INSTANCE_UNIT_1x4(CLASS,METHOD,A_t,CON,IS,VALUE) instance CLASS (A_t) where {is##CLASS = IS;METHOD = CON VALUE VALUE VALUE VALUE};
#define INSTANCE_UNIT_1x5(CLASS,METHOD,A_t,CON,IS,VALUE) instance CLASS (A_t) where {is##CLASS = IS;METHOD = CON VALUE VALUE VALUE VALUE VALUE};
#define INSTANCE_UNIT_1x6(CLASS,METHOD,A_t,CON,IS,VALUE) instance CLASS (A_t) where {is##CLASS = IS;METHOD = CON VALUE VALUE VALUE VALUE VALUE VALUE};
#define INSTANCE_UNIT_1x7(CLASS,METHOD,A_t,CON,IS,VALUE) instance CLASS (A_t) where {is##CLASS = IS;METHOD = CON VALUE VALUE VALUE VALUE VALUE VALUE VALUE};
#define INSTANCE_UNIT_1x8(CLASS,METHOD,A_t,CON,IS,VALUE) instance CLASS (A_t) where {is##CLASS = IS;METHOD = CON VALUE VALUE VALUE VALUE VALUE VALUE VALUE VALUE};
#define INSTANCE_UNIT_1x9(CLASS,METHOD,A_t,CON,IS,VALUE) instance CLASS (A_t) where {is##CLASS = IS;METHOD = CON VALUE VALUE VALUE VALUE VALUE VALUE VALUE VALUE VALUE};
#define INSTANCE_UNIT_1x10(CLASS,METHOD,A_t,CON,IS,VALUE) instance CLASS (A_t) where {is##CLASS = IS;METHOD = CON VALUE VALUE VALUE VALUE VALUE VALUE VALUE VALUE VALUE VALUE};
#define INSTANCE_UNIT_1x11(CLASS,METHOD,A_t,CON,IS,VALUE) instance CLASS (A_t) where {is##CLASS = IS;METHOD = CON VALUE VALUE VALUE VALUE VALUE VALUE VALUE VALUE VALUE VALUE VALUE};
#define INSTANCE_UNIT_1x12(CLASS,METHOD,A_t,CON,IS,VALUE) instance CLASS (A_t) where {is##CLASS = IS;METHOD = CON VALUE VALUE VALUE VALUE VALUE VALUE VALUE VALUE VALUE VALUE VALUE VALUE};
#define INSTANCE_UNIT_1x13(CLASS,METHOD,A_t,CON,IS,VALUE) instance CLASS (A_t) where {is##CLASS = IS;METHOD = CON VALUE VALUE VALUE VALUE VALUE VALUE VALUE VALUE VALUE VALUE VALUE VALUE VALUE};
#define INSTANCE_UNIT_1x14(CLASS,METHOD,A_t,CON,IS,VALUE) instance CLASS (A_t) where {is##CLASS = IS;METHOD = CON VALUE VALUE VALUE VALUE VALUE VALUE VALUE VALUE VALUE VALUE VALUE VALUE VALUE VALUE};
#define INSTANCE_UNIT_1x15(CLASS,METHOD,A_t,CON,IS,VALUE) instance CLASS (A_t) where {is##CLASS = IS;METHOD = CON VALUE VALUE VALUE VALUE VALUE VALUE VALUE VALUE VALUE VALUE VALUE VALUE VALUE VALUE VALUE};
#define INSTANCE_UNIT_1x16(CLASS,METHOD,A_t,CON,IS,VALUE) instance CLASS (A_t) where {is##CLASS = IS;METHOD = CON VALUE VALUE VALUE VALUE VALUE VALUE VALUE VALUE VALUE VALUE VALUE VALUE VALUE VALUE VALUE VALUE};
#define INSTANCE_UNIT_1x17(CLASS,METHOD,A_t,CON,IS,VALUE) instance CLASS (A_t) where {is##CLASS = IS;METHOD = CON VALUE VALUE VALUE VALUE VALUE VALUE VALUE VALUE VALUE VALUE VALUE VALUE VALUE VALUE VALUE VALUE VALUE};
#define INSTANCE_UNIT_1x18(CLASS,METHOD,A_t,CON,IS,VALUE) instance CLASS (A_t) where {is##CLASS = IS;METHOD = CON VALUE VALUE VALUE VALUE VALUE VALUE VALUE VALUE VALUE VALUE VALUE VALUE VALUE VALUE VALUE VALUE VALUE VALUE};
#define INSTANCE_UNIT_1x19(CLASS,METHOD,A_t,CON,IS,VALUE) instance CLASS (A_t) where {is##CLASS = IS;METHOD = CON VALUE VALUE VALUE VALUE VALUE VALUE VALUE VALUE VALUE VALUE VALUE VALUE VALUE VALUE VALUE VALUE VALUE VALUE VALUE};
#define INSTANCE_UNIT_1x20(CLASS,METHOD,A_t,CON,IS,VALUE) instance CLASS (A_t) where {is##CLASS = IS;METHOD = CON VALUE VALUE VALUE VALUE VALUE VALUE VALUE VALUE VALUE VALUE VALUE VALUE VALUE VALUE VALUE VALUE VALUE VALUE VALUE VALUE};
#define INSTANCE_UNIT_1x21(CLASS,METHOD,A_t,CON,IS,VALUE) instance CLASS (A_t) where {is##CLASS = IS;METHOD = CON VALUE VALUE VALUE VALUE VALUE VALUE VALUE VALUE VALUE VALUE VALUE VALUE VALUE VALUE VALUE VALUE VALUE VALUE VALUE VALUE VALUE};
// }}}

/***************************************************************************/

#define INSTANCE_GetSetLook_IxMap(M_t,A_t,B_t,CON,FIELD)\
instance M.GetSetLook (M_t) (Ix (A_t)) (B_t) where {\
  get CON{..} i = FIELD Ix.! i;\
  set CON{..} i x | ! FIELD <- Ix.insert i x FIELD = CON{..};\
  look CON{..} i = Ix.lookup i FIELD;\
  __INLINE(get)\
  __INLINE(set)\
  __INLINE(look)\
};

#define INSTANCE_GetSetLook_M(M_t,A_t,B_t,CON,FIELD)\
instance M.GetSetLook (M_t) (Ix (A_t)) (B_t) where {\
  get CON{..} i = M.get FIELD i;\
  set CON{..} i x | ! FIELD <- M.set FIELD i x = CON{..};\
  look CON{..} i = M.look FIELD i;\
  __INLINE(get)\
  __INLINE(set)\
  __INLINE(look)\
};

/***************************************************************************/

#define INSTANCE_UF(UF,UF_t,A_t,B_t,CON,FIELD)\
instance U.UF UF_t (A_t) (B_t) where {\
  find CON{..} i\
    | HS_UTUP3(i,t,FIELD) <- UF(find) FIELD i\
    = HS_UTUP3(i,t,CON{..});\
  rep CON{..} i\
    | HS_UTUP(i,FIELD) <- UF(rep) FIELD i\
    = HS_UTUP(i,CON{..});\
  tryRep CON{..} i\
    | HS_UTUP(i,FIELD) <- UF(tryRep) FIELD i\
    = HS_UTUP(i,CON{..});\
  get CON{..} i\
    | HS_UTUP(t,FIELD) <- UF(get) FIELD i\
    = HS_UTUP(t,CON{..});\
  ind CON{..} i j\
    | FIELD <- UF(ind) FIELD i j\
    = CON{..};\
  keys CON{..}\
    = UF(keys) FIELD;\
  roots CON{..}\
    = UF(roots) FIELD;\
  lookup CON{..} i\
    | Just (b,! FIELD) <- UF(lookup) FIELD i\
    = Just (b,CON{..})\
    | otherwise\
    = Nothing;\
  insert CON{..} i x\
    | ! FIELD <- UF(insert) FIELD i x\
    = CON{..};\
  unsafeInsert CON{..} i x\
    | ! FIELD <- UF(unsafeInsert) FIELD i x\
    = CON{..};\
  insertWith f CON{..} i x\
    | ! FIELD <- UF(insertWith) f FIELD i x\
    = CON{..};\
  insertWithRep f CON{..} i x\
    | ! FIELD <- UF(insertWithRep) f FIELD i x\
    = CON{..};\
  insertWithM f CON{..} i x\
    = do {! FIELD <- UF(insertWithM) f FIELD i x; return CON{..}};\
  insertWithRepM f CON{..} i x\
    = do {! FIELD <- UF(insertWithRepM) f FIELD i x; return CON{..}};\
  update f CON{..} i\
    | ! FIELD <- UF(update) f FIELD i\
    = CON{..};\
  updateWithRep f CON{..} i\
    | ! FIELD <- UF(updateWithRep) f FIELD i\
    = CON{..};\
  updateM f CON{..} i\
    = do {! FIELD <- UF(updateM) f FIELD i; return CON{..}};\
  updateWithRepM f CON{..} i\
    = do {! FIELD <- UF(updateWithRepM) f FIELD i; return CON{..}};\
  fold f a CON{..}\
    | (!a,! FIELD) <- UF(fold) f a FIELD\
    = (a,CON{..});\
  foldM f a CON{..}\
    = do {(!a,! FIELD) <- UF(foldM) f a FIELD; return (a,CON{..})};\
  union CON{..} i j\
    | HS_UTUP(new,! FIELD) <- UF(union) FIELD i j\
    = HS_UTUP(new,CON{..});\
  unionToLeft CON{..} i j\
    | HS_UTUP(new,! FIELD) <- UF(unionToLeft) FIELD i j\
    = HS_UTUP(new,CON{..});\
  unionAndSet CON{..} i j a\
    | HS_UTUP(new,! FIELD) <- UF(unionAndSet) FIELD i j a\
    = HS_UTUP(new,CON{..});\
  unionToLeftAndSet CON{..} i j a\
    | HS_UTUP(new,! FIELD) <- UF(unionToLeftAndSet) FIELD i j a\
    = HS_UTUP(new,CON{..});\
  unionWith f CON{..} i j\
    | HS_UTUP(new,! FIELD) <- UF(unionWith) f FIELD i j\
    = HS_UTUP(new,CON{..});\
  unionWithRep f CON{..} i j\
    | HS_UTUP(new,! FIELD) <- UF(unionWithRep) f FIELD i j\
    = HS_UTUP(new,CON{..});\
  unionToLeftWith f CON{..} i j\
    | HS_UTUP(new,! FIELD) <- UF(unionToLeftWith) f FIELD i j\
    = HS_UTUP(new,CON{..});\
  unionToLeftWithRep f CON{..} i j\
    | HS_UTUP(new,! FIELD) <- UF(unionToLeftWithRep) f FIELD i j\
    = HS_UTUP(new,CON{..});\
  unionAndSetWithRep CON{..} i j f\
    | HS_UTUP(new,! FIELD) <- UF(unionAndSetWithRep) FIELD i j f\
    = HS_UTUP(new,CON{..});\
  unionToLeftAndSetWithRep CON{..} i j f\
    | HS_UTUP(new,! FIELD) <- UF(unionToLeftAndSetWithRep) FIELD i j f\
    = HS_UTUP(new,CON{..});\
  union_ CON{..} i j\
    | ! FIELD <- UF(union_) FIELD i j\
    = CON{..};\
  unionToLeft_ CON{..} i j\
    | ! FIELD <- UF(unionToLeft_) FIELD i j\
    = CON{..};\
  unionAndSet_ CON{..} i j a\
    | ! FIELD <- UF(unionAndSet_) FIELD i j a\
    = CON{..};\
  unionToLeftAndSet_ CON{..} i j a\
    | ! FIELD <- UF(unionToLeftAndSet_) FIELD i j a\
    = CON{..};\
  unionWith_ f CON{..} i j\
    | ! FIELD <- UF(unionWith_) f FIELD i j\
    = CON{..};\
  unionWithRep_ f CON{..} i j\
    | ! FIELD <- UF(unionWithRep_) f FIELD i j\
    = CON{..};\
  unionToLeftWith_ f CON{..} i j\
    | ! FIELD <- UF(unionToLeftWith_) f FIELD i j\
    = CON{..};\
  unionToLeftWithRep_ f CON{..} i j\
    | ! FIELD <- UF(unionToLeftWithRep_) f FIELD i j\
    = CON{..};\
  unionAndSetWithRep_ CON{..} i j f\
    | ! FIELD <- UF(unionAndSetWithRep_) FIELD i j f\
    = CON{..};\
  unionToLeftAndSetWithRep_ CON{..} i j f\
    | ! FIELD <- UF(unionToLeftAndSetWithRep_) FIELD i j f\
    = CON{..};\
  support CON{..}\
    | (!ix,! FIELD) <- UF(support) FIELD\
    = (ix,CON{..});\
  collect CON{..}\
    | (!ix,! FIELD) <- UF(collect) FIELD\
    = (ix,CON{..});\
  quotient CON{..}\
    = UF(quotient) FIELD;\
  quotient_ CON{..}\
    = UF(quotient_) FIELD;\
  compressAndIndex CON{..}\
    | (!ix,! FIELD) <- UF(compressAndIndex) FIELD\
    = (ix,CON{..});\
  supportAndSize CON{..}\
    | (!n,!ix,! FIELD) <- UF(supportAndSize) FIELD\
    = (n,ix,CON{..});\
  collectAndReindexFrom n CON{..}\
    | (!n,!ix,! FIELD) <- UF(collectAndReindexFrom) n FIELD\
    = (n,ix,CON{..});\
  quotientAndReindexFrom n CON{..}\
    = UF(quotientAndReindexFrom) n FIELD;\
  getInds CON{..} i\
    = U.getInds FIELD i;\
  setInds CON{..} i s\
    | ! FIELD <- U.setInds FIELD i s\
    = CON{..};\
  mapInds f CON{..} i\
    | ! FIELD <- U.mapInds f FIELD i\
    = CON{..};\
  getAndClearInds CON{..} i\
    | HS_UTUP(s,! FIELD) <- U.getAndClearInds FIELD i\
    = HS_UTUP(s,CON{..});\
  popDirty CON{..}\
    | HS_UTUP(o,! FIELD) <- U.popDirty FIELD\
    = HS_UTUP(o,CON{..});\
  __INLINE(find)\
  __INLINE(rep)\
  __INLINE(tryRep)\
  __INLINE(get)\
  __INLINE(ind)\
  __INLINE(keys)\
  __INLINE(roots)\
  __INLINE(lookup)\
  __INLINE(insert)\
  __INLINE(unsafeInsert)\
  __INLINE(insertWith)\
  __INLINE(insertWithRep)\
  __INLINE(insertWithM)\
  __INLINE(insertWithRepM)\
  __INLINE(update)\
  __INLINE(updateWithRep)\
  __INLINE(updateM)\
  __INLINE(updateWithRepM)\
  __INLINE(fold)\
  __INLINE(foldM)\
  __INLINE(union)\
  __INLINE(unionToLeft)\
  __INLINE(unionAndSet)\
  __INLINE(unionToLeftAndSet)\
  __INLINE(unionWith)\
  __INLINE(unionWithRep)\
  __INLINE(unionToLeftWith)\
  __INLINE(unionToLeftWithRep)\
  __INLINE(unionAndSetWithRep)\
  __INLINE(unionToLeftAndSetWithRep)\
  __INLINE(union_)\
  __INLINE(unionToLeft_)\
  __INLINE(unionAndSet_)\
  __INLINE(unionToLeftAndSet_)\
  __INLINE(unionWith_)\
  __INLINE(unionWithRep_)\
  __INLINE(unionToLeftWith_)\
  __INLINE(unionToLeftWithRep_)\
  __INLINE(unionAndSetWithRep_)\
  __INLINE(unionToLeftAndSetWithRep_)\
  __INLINE(support)\
  __INLINE(collect)\
  __INLINE(quotient)\
  __INLINE(compressAndIndex)\
  __INLINE(supportAndSize)\
  __INLINE(collectAndReindexFrom)\
  __INLINE(quotientAndReindexFrom)\
  __INLINE(getInds)\
  __INLINE(setInds)\
  __INLINE(mapInds)\
  __INLINE(getAndClearInds)\
  __INLINE(popDirty)\
};

/***************************************************************************/

#if 0
* PARAMS
    1) M_t
    2) A_t
    3) B_t
    4) GET
    5) SET(x)
* ASSUMES
    a) instance NewM (M_t) (A_t)
    b) M_t :: * -> *
    c) A_t,B_t :: *
    d) UF(x) ==> (e.g.) UF.x
    e) GET :: M_t X
    f) SET :: X -> M_t ()
#endif
#define INSTANCE_GetSetAddM_UF(UF,M_t,A_t,B_t,GET,SET)\
INSTANCE_GetM_UF(UF,M_t,A_t,B_t,GET,SET);\
INSTANCE_SetM_UF(UF,M_t,A_t,B_t,GET,SET);\
INSTANCE_AddM_UF(UF,M_t,A_t,B_t,GET,SET);\
instance U.GetSetM (M_t) (A_t) (B_t);\
instance U.GetSetAddM (M_t) (A_t) (B_t);

#define INSTANCE_GetM_UF(UF,M_t,A_t,B_t,GET,SET)\
instance U.GetM (M_t) (A_t) (B_t) where {\
  __INLINE(getM)\
  getM i = do {\
    uf <- GET;\
    case UF(get) uf i of\
      HS_UTUP( i,uf )-> do {\
        SET(uf);\
        return i}};\
};

#define INSTANCE_SetM_UF(UF,M_t,A_t,B_t,GET,SET)\
instance U.SetM (M_t) (A_t) (B_t) where {\
  __INLINE(setM)\
  __INLINE(unsafeSetM)\
  setM i j = do {\
    uf <- GET;\
    let {!uf2 = UF(insert) uf i j};\
    SET(uf2)};\
  unsafeSetM i j = do {\
    uf <- GET;\
    let {!uf2 = UF(unsafeInsert) uf i j};\
    SET(uf2)};\
};

#define INSTANCE_AddM_UF(UF,M_t,A_t,B_t,GET,SET)\
instance U.AddM (M_t) (A_t) (B_t) where {\
  __INLINE(addM)\
  addM a = do {\
    i <- U.newM;\
    uf <- GET;\
    let {!uf2 = UF(unsafeInsert) uf i a};\
    SET(uf2);\
    return i};\
};

/***************************************************************************/

#if 0
* PARAMS
    1) M_t
    2) A_t
    3) B_t
    4) GET
    5) SET(x)
* ASSUMES
    a) instance NewM (M_t) (A_t)
    b) M_t :: * -> *
    c) A_t,B_t :: *
    d) instance GetSetLook X Elt_t Elt_t
    e) GET :: M_t X
    f) SET :: X -> M_t ()
#endif
#define INSTANCE_GetSetAddM_M(M_t,A_t,B_t,GET,SET)\
INSTANCE_GetM_M(M_t,A_t,B_t,GET,SET);\
INSTANCE_SetM_M(M_t,A_t,B_t,GET,SET);\
INSTANCE_AddM_M(M_t,A_t,B_t,GET,SET);\
instance U.GetSetM (M_t) (A_t) (B_t);\
instance U.GetSetAddM (M_t) (A_t) (B_t);

#define INSTANCE_GetM_M(M_t,A_t,B_t,GET,SET)\
instance U.GetM (M_t) (A_t) (B_t) where {\
  __INLINE(getM)\
  getM i = do {\
    ix <- GET;\
    let {!o = M.get ix i};\
    return o};\
};

#define INSTANCE_SetM_M(M_t,A_t,B_t,GET,SET)\
instance U.SetM (M_t) (A_t) (B_t) where {\
  __INLINE(setM)\
  setM i o = do {\
    ix <- GET;\
    let {!ix2 = M.set ix i o};\
    SET(ix2)};\
};

#define INSTANCE_AddM_M(M_t,A_t,B_t,GET,SET)\
instance U.AddM (M_t) (A_t) (B_t) where {\
  __INLINE(addM)\
  addM o = do {\
    i <- U.newM;\
    ix <- GET;\
    let {!ix2 = M.set ix i o};\
    SET(ix2);\
    return i};\
};

/***************************************************************************/

#if 0
* PARAMS
    1) M_t
    2) A_t
    3) B_t
    4) GET
    5) SET(x)
* ASSUMES
    a) instance UniqM (M_t)
    b) M_t :: * -> *
    c) A_t,B_t :: *
    d) GET :: M_t (UF.UF A_t B_t)
    e) SET :: UF.UF A_t B_t -> M_t ()
#endif
#define INSTANCE_RepM(UF,M_t,A_t,GET,SET)\
instance RepM (M_t) (A_t) where {\
  repM i = do\
    {uf <- GET\
    ;case UF(rep) uf i of\
      HS_UTUP( i,uf )-> do\
        {SET(uf)\
        ;return i}};\
  tryRepM i = do\
    {uf <- GET\
    ;case UF(tryRep) uf i of\
      HS_UTUP( i,uf )-> do\
        {SET(uf)\
        ;return i}};\
  __INLINE(repM);\
  __INLINE(tryRepM);\
};

#define INSTANCE_UFM(UF,M_t,A_t,B_t,GET,SET)\
INSTANCE_RepM(UF,M_t,A_t,GET,SET);\
INSTANCE_GetSetAddM_UF(UF,M_t,A_t,B_t,GET,SET);\
instance UFM (M_t) (A_t) (B_t) where {\
  findM i = do\
    {uf <- GET; case UF(find) uf i of HS_UTUP3( i,a,uf )-> do\
    {SET(uf); return (i,a)}};\
  indM i j = do\
    {uf <- GET; let {!uf2 = UF(ind) uf i j}; SET(uf2)};\
  unionM i j = do\
    {uf <- GET; case UF(union) uf i j of HS_UTUP( new,uf )-> do\
    {SET(uf); return new}};\
  unionAndSetM i j a = do\
    {uf <- GET; case UF(unionAndSet) uf i j a of HS_UTUP( new,uf )-> do\
    {SET(uf); return new}};\
  unionToLeftM i j = do\
    {uf <- GET; case UF(unionToLeft) uf i j of HS_UTUP( new,uf )-> do\
    {SET(uf); return new}};\
  unionToLeftAndSetM i j a = do\
    {uf <- GET\
    ;case UF(unionToLeftAndSet) uf i j a of\
      HS_UTUP( new,uf )-> do\
        {SET(uf)\
        ;return new}};\
  unionWithM f i j = do\
    {uf <- GET; case UF(unionWith) f uf i j of HS_UTUP( new,uf )-> do\
    {SET(uf); return new}};\
  unionWithRepM f i j = do\
    {uf <- GET; case UF(unionWithRep) f uf i j of HS_UTUP( new,uf )-> do\
    {SET(uf); return new}};\
  unionToLeftWithM f i j = do\
    {uf <- GET; case UF(unionToLeftWith) f uf i j of HS_UTUP( new,uf )-> do\
    {SET(uf); return new}};\
  unionToLeftWithRepM f i j = do\
    {uf <- GET; case UF(unionToLeftWithRep) f uf i j of HS_UTUP( new,uf )-> do\
    {SET(uf); return new}};\
  unionM_ i j = do\
    {uf <- GET; case UF(union_) uf i j of uf-> do\
    {SET(uf)}};\
  unionAndSetM_ i j a = do\
    {uf <- GET; case UF(unionAndSet_) uf i j a of uf-> do\
    {SET(uf)}};\
  unionToLeftM_ i j = do\
    {uf <- GET; case UF(unionToLeft_) uf i j of uf-> do\
    {SET(uf)}};\
  unionToLeftAndSetM_ i j a = do\
    {uf <- GET; case UF(unionToLeftAndSet_) uf i j a of uf-> do\
    {SET(uf)}};\
  unionWithM_ f i j = do\
    {uf <- GET; case UF(unionWith_) f uf i j of uf-> do\
    {SET(uf)}};\
  unionWithRepM_ f i j = do\
    {uf <- GET; case UF(unionWithRep_) f uf i j of uf-> do\
    {SET(uf)}};\
  unionToLeftWithM_ f i j = do\
    {uf <- GET; case UF(unionToLeftWith_) f uf i j of uf-> do\
    {SET(uf)}};\
  unionToLeftWithRepM_ f i j = do\
    {uf <- GET; case UF(unionToLeftWithRep_) f uf i j of uf-> do\
    {SET(uf)}};\
  getIndsM i = do\
    {uf <- GET; case UF(getInds) uf i of s-> return s};\
  setIndsM i s = do\
    {uf <- GET; case UF(setInds) uf i s of uf-> do\
    {SET(uf)}};\
  mapIndsM f i = do\
    {uf <- GET; case UF(mapInds) f uf i of uf-> do\
    {SET(uf)}};\
  getAndClearIndsM i = do\
    {uf <- GET; case UF(getAndClearInds) uf i of HS_UTUP( s,uf )-> do\
    {SET(uf); return s}};\
  popDirtyM = do\
    {uf <- GET; case UF(popDirty) uf of HS_UTUP( o,uf )-> do\
    {SET(uf); return o}};\
  __INLINE(findM)\
  __INLINE(indM)\
  __INLINE(unionM)\
  __INLINE(unionAndSetM)\
  __INLINE(unionToLeftM)\
  __INLINE(unionToLeftAndSetM)\
  __INLINE(unionWithM)\
  __INLINE(unionWithRepM)\
  __INLINE(unionToLeftWithM)\
  __INLINE(unionToLeftWithRepM)\
  __INLINE(unionM_)\
  __INLINE(unionAndSetM_)\
  __INLINE(unionToLeftM_)\
  __INLINE(unionToLeftAndSetM_)\
  __INLINE(unionWithM_)\
  __INLINE(unionWithRepM_)\
  __INLINE(unionToLeftWithM_)\
  __INLINE(unionToLeftWithRepM_)\
  __INLINE(getIndsM)\
  __INLINE(setIndsM)\
  __INLINE(mapIndsM)\
  __INLINE(getAndClearIndsM)\
  __INLINE(popDirtyM)\
};

/***************************************************************************/

#if 0
#define INSTANCE_Bin_1x1(A_t,CON)\
  instance Bin (A_t) where\
    {put (CON x1) = do\
        {Bin.put x1}\
    ;get = do\
        {x1 <- Bin.get\
        ;return $! (CON x1)}}
#define INSTANCE_Bin_1x2(A_t,CON)\
  instance Bin (A_t) where\
    {put (CON x1 x2) = do\
        {Bin.put x1\
        ;Bin.put x2}\
    ;get = do\
        {x1 <- Bin.get\
        ;x2 <- Bin.get\
        ;return $! (CON x1 x2)}}
#define INSTANCE_Bin_1x3(A_t,CON)\
  instance Bin (A_t) where\
    {put (CON x1 x2 x3) = do\
        {Bin.put x1\
        ;Bin.put x2\
        ;Bin.put x3}\
    ;get = do\
        {x1 <- Bin.get\
        ;x2 <- Bin.get\
        ;x3 <- Bin.get\
        ;return $! (CON x1 x2 x3)}}
#define INSTANCE_Bin_1x4(A_t,CON)\
  instance Bin (A_t) where\
    {put (CON x1 x2 x3 x4) = do\
        {Bin.put x1\
        ;Bin.put x2\
        ;Bin.put x3\
        ;Bin.put x4}\
    ;get = do\
        {x1 <- Bin.get\
        ;x2 <- Bin.get\
        ;x3 <- Bin.get\
        ;x4 <- Bin.get\
        ;return $! (CON x1 x2 x3 x4)}}
#define INSTANCE_Bin_1x5(A_t,CON)\
  instance Bin (A_t) where\
    {put (CON x1 x2 x3 x4 x5) = do\
        {Bin.put x1\
        ;Bin.put x2\
        ;Bin.put x3\
        ;Bin.put x4\
        ;Bin.put x5}\
    ;get = do\
        {x1 <- Bin.get\
        ;x2 <- Bin.get\
        ;x3 <- Bin.get\
        ;x4 <- Bin.get\
        ;x5 <- Bin.get\
        ;return $! (CON x1 x2 x3 x4 x5)}}
#define INSTANCE_Bin_1x6(A_t,CON)\
  instance Bin (A_t) where\
    {put (CON x1 x2 x3 x4 x5 x6) = do\
        {Bin.put x1\
        ;Bin.put x2\
        ;Bin.put x3\
        ;Bin.put x4\
        ;Bin.put x5\
        ;Bin.put x6}\
    ;get = do\
        {x1 <- Bin.get\
        ;x2 <- Bin.get\
        ;x3 <- Bin.get\
        ;x4 <- Bin.get\
        ;x5 <- Bin.get\
        ;x6 <- Bin.get\
        ;return $! (CON x1 x2 x3 x4 x5 x6)}}
#define INSTANCE_Bin_1x7(A_t,CON)\
  instance Bin (A_t) where\
    {put (CON x1 x2 x3 x4 x5 x6 x7) = do\
        {Bin.put x1\
        ;Bin.put x2\
        ;Bin.put x3\
        ;Bin.put x4\
        ;Bin.put x5\
        ;Bin.put x6\
        ;Bin.put x7}\
    ;get = do\
        {x1 <- Bin.get\
        ;x2 <- Bin.get\
        ;x3 <- Bin.get\
        ;x4 <- Bin.get\
        ;x5 <- Bin.get\
        ;x6 <- Bin.get\
        ;x7 <- Bin.get\
        ;return $! (CON x1 x2 x3 x4 x5 x6 x7)}}
#define INSTANCE_Bin_1x8(A_t,CON)\
  instance Bin (A_t) where\
    {put (CON x1 x2 x3 x4 x5 x6 x7 x8) = do\
        {Bin.put x1\
        ;Bin.put x2\
        ;Bin.put x3\
        ;Bin.put x4\
        ;Bin.put x5\
        ;Bin.put x6\
        ;Bin.put x7\
        ;Bin.put x8}\
    ;get = do\
        {x1 <- Bin.get\
        ;x2 <- Bin.get\
        ;x3 <- Bin.get\
        ;x4 <- Bin.get\
        ;x5 <- Bin.get\
        ;x6 <- Bin.get\
        ;x7 <- Bin.get\
        ;x8 <- Bin.get\
        ;return $! (CON x1 x2 x3 x4 x5 x6 x7 x8)}}
#define INSTANCE_Bin_1x9(A_t,CON)\
  instance Bin (A_t) where\
    {put (CON x1 x2 x3 x4 x5 x6 x7 x8 x9) = do\
        {Bin.put x1\
        ;Bin.put x2\
        ;Bin.put x3\
        ;Bin.put x4\
        ;Bin.put x5\
        ;Bin.put x6\
        ;Bin.put x7\
        ;Bin.put x8\
        ;Bin.put x9}\
    ;get = do\
        {x1 <- Bin.get\
        ;x2 <- Bin.get\
        ;x3 <- Bin.get\
        ;x4 <- Bin.get\
        ;x5 <- Bin.get\
        ;x6 <- Bin.get\
        ;x7 <- Bin.get\
        ;x8 <- Bin.get\
        ;x9 <- Bin.get\
        ;return $! (CON x1 x2 x3 x4 x5 x6 x7 x8 x9)}}
#define INSTANCE_Bin_1x11(A_t,CON)\
  instance Bin (A_t) where\
    {put (CON x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11) = do\
        {Bin.put x1\
        ;Bin.put x2\
        ;Bin.put x3\
        ;Bin.put x4\
        ;Bin.put x5\
        ;Bin.put x6\
        ;Bin.put x7\
        ;Bin.put x8\
        ;Bin.put x9\
        ;Bin.put x10\
        ;Bin.put x11}\
    ;get = do\
        {x1 <- Bin.get\
        ;x2 <- Bin.get\
        ;x3 <- Bin.get\
        ;x4 <- Bin.get\
        ;x5 <- Bin.get\
        ;x6 <- Bin.get\
        ;x7 <- Bin.get\
        ;x8 <- Bin.get\
        ;x9 <- Bin.get\
        ;x10 <- Bin.get\
        ;x11 <- Bin.get\
        ;return $! (CON x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11)}}
#define INSTANCE_Bin_1x12(A_t,CON)\
  instance Bin (A_t) where\
    {put (CON x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12) = do\
        {Bin.put x1\
        ;Bin.put x2\
        ;Bin.put x3\
        ;Bin.put x4\
        ;Bin.put x5\
        ;Bin.put x6\
        ;Bin.put x7\
        ;Bin.put x8\
        ;Bin.put x9\
        ;Bin.put x10\
        ;Bin.put x11\
        ;Bin.put x12}\
    ;get = do\
        {x1 <- Bin.get\
        ;x2 <- Bin.get\
        ;x3 <- Bin.get\
        ;x4 <- Bin.get\
        ;x5 <- Bin.get\
        ;x6 <- Bin.get\
        ;x7 <- Bin.get\
        ;x8 <- Bin.get\
        ;x9 <- Bin.get\
        ;x10 <- Bin.get\
        ;x11 <- Bin.get\
        ;x12 <- Bin.get\
        ;return $! (CON x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12)}}
#define INSTANCE_Bin_1x13(A_t,CON)\
  instance Bin (A_t) where\
    {put (CON x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13) = do\
        {Bin.put x1\
        ;Bin.put x2\
        ;Bin.put x3\
        ;Bin.put x4\
        ;Bin.put x5\
        ;Bin.put x6\
        ;Bin.put x7\
        ;Bin.put x8\
        ;Bin.put x9\
        ;Bin.put x10\
        ;Bin.put x11\
        ;Bin.put x12\
        ;Bin.put x13}\
    ;get = do\
        {x1 <- Bin.get\
        ;x2 <- Bin.get\
        ;x3 <- Bin.get\
        ;x4 <- Bin.get\
        ;x5 <- Bin.get\
        ;x6 <- Bin.get\
        ;x7 <- Bin.get\
        ;x8 <- Bin.get\
        ;x9 <- Bin.get\
        ;x10 <- Bin.get\
        ;x11 <- Bin.get\
        ;x12 <- Bin.get\
        ;x13 <- Bin.get\
        ;return $! (CON x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13)}}
#define INSTANCE_Bin_1x14(A_t,CON)\
  instance Bin (A_t) where\
    {put (CON x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14) = do\
        {Bin.put x1\
        ;Bin.put x2\
        ;Bin.put x3\
        ;Bin.put x4\
        ;Bin.put x5\
        ;Bin.put x6\
        ;Bin.put x7\
        ;Bin.put x8\
        ;Bin.put x9\
        ;Bin.put x10\
        ;Bin.put x11\
        ;Bin.put x12\
        ;Bin.put x13\
        ;Bin.put x14}\
    ;get = do\
        {x1 <- Bin.get\
        ;x2 <- Bin.get\
        ;x3 <- Bin.get\
        ;x4 <- Bin.get\
        ;x5 <- Bin.get\
        ;x6 <- Bin.get\
        ;x7 <- Bin.get\
        ;x8 <- Bin.get\
        ;x9 <- Bin.get\
        ;x10 <- Bin.get\
        ;x11 <- Bin.get\
        ;x12 <- Bin.get\
        ;x13 <- Bin.get\
        ;x14 <- Bin.get\
        ;return $! (CON x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14)}}
#endif

/***************************************************************************/

#endif
