#ifndef _MM_CONTAINERS_H

#define IMPORT_MM_CONTAINERS_IX\
  import MM.Data.Ix.Types;\
  import MM.Data.Ix.Map(IxMap,Index,Rename,Trans);\
  import MM.Data.Ix.Set(IxSet);\
  import MM.Data.Ix.Trie(IxTrie);\
  import qualified MM.Data.Ix.Map as Ix;\
  import qualified MM.Data.Ix.Set as IxS;\
  import qualified MM.Data.Ix.Set as IxT;

#define IMPORT_MM_CONTAINERS_UF\
  import MM.Data.Quotient.UF(UF);\
  import qualified MM.Data.Quotient.UF as UF;\
  import qualified MM.Data.Class.UF as U;

#define IMPORT_MM_CONTAINERS_INTMAPSET\
  import MM.Data.Map.Int(IntMap);\
  import MM.Data.Map.Word(WordMap);\
  import MM.Data.Set.Int(IntSet);\
  import qualified MM.Data.Map.Int as IM;\
  import qualified MM.Data.Map.Word as WM;\
  import qualified MM.Data.Set.Int as IS;

#define IMPORT_MM_CONTAINERS_CLASSES\
  import MM.Data.Class.List;\
  import MM.Data.Class.O;\
  import MM.Data.Class.Share;

#define IMPORT_MM_CONTAINERS_ORDMAPSET\
  import MM.Data.Map.Ord(Map);\
  import MM.Data.Set.Ord(Set);\
  import qualified MM.Data.Map.Ord as M;\
  import qualified MM.Data.Set.Ord as S;

#define IMPORT_MM_CONTAINERS_UTIL\
  import MM.Data.Util;

#define IMPORT_MM_CONTAINERS_ALL\
  IMPORT_MM_DATA_UTIL;\
  IMPORT_MM_DATA_IX;\
  IMPORT_MM_DATA_UF;\
  IMPORT_MM_DATA_INTMAPSET;\
  IMPORT_MM_DATA_ORDMAPSET;\
  IMPORT_MM_DATA_CLASSES;

#endif
