{-# LANGUAGE IncoherentInstances #-}

module MM.Data.Class.Lattice (
   Diff(..)
  ,Lat(..),LatM(..)
  ,Meet(..),Join(..)
  ,MeetM(..),JoinM(..)
  ,MeetLat(..),JoinLat(..)
  ,MeetLatM(..),JoinLatM(..)
) where

import MM.Data.Class.Empty(Bot,Top)

-----------------------------------------------------------------------------

-- | .
class Diff a where (\\) :: a -> a -> a

class Meet a where (/\) :: a -> a -> a
class Join a where (\/) :: a -> a -> a
class MeetM m a where meetM :: a -> a -> m a
class JoinM m a where joinM :: a -> a -> m a

class (Bot a, Meet a) => MeetLat a
class (Top a, Join a) => JoinLat a
class (Bot a, MeetM m a) => MeetLatM m a
class (Top a, JoinM m a) => JoinLatM m a

class (MeetLat a, JoinLat a) => Lat a
class (MeetLatM m a, JoinLatM m a) => LatM m a

-----------------------------------------------------------------------------

instance (Bot a, Meet a) => MeetLat a
instance (Top a, Join a) => JoinLat a
instance (Bot a, MeetM m a) => MeetLatM m a
instance (Top a, JoinM m a) => JoinLatM m a
instance (MeetLat a, JoinLat a) => Lat a
instance (MeetLatM m a, JoinLatM m a) => LatM m a

-----------------------------------------------------------------------------

instance Lat ()
instance MeetLat ()
instance JoinLat ()
instance Meet () where (/\) () () = ()
instance Join () where (\/) () () = ()
instance Diff () where (\\) () () = ()

-----------------------------------------------------------------------------

instance (Lat a, Lat b) => Lat (a,b)
instance (MeetLat a, MeetLat b) => MeetLat (a,b)
instance (JoinLat a, JoinLat b) => JoinLat (a,b)
instance (Meet a, Meet b) => Meet (a,b) where
  (/\) (a1,b1) (a2,b2)
    | !a <- a1/\a2
    , !b <- b1/\b2
    = (a,b)
instance (Join a, Join b) => Join (a,b) where
  (\/) (a1,b1) (a2,b2)
    | !a <- a1\/a2
    , !b <- b1\/b2
    = (a,b)
instance (Diff a, Diff b) => Diff (a,b) where
  (\\) (a1,b1) (a2,b2)
    | !a <- a1\\a2
    , !b <- b1\\b2
    = (a,b)

-----------------------------------------------------------------------------
