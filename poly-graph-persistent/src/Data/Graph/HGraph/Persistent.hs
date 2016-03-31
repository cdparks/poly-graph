{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-- Pattern synonyms and exhaustivity checking don't work well together
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Graph.HGraph.Persistent where

import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.IO.Class (MonadIO)
import Data.Functor.Identity
import Data.Proxy
import Data.Tagged
import Database.Persist

import Data.Graph.HGraph
import Data.Graph.HGraph.Internal

type Never = Proxy
type Always = Identity

pattern Always a = Identity a
pattern Never = Proxy

class InsertEntityGraph a where
  type InsertEntityGraphBackend a
  insertEntityGraph
    :: (Monad m, MonadIO m, PersistStore (InsertEntityGraphBackend a))
    => a -> ReaderT (InsertEntityGraphBackend a) m ()

instance {-# OVERLAPPING #-}
  (b `GPointsAt` Entity a)
  => (Key a, b) `GPointsAt` Entity a where
  (_, b) `gPointsAt` e@(Entity k _) = (k, b `gPointsAt` e)
instance {-# OVERLAPPING #-}
  (b `GPointsAt` Maybe (Entity a))
  => (Maybe (Key a), b) `GPointsAt` Maybe (Entity a) where
  (_, b) `gPointsAt` e@(Just (Entity k _)) = (Just k, b `gPointsAt` e)
  (_, b) `gPointsAt` e@Nothing = (Nothing, b `gPointsAt` e)

-- Can't make `HGraph '[]` the base case
-- because then we don't know which type of backend to use
instance (InsertEntityGraph a) => InsertEntityGraph (HGraph '[ '(a, i, is)]) where
  type InsertEntityGraphBackend (HGraph '[ '(a, i, is)]) = InsertEntityGraphBackend a
  insertEntityGraph (a :<: Nil) = insertEntityGraph a
instance
  ( InsertEntityGraph a, InsertEntityGraph (HGraph (b ': c))
  , InsertEntityGraphBackend a ~ InsertEntityGraphBackend (HGraph (b ': c))
  ) => InsertEntityGraph (HGraph ('(a, i, is) ': b ': c)) where
  type InsertEntityGraphBackend (HGraph ('(a, i, is) ': b ': c)) = InsertEntityGraphBackend a
  insertEntityGraph (a :<: b) = insertEntityGraph b >> insertEntityGraph a

instance InsertEntityGraph (Entity a) where
  type InsertEntityGraphBackend (Entity a) = PersistEntityBackend a
  insertEntityGraph (Entity key val) = insertKey key val

instance InsertEntityGraph (Never (a :: *)) where
  type InsertEntityGraphBackend (Never a) = InsertEntityGraphBackend a
  insertEntityGraph Never = return ()
instance (InsertEntityGraph a) => InsertEntityGraph (Always a) where
  type InsertEntityGraphBackend (Always a) = InsertEntityGraphBackend a
  insertEntityGraph (Always a) = insertEntityGraph a
instance (InsertEntityGraph a) => InsertEntityGraph (Maybe a) where
  type InsertEntityGraphBackend (Maybe a) = InsertEntityGraphBackend a
  insertEntityGraph (Just a) = insertEntityGraph a
  insertEntityGraph Nothing = return ()

type family Unwrap a where
  Unwrap (Entity a) = a
  Unwrap (Never (Entity a)) = Never a
  Unwrap (Always (Entity a)) = Always a
  Unwrap (Maybe (Entity a)) = Maybe a
type family UnwrapAll a where
  UnwrapAll ('(a, i, is) ': as) = '(Unwrap a, i, is) ': UnwrapAll as
  UnwrapAll '[] = '[]

class InsertGraph a b backend | b -> a, a -> backend , b -> backend where
  insertGraph :: (Monad m, MonadIO m, PersistStore backend) => a -> ReaderT backend m b

instance
  ( Tagged '(i, is) a `PointsAtR` HGraph (e ': f)
  , InsertGraph (HGraph (b ': c)) (HGraph (e ': f)) backend
  , InsertGraph a d backend
  , (b ': c) ~ UnwrapAll (e ': f)
  , a ~ Unwrap d
  )
  => InsertGraph (HGraph ('(a, i, is) ': b ': c)) (HGraph ('(d, i, is) ': e ': f)) backend where
  insertGraph (a `Cons` b) = do
    b' <- insertGraph b
    let a' = unTagged $ a `pointsAtR` b'
    a'' <- insertGraph a'
    pure $ a'' `Cons` b'
instance
  ( a ~ Unwrap b
  , InsertGraph a b backend
  )
  => InsertGraph (HGraph '[ '(a, i, is)]) (HGraph '[ '(b, i, is)]) backend where
  insertGraph (a :<: Nil) = do
    e <- insertGraph a
    pure $ e `Cons` Nil

instance
  ( a ~ Unwrap b
  , InsertGraph a b backend
  ) => InsertGraph a (Tagged '(i, is) b) backend where
  insertGraph a = Tagged <$> insertGraph a

instance ( PersistEntityBackend a ~ backend, PersistEntity a
         ) => InsertGraph a (Entity a) backend where
  insertGraph a = flip Entity a <$> insert a
-- The constraint here isn't strictly necessary but it allows us to add the `a -> backend` and `b -> backend` fundeps
instance (PersistEntityBackend a ~ backend) => InsertGraph (Never a) (Never (Entity a)) backend where
  insertGraph Never = pure Never
instance (InsertGraph a (Entity a) backend) => InsertGraph (Always a) (Always (Entity a)) backend where
  insertGraph (Always a) = Always <$> insertGraph a
instance (InsertGraph a (Entity a) backend) => InsertGraph (Maybe a) (Maybe (Entity a)) backend where
  insertGraph (Just a) = Just <$> insertGraph a
  insertGraph Nothing = pure Nothing

instance {-# OVERLAPPABLE #-} (a `PointsAt` b) => Entity a `PointsAt` b where
  Entity id' a `pointsAt` b = Entity id' $ a `pointsAt` b
instance {-# OVERLAPPING #-} (a `PointsAt` Maybe b) => Entity a `PointsAt` Maybe b where
  Entity id' a `pointsAt` b = Entity id' $ a `pointsAt` b
instance {-# OVERLAPPING #-} (a `PointsAt` Always b) => Entity a `PointsAt` Always b where
  Entity id' a `pointsAt` b = Entity id' $ a `pointsAt` b
instance {-# OVERLAPPING #-} (a `PointsAt` Never b) => Entity a `PointsAt` Never b where
  Entity id' a `pointsAt` b = Entity id' $ a `pointsAt` b

-- | No-op instances for use with `insertGraph`
instance {-# OVERLAPPABLE #-} (Entity a `PointsAt` Entity b) => a `PointsAt` b where
  a `pointsAt` _ = a
instance {-# OVERLAPPABLE #-} (a `PointsAt` Maybe (Entity b)) => a `PointsAt` Maybe b where
  a `pointsAt` _ = a
