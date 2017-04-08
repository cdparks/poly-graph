{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Graph.HGraph.Persistent.TH
  ( NullableEqualityModuloFKs(..)
  , couldCauseUniquenessViolation
  , mkUniquenessChecks
  , mkUniquenessChecksFor
  ) where

import Database.Persist
import Language.Haskell.TH
import Data.Graph.HGraph.Persistent.TH.Internal

-- | 'couldCauseUniquenessViolation' returns 'True' if its arguments violate
-- at least one uniqueness constraint on that type, ignoring foreign keys.
couldCauseUniquenessViolation :: (PersistEntity a, NullableEqualityModuloFKs (Unique a)) => a -> a -> Bool
couldCauseUniquenessViolation lhs rhs =
  or (zipWith nullableEqualityModuloFKs (persistUniqueKeys lhs) (persistUniqueKeys rhs))

-- | Use 'mkUniquenessChecks' to generate instances for 'NullableEqualityModuloFKs'.
-- For example:
--
-- > share
-- >   [ mkPersist sqlSettings
-- >   , mkMigrate "migrate"
-- >   ]
-- >   [persistLowerCase|
-- >     Author
-- >       name Text
-- >       pseudonym Text Maybe
-- >       UniqueAuthorName name
-- >       UniqueAuthoPseudonym pseudonym !force
-- >       deriving Show Eq Generic
-- >     Book
-- >       title Text
-- >       authorId AuthorId
-- >       publicationDate Date
-- >       isbn ISBN
-- >       notes NotesId Maybe
-- >       UniquePublicationInfo authorId title publicationDate
-- >       UniqueISBN isbn
-- >       deriving Show Eq Generic
-- >     Notes
-- >       notes Text
-- >       deriving Show Eq Generic
-- >   |]
-- >
-- > $mkUniquenessChecks
--
-- 'mkUniquenessChecks' generates a 'NullableEqualityModuloFKs' instance for
-- each 'Unique' instance in scope that doesn't already have one.
--
-- > instance NullableEqualityModuloFKs Author where
-- >   nullableEqualityModuloFKs _lhs _rhs =
-- >     case (_lhs, _rhs) of
-- >       (UniqueAuthorName _lhsName, UniqueAuthorName _rhsName) ->
-- >         _lhsName == _rhsName
-- >       (UniqueAuthoPseudonym _lhsPseudonym, UniqueAuthoPseudonym _rhsPseudonym) ->
-- >         maybe False ((==) <$> _lhsPseudonym <*> _rhsPseudonym)
-- >       _ -> False
-- >
-- > instance NullableEqualityModuloFKs Book where
-- >   nullableEqualityModuloFKs _lhs _rhs =
-- >     case (_lhs, _rhs) of
-- >       ( UniquePublicationInfo _lhsAuthorId _lhsTitle _lhsPublicationDate
-- >       , UniquePublicationInfo _rhsAuthorId _rhsTitle _rhsPublicationDate) ->
-- >        _lhsTitle == _rhsTitle &&
-- >        _lhsPublicationDate == _rhsPublicationDate
-- >       (UniqueISBN _lhsIsbn, UniqueISBN _rhsIsbn) ->
-- >         _lhsIsbn == _rhsIsbn
-- >       _ -> False
-- >
-- > instance NullableEqualityModuloFKs Notes where
-- >   nullableEqualityModuloFKs _lhs _rhs = False
--
-- Note the difference in how non-null fields are compared versus how nullable
-- fields are compared. In Haskell, 'Nothing' is equal to 'Nothing', but in SQL,
-- NULL is not equal to NULL.
--
-- Additionally the foreign keys aren't compared since they haven't been updated
-- to actually point to other entities yet, so we can't rely on them contributing
-- to uniqueness.
--
-- Entities with no 'Unique' constructors always return 'False' for
-- 'nullableEqualityModuloFKs'.
--
-- Finally, we add a catch-all pattern if the entity has more than one 'Unique'
-- constructor. Because unique constructors will be compared pair-wise in a
-- predictable order, we should never hit this case, but ghc doesn't know this,
-- and we want to avoid generating an incomplete-pattern warning.
mkUniquenessChecks :: Q [Dec]
mkUniquenessChecks = fmap concat . traverse mkInstance =<< warnEmpty =<< availableUniqueInstances

-- | Use 'mkUniquenessChecksFor' with a specific name to generate an instances
-- for 'NullableEqualityModuloFKs' for that entity's 'Unique' instance. This plays
-- nice with 'mkUniquenessChecks' which only generates 'NullableEqualityModuloFKs'
-- instances for 'Unique' instances that don't already have one.
mkUniquenessChecksFor :: Name -> Q [Dec]
mkUniquenessChecksFor name = do
  instances <- reifyInstances ''Unique [ConT name]
  case instances of
    [uniqueInstance] -> mkInstance (unpackDataInstance uniqueInstance)
    _ -> fail $ "`" ++ show name ++ "`must have a data instance for `Unique`"
