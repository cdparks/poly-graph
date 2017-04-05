{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import Test.Hspec

import Control.Lens hiding ((:<), _head, _tail)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger (LoggingT(..), runStderrLoggingT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.Resource (MonadBaseControl)
import Data.Monoid (Endo)
import Data.Proxy (Proxy(..))
import Data.Text (Text, pack)
import qualified Data.Vector.Sized as Sized
import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.TH
import GHC.Generics (Generic)
import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.QuickCheck.Gen (generate, Gen)
import Text.Shakespeare.Text (st)

import Data.Graph.HGraph
import Data.Graph.HGraph.Instances ()
import Data.Graph.HGraph.Persistent
import Data.Graph.HGraph.Persistent.Instances ()
import Data.Graph.HGraph.Persistent.TH
import Data.Graph.HGraph.TH

import Common
import External hiding (testMigrate)
import qualified External as External

connString :: ConnectionString
connString = "host=localhost port=5432 user=test dbname=poly-graph password=test"

runConn :: (MonadIO m, MonadBaseControl IO m) => SqlPersistT (LoggingT m) t -> m t
runConn = runStderrLoggingT . withPostgresqlConn connString . runSqlConn

db :: (MonadIO m, MonadBaseControl IO m) => SqlPersistT (LoggingT m) () -> m ()
db actions = runConn $ actions >> resetSequences >> transactionUndo

resetSequences :: (MonadIO m) => SqlPersistT (LoggingT m) [Single Int]
resetSequences =
  rawSql
    [st|
      SELECT SETVAL('district_id_seq', 1, false);
      SELECT SETVAL('foo_id_seq', 1, false);
      SELECT SETVAL('school_id_seq', 1, false);
      SELECT SETVAL('self_ref_id_seq', 1, false);
      SELECT SETVAL('state_id_seq', 1, false);
      SELECT SETVAL('student_id_seq', 1, false);
      SELECT SETVAL('teacher_id_seq', 1, false);
      SELECT SETVAL('foo_id_seq', 1, false);
      SELECT SETVAL('baz_id_seq', 1, false);
      SELECT SETVAL('quux_id_seq', 1, false);
      SELECT SETVAL('merp_id_seq', 1, false);
      SELECT SETVAL('local_id_seq', 1, false);
      SELECT SETVAL('external_id_seq', 1, false);
    |]
    []

share [mkUniquenessChecksIgnoring externalFk testSettings, mkPersist testSettings,  mkMigrate "testMigrate"] [persistLowerCase|
  SelfRef
    name Text
    selfRefId SelfRefId Maybe
    deriving Show Eq Generic
  State
    name Text
    deriving Show Eq Generic
  District
    name Text
    stateId StateId
    deriving Show Eq Generic
  School
    name Text
    districtId DistrictId Maybe
    deriving Show Eq Generic
  Teacher
    name Text
    schoolId SchoolId
    deriving Show Eq Generic
  Student
    name Text
    teacherId TeacherId
    deriving Show Eq Generic
  Foo
    name Text
    studentId StudentId Maybe
    teacherId TeacherId Maybe
    bar Bool
    UniqueBar bar -- This is a nonsensical constraint just to test uniqueness violations
    deriving Show Eq Generic
  Baz
    name Text
    foo FooId
    UniqueFoo foo -- Another nonsensical constraint but one that only has a FK
    deriving Show Eq Generic
  Quux
    name Text
    bar Bool
    foo FooId
    UniqueFooBar foo bar -- A nonsensical constraint that has an FK and plain value
    deriving Show Eq Generic
  Merp
    name Text
    bar Bool
    baz Bool
    whomp Bool Maybe
    foo FooId
    UniqueFooBarBaz foo bar baz -- A nonsensical constraint that has an FK and two plain values
    UniqueWhomp whomp !force -- A second constraint with a nullable field
    deriving Show Eq Generic
  Local
    name Text
    flag Bool
    external ExternalId external-fk
    UniqueFlagExternal flag external -- A constraint pointing at an FK that persistent doesn't know about
    deriving Show Eq Generic
|]

instance Arbitrary State where
  arbitrary = pure $ State "grault"
instance Arbitrary District where
  arbitrary = District "foo" <$> arbitrary
instance Arbitrary School where
  arbitrary = School "bar" <$> arbitrary
instance Arbitrary Teacher where
  arbitrary = Teacher "baz" <$> arbitrary
instance Arbitrary Student where
  arbitrary = Student "qux" <$> arbitrary
instance Arbitrary SelfRef where
  arbitrary = SelfRef "self" <$> arbitrary
instance Arbitrary Foo where
  arbitrary = Foo "foo" <$> arbitrary <*> arbitrary <*> arbitrary
instance Arbitrary Baz where
  arbitrary = Baz "baz" <$> arbitrary
instance Arbitrary Quux where
  arbitrary = Quux "quux" <$> arbitrary <*> arbitrary
instance Arbitrary Merp where
  arbitrary = Merp "merp" <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
instance Arbitrary Local where
  arbitrary = Local "merp" <$> arbitrary <*> arbitrary

instance SelfRef `PointsAt` Entity SelfRef
instance SelfRef `PointsAt` Maybe (Entity SelfRef)
instance Student `PointsAt` Entity Teacher
instance Teacher `PointsAt` Entity School
instance School `PointsAt` Entity District
instance School `PointsAt` Maybe (Entity District)
instance District `PointsAt` Entity State
instance District `PointsAt` Maybe (Entity State)
instance Foo `PointsAt` Entity Student
instance Foo `PointsAt` Entity Teacher
instance Baz `PointsAt` Entity Foo
instance Quux `PointsAt` Entity Foo
instance Merp `PointsAt` Entity Foo
instance Local `PointsAt` Entity External

_entityKey :: Lens' (Entity a) (Key a)
_entityKey pure' (Entity i e) = (\i' -> Entity i' e) <$> pure' i

type M = ReaderT SqlBackend (LoggingT IO)
main :: IO ()
main = do
  runConn $ do
    runMigrationUnsafe External.testMigrate
    runMigrationUnsafe testMigrate
  hspec $
    describe "poly-graph-persistent" $ do
      it "works with plucked lenses" $ do
        graph <-
          unRawGraph <$> generate arbitrary
            :: IO (Line '[Student, Teacher, School])
        let graph' = graph & pluck (Proxy :: Proxy School) . schoolName .~ "Hello"
        pure ()
      -- it "doesn't type check with a dangling (non-`Maybe`) key" $ db $ do
      --   graph <- liftIO (generate arbitrary) :: M (HGraph '[ '("Teacher", '[], Teacher) ])
      --   liftIO $ print graph
      -- it "doesn't type check with a repeated name" $ db $ do
      --   graph <-
      --     liftIO (generate arbitrary)
      --     :: M (HGraph '[ '("Teacher", '["Teacher"], Teacher), '("Teacher", '[], Student) ])
      --   liftIO $ print graph
      it "generates arbitrary entities" $ do
        _ <-
          generate arbitrary
            :: IO (Line '[Entity Student, Entity Teacher, Entity School, Entity District, Entity State])
        pure ()
      it "works with paired vectors" $ db $ do
        void . insert $ School "Bump id" Nothing
        arbGraph <- unRawGraph <$> liftIO (generate arbitrary)
        entGraph <-
          insertGraph arbGraph
            :: M
              (HGraph
               '[ '("T", '["S"], Sized.Vector 3 (Entity Teacher))
                , '("S", '["D"], Sized.Vector 3 (Entity School))
                , '("D", '["St"], Sized.Vector 3 (Entity District))
                , '("St", '[], Entity State)
                ]
              )
        liftIO $ print entGraph
        pure ()
      it "defaults only missing keys to nothing" $ db $ do
        arbGraph <- unRawGraph <$> liftIO (generate arbitrary)
        entGraph <-
          insertGraph arbGraph
          :: M (
               HGraph
                '[ '("F", '["S"], Entity Foo)
                 , '("S", '["T"], Entity Student)
                 , '("T", '["Sc"], Entity Teacher)
                 , '("Sc", '["Di"], Entity School)
                 , '("Di", '["St"], Maybe (Entity District))
                 , '("St", '[], Entity State)
                 ]
               )
        liftIO $ (entGraph ^. _head . _entityVal . fooTeacherId) `shouldBe` Nothing
        liftIO $ (entGraph ^. _head . _entityVal . fooStudentId) `shouldBe` (Just $ entGraph ^. _tail . _head . _entityKey)
      it "defaults `Maybe` keys to `Nothing` during `Arbitrary` creation when they're at the end of the graph" $ do
        arbGraph <- generate arbitrary :: IO (Line '[Maybe (Entity SelfRef)])
        (arbGraph ^? _head . _Just . _entityVal . selfRefSelfRefId . _Just) `shouldBe` Nothing
      it "defaults `Maybe` keys to `Nothing` during insertion when they're at the end of the graph" $ db $ do
        entGraph <- insertGraph . unRawGraph =<< liftIO (generate arbitrary) :: M (Line '[Maybe (Entity SelfRef)])
        liftIO $ (entGraph ^? _head . _Just . _entityVal . selfRefSelfRefId . _Just) `shouldBe` Nothing
      it "defaults `Maybe` keys to `Nothing` during `Arbitrary` creation when they're in the middle of the graph" $ do
        arbGraph <-
          generate arbitrary
          :: IO (HGraph '[ '(1, '[], Entity SelfRef), '(2, '[], Maybe (Entity SelfRef)) ])
        (arbGraph ^? _head . _entityVal . selfRefSelfRefId . _Just) `shouldBe` Nothing
      it "defaults `Maybe` keys to `Nothing` during insertion when they're in the middle of the graph" $ db $ do
        entGraph <-
          insertGraph . unRawGraph =<< liftIO (generate arbitrary)
          :: M (HGraph '[ '(1, '[], Entity SelfRef), '(2, '[], Maybe (Entity SelfRef)) ])
        liftIO $ (entGraph ^? _head . _entityVal . selfRefSelfRefId . _Just) `shouldBe` Nothing
      it "works with unique constraints" $ db $ do
        graph <-
          liftIO (generate (unique fooBar . unRawGraph =<< arbitrary))
            :: M (
                 HGraph
                   '[ '("Foo1", '[], Foo)
                    , '("Foo2", '[], Foo)
                    ]
                 )
        graph' <-
          insertGraph graph
            :: M (
                 HGraph
                   '[ '("Foo1", '[], Entity Foo)
                    , '("Foo2", '[], Entity Foo)
                    ]
                 )
        pure ()
      it "works with unique constraints without using unique" $ db $ do
        graph <-
          liftIO (generate (ensureGraphUniqueness =<< fmap unRawGraph arbitrary))
            :: M (
                 HGraph
                   '[ '("Foo1", '[], Foo)
                    , '("Foo2", '[], Foo)
                    ]
                 )
        graph' <-
          insertGraph graph
            :: M (
                 HGraph
                   '[ '("Foo1", '[], Entity Foo)
                    , '("Foo2", '[], Entity Foo)
                    ]
                 )
        pure ()
      it "works with unique constraints and unique if the latter is used carefully" $ db $ do
        graph <-
          liftIO (generate (unique fooName =<< ensureGraphUniqueness =<< fmap unRawGraph arbitrary))
            :: M (
                 HGraph
                   '[ '("Foo1", '[], Foo)
                    , '("Foo2", '[], Foo)
                    ]
                 )
        graph' <-
          insertGraph graph
            :: M (
                 HGraph
                   '[ '("Foo1", '[], Entity Foo)
                    , '("Foo2", '[], Entity Foo)
                    ]
                 )
        pure ()
      it "ensures internal uniqueness in a single node" $ db $ do
        graph <-
          liftIO (generate (ensureGraphUniqueness =<< fmap unRawGraph arbitrary))
            :: M (
                 HGraph
                   '[ '("Foo", '[], Sized.Vector 2 Foo)
                    ]
                 )
        graph' <-
          insertGraph graph
            :: M (
                 HGraph
                   '[ '("Foo", '[], Sized.Vector 2 (Entity Foo))
                    ]
                 )
        pure ()
      it "user can edit graph after it's been uniqued" $ db $ do
        graph <-
          liftIO (generate (ensureGraphUniqueness =<< fmap unRawGraph arbitrary))
            <&> pluck (Proxy :: Proxy "Foo1") . fooName .~ "foo1"
            <&> pluck (Proxy :: Proxy "Foo2") . fooName .~ "foo2"
              :: M (
                   HGraph
                     '[ '("Foo1", '[], Foo)
                      , '("Foo2", '[], Foo)
                      ]
                   )
        graph' <-
          insertGraph graph
            :: M (
                 HGraph
                   '[ '("Foo1", '[], Entity Foo)
                    , '("Foo2", '[], Entity Foo)
                    ]
                 )
        liftIO $ do
          graph' ^. pluck (Proxy :: Proxy "Foo1") . _entityVal . fooName `shouldBe` "foo1"
          graph' ^. pluck (Proxy :: Proxy "Foo2") . _entityVal . fooName `shouldBe` "foo2"
      it "user can un-unique graph after it's been uniqued" $ db $ do
        graph <-
          liftIO (generate (ensureGraphUniqueness =<< fmap unRawGraph arbitrary))
            <&> pluck (Proxy :: Proxy "Foo1") . fooBar .~ False
            <&> pluck (Proxy :: Proxy "Foo2") . fooBar .~ False
              :: M (
                   HGraph
                     '[ '("Foo1", '[], Foo)
                      , '("Foo2", '[], Foo)
                      ]
                   )
        let
          insertUniquenessViolation =
            insertGraph graph
              :: M (
                   HGraph
                     '[ '("Foo1", '[], Entity Foo)
                      , '("Foo2", '[], Entity Foo)
                      ]
                   )
        liftIO $ runConn insertUniquenessViolation `shouldThrow` anyException
      it "ignores unique constraints consisting solely of FKs" $ db $ do
        graph <-
          liftIO (generate (ensureGraphUniqueness =<< fmap unRawGraph arbitrary))
            :: M (
                 HGraph
                   '[ '("Baz1", '["Foo"], Baz)
                    , '("Baz2", '["Foo"], Baz)
                    , '("Foo", '[], Foo)
                    ]
                 )
        let
          insertUniquenessViolation =
            insertGraph graph
              :: M (
                   HGraph
                     '[ '("Baz1", '["Foo"], Entity Baz)
                      , '("Baz2", '["Foo"], Entity Baz)
                      , '("Foo", '[], Entity Foo)
                      ]
                   )
        liftIO $ runConn insertUniquenessViolation `shouldThrow` anyException
      it "ignores the FK component of a unique constraint" $ db $ do
        graph <-
          liftIO (generate (ensureGraphUniqueness =<< fmap unRawGraph arbitrary))
            :: M (
                 HGraph
                   '[ '("Quux1", '["Foo"], Quux)
                    , '("Quux2", '["Foo"], Quux)
                    , '("Foo", '[], Foo)
                    ]
                 )
        graph' <-
          insertGraph graph
            :: M (
                 HGraph
                   '[ '("Quux1", '["Foo"], Entity Quux)
                    , '("Quux2", '["Foo"], Entity Quux)
                    , '("Foo", '[], Entity Foo)
                    ]
                 )
        pure ()
      it "ignores the FK component of a unique constraint with multiple plain components" $ db $ do
        graph <-
          liftIO (generate (ensureGraphUniqueness =<< fmap unRawGraph arbitrary))
            :: M (
                 HGraph
                   '[ '("Merp1", '["Foo"], Merp)
                    , '("Merp2", '["Foo"], Merp)
                    , '("Foo", '[], Foo)
                    ]
                 )
        graph' <-
          insertGraph graph
            :: M (
                 HGraph
                   '[ '("Merp1", '["Foo"], Entity Merp)
                    , '("Merp2", '["Foo"], Entity Merp)
                    , '("Foo", '[], Entity Foo)
                    ]
                 )
        pure ()
      it "ignores the component of a unique constraint marked with 'external-fk'" $ db $ do
        graph <-
          liftIO (generate (ensureGraphUniqueness =<< fmap unRawGraph arbitrary))
            :: M (
                 HGraph
                   '[ '("Local1", '["External"], Local)
                    , '("Local2", '["External"], Local)
                    , '("External", '[], External)
                    ]
                 )
        graph' <-
          insertGraph graph
            :: M (
                 HGraph
                   '[ '("Local1", '["External"], Entity Local)
                    , '("Local2", '["External"], Entity Local)
                    , '("External", '[], Entity External)
                    ]
                 )
        pure ()
      it "works with Maybe key to plain" $ db $ do
        graph <-
          unRawGraph <$> liftIO (generate arbitrary)
            :: M (
                 HGraph
                   '[ '("Plain1", '["Plain2"], SelfRef)
                    , '("Plain2", '[], SelfRef)
                    ]
                 )
        graph' <-
          insertGraph graph
            :: M (
                 HGraph
                   '[ '("Plain1", '["Plain2"], Entity SelfRef)
                    , '("Plain2", '[], Entity SelfRef)
                    ]
                 )
        liftIO $
          (graph' ^. pluck (Proxy :: Proxy "Plain1") . _entityVal . selfRefSelfRefId) `shouldBe`
          (Just $ graph' ^. pluck (Proxy :: Proxy "Plain2") . _entityKey)
      it "works with a variety of `Maybe`, `Always`, `Never` combinations" $ db $ do
        graph <-
          unRawGraph <$> liftIO (generate arbitrary)
            :: M (
                 HGraph
                   '[ '("Plain1", '["Maybe1", "Always1", "Plain2"], SelfRef)
                    , '("Maybe1", '["Always1", "Plain2", "Maybe2"], Maybe SelfRef)
                    , '("Always1", '["Plain2", "Maybe2", "Always2"], SelfRef)
                    , '("Plain2", '[], SelfRef)
                    , '("Maybe2", '[], Maybe SelfRef)
                    , '("Always2", '[], SelfRef)
                    ]
                 )
        _ <-
          insertGraph graph
            :: M (
                 HGraph
                   '[ '("Plain1", '["Maybe1", "Always1", "Plain2"], Entity SelfRef)
                    , '("Maybe1", '["Always1", "Plain2", "Maybe2"], Maybe (Entity SelfRef))
                    , '("Always1", '["Plain2", "Maybe2", "Always2"], Entity SelfRef)
                    , '("Plain2", '[], Entity SelfRef)
                    , '("Maybe2", '[], Maybe (Entity SelfRef))
                    , '("Always2", '[], Entity SelfRef)
                    ]
                 )
        pure ()

      it "Manual creation and insertion should produce the same results as automatic creation and insertion" $ db $ do
        stateId1 <- insert $ State "CA"
        void . insert $ District "bump id to prove we're doing something mildly interesting" stateId1

        stateB <- liftIO $ generate arbitrary
        stateId2 <- insert stateB
        diB <- fmap (set districtStateId stateId2) . liftIO $ generate arbitrary
        diId <- insert diB
        scB <- fmap (set schoolDistrictId (Just diId)) . liftIO $ generate arbitrary
        scId <- insert scB
        teB <- fmap (set teacherSchoolId scId) . liftIO $ generate arbitrary
        teId <- insert teB
        stB <- fmap (set studentTeacherId teId) . liftIO $ generate arbitrary
        stId <- insert stB

        resetSequences
        transactionUndo
        stateId3 <- insert $ State "CA"
        void . insert $ District "bump id to prove we're doing something mildly interesting" stateId3

        graph <-
          unRawGraph <$> liftIO (generate arbitrary)
        (st :< te :< sc :< di :< state :< Nil) <-
          insertGraph graph
            :: M (
                 Line
                   '[ Entity Student
                    , Entity Teacher
                    , Entity School
                    , Entity District
                    , Entity State
                    ]
                 )
        let manualTree = (Entity stId stB, Entity teId teB, Entity scId scB, Entity diId diB, Entity stateId2 stateB)
        let autoTree = (st, te, sc, di, state)
        liftIO $ autoTree `shouldBe` manualTree
