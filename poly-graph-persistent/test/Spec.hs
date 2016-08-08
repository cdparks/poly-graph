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
import Data.Proxy (Proxy(..))
import Data.Text (Text, pack)
import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.TH
import GHC.Generics (Generic)
import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.QuickCheck.Gen (generate)
import Text.Shakespeare.Text (st)

import Data.Graph.HGraph
import Data.Graph.HGraph.Instances ()
import Data.Graph.HGraph.Persistent
import Data.Graph.HGraph.Persistent.Instances ()
import Data.Graph.HGraph.TH

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
    |]
    []

instance (Arbitrary a) => Arbitrary (Always a) where
  arbitrary = pure <$> arbitrary
instance Arbitrary (Never a) where
  arbitrary = pure Never

instance Arbitrary Text where
  arbitrary = pack . filter (not . isBadChar) <$> arbitrary
    where isBadChar x = x == '\NUL' || x == '\\' -- Make postgres vomit


share [mkPersist sqlSettings { mpsGenerateLenses = True },  mkMigrate "testMigrate"] [persistLowerCase|
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
  arbitrary = Foo "foo" <$> arbitrary <*> arbitrary

declareBases [''SelfRef, ''State, ''District, ''School, ''Student, ''Teacher, ''Foo, ''Text]

instance SelfRef `PointsAt` Entity SelfRef
instance SelfRef `PointsAt` Maybe (Entity SelfRef)
instance Student `PointsAt` Entity Teacher
instance Teacher `PointsAt` Entity School
instance School `PointsAt` Maybe (Entity District)
instance District `PointsAt` Entity State
instance District `PointsAt` Maybe (Entity State)
instance Foo `PointsAt` Entity Student
instance Foo `PointsAt` Entity Teacher

_entityKey :: Lens' (Entity a) (Key a)
_entityKey pure' (Entity i e) = (\i' -> Entity i' e) <$> pure' i

type M = ReaderT SqlBackend (LoggingT IO)
main :: IO ()
main = do
  runConn $ runMigrationUnsafe testMigrate
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
            :: IO (Line '[Entity Student, Entity Teacher, Entity School, Always (Entity District), Entity State])
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
          :: IO (HGraph '[ '(1, '[], Always (Entity SelfRef)), '(2, '[], Maybe (Entity SelfRef)) ])
        (arbGraph ^? _head . _Always . _entityVal . selfRefSelfRefId . _Just) `shouldBe` Nothing
      it "defaults `Maybe` keys to `Nothing` during insertion when they're in the middle of the graph" $ db $ do
        entGraph <-
          insertGraph . unRawGraph =<< liftIO (generate arbitrary)
          :: M (HGraph '[ '(1, '[], Always (Entity SelfRef)), '(2, '[], Maybe (Entity SelfRef)) ])
        liftIO $ (entGraph ^? _head . _Always . _entityVal . selfRefSelfRefId . _Just) `shouldBe` Nothing
      it "works with Maybe key to plain" $ db $ do
        graph <-
          unRawGraph <$> liftIO (generate arbitrary)
            :: M (
                 HGraph
                   '[ '("Plain1", '["Plain2"], SelfRef)
                    , '("Plain2", '["Never"], SelfRef)
                    , '("Never", '[], Never SelfRef)
                    ]
                 )
        graph' <-
          insertGraph graph
            :: M (
                 HGraph
                   '[ '("Plain1", '["Plain2"], Entity SelfRef)
                    , '("Plain2", '["Never"], Entity SelfRef)
                    , '("Never", '[], Never (Entity SelfRef))
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
                   '[ '("Plain1", '["Maybe1", "Always1", "Never1", "Plain2"], SelfRef)
                    , '("Maybe1", '["Always1", "Never1", "Plain2", "Maybe2"], Maybe SelfRef)
                    , '("Always1", '["Never1", "Plain2", "Maybe2", "Always2"], Always SelfRef)
                    , '("Never1", '["Plain2", "Maybe2", "Always2", "Never2"], Never SelfRef)
                    , '("Plain2", '["Never2"], SelfRef)
                    , '("Maybe2", '["Never2"], Maybe SelfRef)
                    , '("Always2", '["Never2"], Always SelfRef)
                    , '("Never2", '[], Never SelfRef)
                    ]
                 )
        _ <-
          insertGraph graph
            :: M (
                 HGraph
                   '[ '("Plain1", '["Maybe1", "Always1", "Never1", "Plain2"], Entity SelfRef)
                    , '("Maybe1", '["Always1", "Never1", "Plain2", "Maybe2"], Maybe (Entity SelfRef))
                    , '("Always1", '["Never1", "Plain2", "Maybe2", "Always2"], Always (Entity SelfRef))
                    , '("Never1", '["Plain2", "Maybe2", "Always2", "Never2"], Never (Entity SelfRef))
                    , '("Plain2", '["Never2"], Entity SelfRef)
                    , '("Maybe2", '["Never2"], Maybe (Entity SelfRef))
                    , '("Always2", '["Never2"], Always (Entity SelfRef))
                    , '("Never2", '[], Never (Entity SelfRef))
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
        (st :< te :< sc :< Always di :< state :< Nil) <-
          insertGraph graph
            :: M (
                 Line
                   '[ Entity Student
                    , Entity Teacher
                    , Entity School
                    , Always (Entity District)
                    , Entity State
                    ]
                 )
        let manualTree = (Entity stId stB, Entity teId teB, Entity scId scB, Entity diId diB, Entity stateId2 stateB)
        let autoTree = (st, te, sc, di, state)
        liftIO $ autoTree `shouldBe` manualTree
