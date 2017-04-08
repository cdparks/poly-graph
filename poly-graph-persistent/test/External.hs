{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module External where

import Test.Hspec

import Data.Text (Text, pack)
import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.TH
import GHC.Generics (Generic)
import Test.QuickCheck.Arbitrary (Arbitrary(..))

import Data.Graph.HGraph.Persistent.TH

import Common

share [mkPersist testSettings,  mkMigrate "testMigrate"] [persistLowerCase|
  External
    name Text
    position Int
    UniquePosition position
    deriving Show Eq Generic
|]

instance Arbitrary External where
  arbitrary = External "external" <$> arbitrary

$(mkUniquenessChecksFor ''External)
