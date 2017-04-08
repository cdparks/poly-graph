{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Common where

import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy(..))
import Data.Text (Text, pack, isSuffixOf)
import qualified Data.Vector.Sized as Sized
import Database.Persist
import Database.Persist.TH
import GHC.TypeLits (KnownNat, natVal)
import Test.QuickCheck.Arbitrary (Arbitrary(..), vector)

instance Arbitrary Text where
  arbitrary = pack . filter (not . isBadChar) <$> arbitrary
    where isBadChar x = x == '\NUL' || x == '\\' -- Make postgres vomit

instance (KnownNat n, Arbitrary a) => Arbitrary (Sized.Vector n a) where
  arbitrary =
    fromMaybe (error "`vector` should return list of requested length") . Sized.fromList <$>
    vector (fromIntegral (natVal (Proxy :: Proxy n)))

testSettings :: MkPersistSettings
testSettings = sqlSettings { mpsGenerateLenses = True }
