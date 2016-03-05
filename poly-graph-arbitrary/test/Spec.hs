{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

import Data.DeriveTH (derive, makeArbitrary)
import Test.Hspec (hspec, describe, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck.Arbitrary (Arbitrary(..))
import Test.QuickCheck.Gen (Gen, generate)

import Data.Graph.HGraph
import Data.Graph.HGraph.Arbitrary

data Node
  = Node
  { nodeId :: Int
  , pointer :: Maybe Int
  } deriving (Show, Eq)
$(derive makeArbitrary ''Node)
instance Node `Link` Maybe Node where
  (Node id1 _) `link` n2 = Node id1 $ nodeId <$> n2

main :: IO ()
main = do
  print =<< generate (arbitrary :: Gen (Tree (Node :<: Maybe Node)))
  hspec $
    describe "Arbitrary" $ do
      prop "`Always` always links" $ \((x :<: Always y :<: Nil) :: Tree (Node :<: Always Node)) ->
          pointer x `shouldBe` Just (nodeId y)
      prop "`Never` never links" $ \((x :<: Never :<: Nil) :: Tree (Node :<: Never Node)) ->
        pointer x `shouldBe` Nothing
