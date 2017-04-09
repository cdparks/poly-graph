{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Data.Graph.HGraph.Persistent.TH.Internal where

import Control.Monad (filterM, when)
import Data.List.NonEmpty (nonEmpty)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Traversable (for)
import Database.Persist
import Language.Haskell.TH
import Language.Haskell.TH.ExpandSyns

-- | 'nullableEqualityModuloFKs' returns 'True' if its arguments
-- compare equal except on foreign keys and null ('Nothing') components
class NullableEqualityModuloFKs a where
  nullableEqualityModuloFKs :: a -> a -> Bool

warnEmpty :: [a] -> Q [a]
warnEmpty instances = do
  when (null instances) $
    reportWarning "`mkUniquenessChecks` used with no Unique data instances in scope"
  pure instances

availableUniqueInstances :: Q [(Type, [Con])]
availableUniqueInstances = do
  FamilyI _ instances <- reify ''Unique
  filterM (noInstanceYet . fst) $ map unpackDataInstance instances

noInstanceYet :: Type -> Q Bool
noInstanceYet ty = not <$> isInstance ''NullableEqualityModuloFKs [ConT ''Unique `AppT` ty]

unpackDataInstance :: Dec -> (Type, [Con])
unpackDataInstance (DataInstD _ _ [ty] cons _) = (ty, cons)
unpackDataInstance _ = error "Expected data instance for `Unique`"

mkInstance :: (Type, [Con]) -> Q [Dec]
mkInstance (ty, cons) = do
  lhs <- newName "_lhs"
  rhs <- newName "_rhs"
  branches <- mkExhaustive cons =<< mkBranches cons
  declareInstance (lhs, rhs) ty (mkBody (lhs, rhs) branches)

mkExhaustive :: [Con] -> [Match] -> Q [Match]
mkExhaustive [] _ = pure []
mkExhaustive [_] branches = pure branches
mkExhaustive _ branches = do
  wild <- match wildP (normalB $ conE $ 'False) []
  pure $ branches ++ [wild]

mkBody :: (Name, Name) -> [Match] -> Exp
mkBody _ [] = ConE $ 'False
mkBody (lhs, rhs) branches = CaseE (TupE [VarE lhs, VarE rhs]) branches

declareInstance :: (Name, Name) -> Type -> Exp -> Q [Dec]
declareInstance (lhs, rhs) ty body =
  [d|
    instance NullableEqualityModuloFKs (Unique $(pure ty)) where
      nullableEqualityModuloFKs $(varP lhs) $(varP rhs) = $(pure body)
  |]

mkBranches :: [Con] -> Q [Match]
mkBranches = fmap catMaybes . traverse mkBranch

mkBranch :: Con -> Q (Maybe Match)
mkBranch (NormalC name components) = do
  binds <- for components $ \(_, ty) -> do
    lhs <- newName "_lhs"
    rhs <- newName "_rhs"
    expanded <- expandSyns ty
    return (expanded, lhs, rhs)
  for (mkComparisons binds) $ \comparisons -> do
    let (_, lhsNames, rhsNames) = unzip3 binds
    pat <- tupP
      [ conP name (map varP lhsNames)
      , conP name (map varP rhsNames)
      ]
    pure $ Match pat comparisons []
mkBranch _ = error "Expected normal constructor for `Unique` data instance constructor"

mkComparisons :: [(Type, Name, Name)] -> Maybe Body
mkComparisons binds =
  NormalB . foldl1 (binApp $ VarE $ mkName "&&") <$> nonEmpty (mapMaybe mkComparison binds)

mkComparison :: (Type, Name, Name) -> Maybe Exp
mkComparison (ty, lhs, rhs)
  | isForeignKey ty = Nothing
  | isNullable ty = pure $ mkNonNullEqComparison lhs rhs
  | otherwise = pure $ mkEqComparison lhs rhs
 where
  -- Only worry about keys and nullable keys
  isForeignKey (AppT (ConT outer) inner) = outer == ''Key || outer == ''Maybe && isForeignKey inner
  isForeignKey _ = False

  -- TODO: arbitrary depth
  isNullable (AppT (ConT outer) _) = outer == ''Maybe
  isNullable _ = False

mkEqComparison :: Name -> Name -> Exp
mkEqComparison lhs rhs =
  binApp
    (VarE $ mkName "==")
    (VarE lhs)
    (VarE rhs)

mkNonNullEqComparison :: Name -> Name -> Exp
mkNonNullEqComparison lhs rhs =
  VarE 'maybe
    `AppE` ConE 'False
    `AppE` VarE 'id
    `AppE` ParensE
      (binApp
        (VarE $ mkName "<*>")
        (binApp
          (VarE $ mkName "<$>")
          (VarE $ mkName "==")
          (VarE lhs))
        (VarE rhs))

binApp :: Exp -> Exp -> Exp -> Exp
binApp f x y = UInfixE x f y
