{-# LANGUAGE TupleSections, Rank2Types #-}

module CabalLenses.Traversals.Internal
   ( traverseDataIf
   , traverseData
   , traverseDependencyIf
   , traverseDependency
   ) where

import CabalLenses.CondVars (CondVars)
import qualified CabalLenses.CondVars as CV
import Distribution.Types.CondTree (CondTree(..), CondBranch(..))
import Distribution.PackageDescription (ConfVar)
import Distribution.Package (Dependency(..))
import Control.Lens (Traversal')

type CondTree' a = CondTree ConfVar [Dependency] a


-- | A traversal for all 'condTreeData' of 'CondTree' that match 'CondVars'.
traverseDataIf :: CondVars -> Traversal' (CondTree' dat) dat
traverseDataIf condVars f (CondNode dat constr comps) =
   CondNode <$> f dat
            <*> pure constr
            <*> (traverse . traverseCompIf condVars) f comps
   where
      traverseCompIf condVars f (CondBranch cond ifComp elseComp) =
         CondBranch <$> pure cond <*> ifComp' <*> elseComp'
         where
            ifComp' | condMatches = traverseDataIf condVars f ifComp
                    | otherwise   = pure ifComp

            elseComp' | condMatches = pure elseComp
                      | otherwise   = (traverse . traverseDataIf condVars) f elseComp

            condMatches = CV.eval condVars cond


-- | A traversal for all 'condTreeData' (the if and else branches) of the 'CondTree'.
traverseData :: Traversal' (CondTree' dat) dat
traverseData f (CondNode dat constr comps) =
   CondNode <$> f dat
            <*> pure constr
            <*> (traverse . traverseComp) f comps
   where
      traverseComp f (CondBranch cond ifComp elseComp) =
         CondBranch <$> pure cond
                    <*> traverseData f ifComp
                    <*> (traverse . traverseData) f elseComp


-- | A traversal for all 'condTreeConstraints' of 'CondTree' that match 'CondVars'.
traverseDependencyIf :: CondVars -> Traversal' (CondTree' dat) Dependency
traverseDependencyIf condVars f (CondNode dat constr comps) =
   CondNode <$> pure dat
            <*> traverse f constr
            <*> (traverse . traverseCompIf condVars) f comps
   where
      traverseCompIf condVars f (CondBranch cond ifComp elseComp) =
         CondBranch <$> pure cond <*> ifComp' <*> elseComp'
         where
            ifComp' | condMatches = traverseDependencyIf condVars f ifComp
                    | otherwise   = pure ifComp

            elseComp' | condMatches = pure elseComp
                      | otherwise   = (traverse . traverseDependencyIf condVars) f elseComp

            condMatches = CV.eval condVars cond


-- | A traversal for all 'condTreeConstraints' (the if and else branches) of the 'CondTree'.
traverseDependency :: Traversal' (CondTree' dat) Dependency
traverseDependency f (CondNode dat constr comps) =
   CondNode <$> pure dat
            <*> traverse f constr
            <*> (traverse . traverseComp) f comps
   where
      traverseComp f (CondBranch cond ifComp elseComp) =
         CondBranch <$> pure cond
                    <*> traverseDependency f ifComp
                    <*> (traverse . traverseDependency) f elseComp
