{-# LANGUAGE TupleSections #-}

module CabalLenses.Traversals.Internal
   ( traverseDataIf
   , traverseData
   , traverseDependencyIf
   , traverseDependency
   ) where

import CabalLenses.CondVars (CondVars)
import qualified CabalLenses.CondVars as CV
import Distribution.PackageDescription (CondTree(..), ConfVar)
import Distribution.Package (Dependency(..))
import Data.Traversable (traverse)
import Control.Applicative (Applicative, pure, (<$>), (<*>))

type CondTree' a = CondTree ConfVar [Dependency] a


-- | A traversal for all 'condTreeData' of 'CondTree' that match 'CondVars'.
traverseDataIf :: Applicative f => CondVars
                                -> (dat -> f dat)
                                -> CondTree' dat
                                -> f (CondTree' dat)
traverseDataIf condVars f (CondNode dat constr comps) =
   CondNode <$> f dat
            <*> pure constr
            <*> (traverse . traverseCompIf condVars) f comps  
   where
      traverseCompIf condVars f (cond, ifComp, elseComp) =
         (,,) <$> pure cond <*> ifComp' <*> elseComp'
         where
            ifComp' | condMatches = traverseDataIf condVars f ifComp
                    | otherwise   = pure ifComp

            elseComp' | condMatches = pure elseComp
                      | otherwise   = (traverse . traverseDataIf condVars) f elseComp

            condMatches = CV.eval condVars cond


-- | A traversal for all 'condTreeData' (the if and else branches) of the 'CondTree'.
traverseData :: Applicative f => (dat -> f dat)
                              -> CondTree' dat
                              -> f (CondTree' dat)
traverseData f (CondNode dat constr comps) =
   CondNode <$> f dat
            <*> pure constr
            <*> (traverse . traverseComp) f comps
   where
      traverseComp f (cond, ifComp, elseComp) =
         (,,) <$> pure cond <*> traverseData f ifComp <*> (traverse . traverseData) f elseComp


-- | A traversal for all 'condTreeConstraints' of 'CondTree' that match 'CondVars'.
traverseDependencyIf :: Applicative f => CondVars
                                      -> (Dependency -> f Dependency)
                                      -> CondTree' dat
                                      -> f (CondTree' dat)
traverseDependencyIf condVars f (CondNode dat constr comps) =
   CondNode <$> pure dat
            <*> traverse f constr
            <*> (traverse . traverseCompIf condVars) f comps  
   where
      traverseCompIf condVars f (cond, ifComp, elseComp) =
         (,,) <$> pure cond <*> ifComp' <*> elseComp'
         where
            ifComp' | condMatches = traverseDependencyIf condVars f ifComp
                    | otherwise   = pure ifComp

            elseComp' | condMatches = pure elseComp
                      | otherwise   = (traverse . traverseDependencyIf condVars) f elseComp

            condMatches = CV.eval condVars cond


-- | A traversal for all 'condTreeConstraints' (the if and else branches) of the 'CondTree'.
traverseDependency :: Applicative f => (Dependency -> f Dependency)
                                    -> CondTree' dat
                                    -> f (CondTree' dat)
traverseDependency f (CondNode dat constr comps) =
   CondNode <$> pure dat
            <*> traverse f constr
            <*> (traverse . traverseComp) f comps
   where
      traverseComp f (cond, ifComp, elseComp) =
         (,,) <$> pure cond <*> traverseDependency f ifComp <*> (traverse . traverseDependency) f elseComp
