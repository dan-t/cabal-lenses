{-# LANGUAGE Rank2Types #-}

module CabalLenses.Traversals.Dependency
   ( allDependency
   , allDependencyIf
   , dependency
   , dependencyIf
   ) where

import CabalLenses.Section (Section(..))
import CabalLenses.Traversals.Internal (traverseDependency, traverseDependencyIf, having)
import CabalLenses.CondVars (CondVars)
import CabalLenses.PackageDescription
import Control.Lens
import Distribution.PackageDescription (GenericPackageDescription(GenericPackageDescription))
import Distribution.Package (Dependency)


-- | A traversal for all 'Dependency' of all 'Section'.
allDependency :: Traversal' GenericPackageDescription Dependency
allDependency f (GenericPackageDescription descrp flags lib exes tests benchs) =
   GenericPackageDescription <$> pure descrp
                             <*> pure flags
                             <*> (_Just . traverseDependency) f lib
                             <*> (traverse . _2 . traverseDependency) f exes
                             <*> (traverse . _2 . traverseDependency) f tests
                             <*> (traverse . _2 . traverseDependency) f benchs


-- | A traversal for all 'Dependency' of all 'Section' that match 'CondVars'.
allDependencyIf :: CondVars -> Traversal' GenericPackageDescription Dependency
allDependencyIf condVars f (GenericPackageDescription descrp flags lib exes tests benchs) =
   GenericPackageDescription <$> pure descrp
                             <*> pure flags
                             <*> (_Just . traverseDependencyIf condVars) f lib
                             <*> (traverse . _2 . traverseDependencyIf condVars) f exes
                             <*> (traverse . _2 . traverseDependencyIf condVars) f tests
                             <*> (traverse . _2 . traverseDependencyIf condVars) f benchs


-- | A traversal for all 'Dependency' of 'Section'.
dependency :: Section -> Traversal' GenericPackageDescription Dependency
dependency Library           = condLibraryL . _Just . traverseDependency
dependency (Executable name) = condExecutablesL . traverse . having name . _2 . traverseDependency
dependency (TestSuite name)  = condTestSuitesL . traverse . having name . _2 . traverseDependency
dependency (Benchmark name)  = condBenchmarksL . traverse . having name . _2 . traverseDependency


-- | A traversal for the 'Dependency' of 'Section' that match 'CondVars'.
dependencyIf :: CondVars -> Section -> Traversal' GenericPackageDescription Dependency
dependencyIf condVars Library           = condLibraryL . _Just . traverseDependencyIf condVars
dependencyIf condVars (Executable name) = condExecutablesL . traverse . having name . _2 . traverseDependencyIf condVars
dependencyIf condVars (TestSuite name)  = condTestSuitesL . traverse . having name . _2 . traverseDependencyIf condVars
dependencyIf condVars (Benchmark name)  = condBenchmarksL . traverse . having name . _2 . traverseDependencyIf condVars

