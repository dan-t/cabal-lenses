{-# LANGUAGE Rank2Types #-}

module CabalLenses.Traversals.Dependency
   ( dependency
   , dependencyIf
   , dependencyToString
   ) where

import CabalLenses.Section (Section(..))
import CabalLenses.Traversals.Internal (traverseDependency, traverseDependencyIf)
import CabalLenses.CondVars (CondVars)
import CabalLenses.PackageDescription
import Control.Lens
import Distribution.PackageDescription (GenericPackageDescription)
import Distribution.Package (Dependency)


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


-- | An iso that converts a 'Dependency' to a 'String' containing the name of the dependency.
dependencyToString :: Iso' Dependency String
dependencyToString = iso toString toDep
   where
      toString (Dependency (PackageName name) _) = name
      toDep name = Dependency (PackageName name) anyVersion


having name = filtered ((== name) . fst) 
