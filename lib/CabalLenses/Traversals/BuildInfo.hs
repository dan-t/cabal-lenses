{-# LANGUAGE Rank2Types #-}

module CabalLenses.Traversals.BuildInfo
   ( buildInfo
   , buildInfoIf
   ) where

import CabalLenses.Section (Section(..))
import CabalLenses.Traversals.Internal (traverseData, traverseDataIf)
import CabalLenses.CondVars (CondVars)
import CabalLenses.PackageDescription
import Control.Lens
import Distribution.PackageDescription (GenericPackageDescription, BuildInfo)


-- | A traversal for all 'BuildInfo' of 'Section'.
buildInfo :: Section -> Traversal' GenericPackageDescription BuildInfo
buildInfo Library           = condLibraryL . _Just . traverseData . libBuildInfoL
buildInfo (Executable name) = condExecutablesL . traverse . having name . _2 . traverseData . buildInfoL
buildInfo (TestSuite name)  = condTestSuitesL . traverse . having name . _2 . traverseData . testBuildInfoL
buildInfo (Benchmark name)  = condBenchmarksL . traverse . having name . _2 . traverseData . benchmarkBuildInfoL


-- | A traversal for the 'BuildInfo' of 'Section' that match 'CondVars'.
buildInfoIf :: CondVars -> Section -> Traversal' GenericPackageDescription BuildInfo
buildInfoIf condVars Library           = condLibraryL . _Just . traverseDataIf condVars . libBuildInfoL
buildInfoIf condVars (Executable name) = condExecutablesL . traverse . having name . _2 . traverseDataIf condVars . buildInfoL
buildInfoIf condVars (TestSuite name)  = condTestSuitesL . traverse . having name . _2 . traverseDataIf condVars . testBuildInfoL
buildInfoIf condVars (Benchmark name)  = condBenchmarksL . traverse . having name . _2 . traverseDataIf condVars . benchmarkBuildInfoL


having name = filtered ((== name) . fst) 
