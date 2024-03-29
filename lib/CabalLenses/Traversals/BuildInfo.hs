{-# LANGUAGE Rank2Types #-}

module CabalLenses.Traversals.BuildInfo
   ( allBuildInfo
   , buildInfo
   , buildInfoIf
   ) where

import CabalLenses.Section (Section(..))
import CabalLenses.Traversals.Internal (traverseData, traverseDataIf)
import CabalLenses.CondVars (CondVars)
import CabalLenses.PackageDescription
import Control.Lens
import Distribution.PackageDescription (GenericPackageDescription(GenericPackageDescription), BuildInfo)
import Distribution.Types.UnqualComponentName (unUnqualComponentName)

-- | A traversal for all 'BuildInfo' of all 'Section'
allBuildInfo :: Traversal' GenericPackageDescription BuildInfo
allBuildInfo f (GenericPackageDescription descrp gpdVers flags lib subLibs foreignLibs exes tests benchs) =
   GenericPackageDescription <$> pure descrp
                             <*> pure gpdVers
                             <*> pure flags
                             <*> (_Just . traverseData . libBuildInfoL) f lib
                             <*> pure subLibs
                             <*> pure foreignLibs
                             <*> (traverse . _2 . traverseData . buildInfoL) f exes
                             <*> (traverse . _2 . traverseData . testBuildInfoL) f tests
                             <*> (traverse . _2 . traverseData . benchmarkBuildInfoL) f benchs

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


having name = filtered ((== name) . unUnqualComponentName . fst)
