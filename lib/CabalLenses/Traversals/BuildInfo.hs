{-# LANGUAGE Rank2Types, CPP #-}
{-| 

-}
module CabalLenses.Traversals.BuildInfo
   ( allBuildInfo
   , buildInfo
   , buildInfoIf
   ) where

import CabalLenses.Section (Section(..))
import CabalLenses.Traversals.Internal (traverseData, traverseDataIf, having)
import CabalLenses.CondVars (CondVars)
import CabalLenses.PackageDescription
import Control.Lens
import Distribution.PackageDescription (GenericPackageDescription(GenericPackageDescription), BuildInfo)

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>), (<*>), pure)
#endif

-- | A traversal for all 'BuildInfo' of all 'Section'
allBuildInfo :: Traversal' GenericPackageDescription BuildInfo
allBuildInfo f (GenericPackageDescription descrp flags lib exes tests benchs) =
   GenericPackageDescription <$> pure descrp
                             <*> pure flags
                             <*> (_Just . traverseData . libBuildInfoL) f lib
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

