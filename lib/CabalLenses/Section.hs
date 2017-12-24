
module CabalLenses.Section
   ( Section(..)
   , allSections
   ) where

import Distribution.PackageDescription (GenericPackageDescription(..))
import Distribution.Types.UnqualComponentName (unUnqualComponentName)

type Name = String

-- | A section of the cabal file.
data Section = Library
             | Executable Name
             | TestSuite Name
             | Benchmark Name
             deriving (Show, Eq)


-- | All sections defined in 'GenericPackageDescription'.
allSections :: GenericPackageDescription -> [Section]
allSections pkgDescr =
   concat [ maybe [] (const [Library]) (condLibrary pkgDescr)
          , map (Executable . unUnqualComponentName . fst) (condExecutables pkgDescr)
          , map (TestSuite . unUnqualComponentName . fst) (condTestSuites pkgDescr)
          , map (Benchmark . unUnqualComponentName . fst) (condBenchmarks pkgDescr)
          ]
