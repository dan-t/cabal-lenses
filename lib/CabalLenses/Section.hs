{-| 

-}
module CabalLenses.Section
   ( Section(..)
   , allSections
   ) where

import Distribution.PackageDescription (GenericPackageDescription(..))

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
          , map (Executable . fst) (condExecutables pkgDescr)
          , map (TestSuite . fst) (condTestSuites pkgDescr)
          , map (Benchmark . fst) (condBenchmarks pkgDescr)
          ]
