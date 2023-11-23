{-# LANGUAGE TemplateHaskell #-}

-- |
-- Lenses for several data types of the 'Distribution.Version' module.
-- All lenses are named after their field names with a 'L' appended.

module CabalLenses.Version where

import Data.Maybe (fromMaybe)
import Distribution.Version
import Distribution.Types.VersionInterval
import Control.Lens


versionBranchL :: Iso' Version [Int]
versionBranchL = iso versionNumbers mkVersion


intervals :: Iso' VersionRange [VersionInterval]
intervals = iso asVersionIntervals toVersionRange
   where
      toVersionRange []        = anyVersion
      toVersionRange intervals = fromVersionIntervals . fromMaybe noVersion . mkVersionIntervals $ intervals


lowerBound :: Lens' VersionInterval LowerBound
lowerBound = lens getLowerBound setLowerBound
   where
      getLowerBound (VersionInterval lowerBound _)            = lowerBound
      setLowerBound (VersionInterval _ upperBound) lowerBound = VersionInterval lowerBound upperBound


version :: Lens' LowerBound Version
version = lens getVersion setVersion
   where
      getVersion (LowerBound vers _)          = vers
      setVersion (LowerBound _    bound) vers = LowerBound vers bound


bound :: Lens' LowerBound Bound
bound = lens getBound setBound
   where
      getBound (LowerBound _ bound)      = bound
      setBound (LowerBound vers _) bound = LowerBound vers bound


upperBound :: Lens' VersionInterval UpperBound
upperBound = lens getUpperBound setUpperBound
   where
      getUpperBound (VersionInterval _ upperBound)            = upperBound
      setUpperBound (VersionInterval lowerBound _) upperBound = VersionInterval lowerBound upperBound


noLowerBound :: LowerBound
noLowerBound = LowerBound (mkVersion [0]) InclusiveBound
