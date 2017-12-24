{-# LANGUAGE TemplateHaskell #-}

-- |
-- Lenses for several data types of the 'Distribution.Version' module.
-- All lenses are named after their field names with a 'L' appended.

module CabalLenses.Version where

import Distribution.Version
import Control.Lens
import Data.Maybe (fromMaybe)
import Control.Applicative ((<$>))


versionBranchL :: Iso' Version [Int]
versionBranchL = iso versionNumbers mkVersion


intervals :: Iso' VersionRange [VersionInterval]
intervals = iso asVersionIntervals toVersionRange
   where
      toVersionRange intervals =
         fromMaybe anyVersion (fromVersionIntervals <$> mkVersionIntervals intervals)


lowerBound :: Lens' VersionInterval LowerBound
lowerBound = _1


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
upperBound = _2


noLowerBound :: LowerBound
noLowerBound = LowerBound (mkVersion [0]) InclusiveBound
