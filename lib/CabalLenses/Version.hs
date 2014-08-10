{-# LANGUAGE TemplateHaskell #-}

-- |
-- Lenses for several data types of the 'Distribution.Version' module.
-- All lenses are named after their field names with a 'L' appended.

module CabalLenses.Version where

import Distribution.Version
import Control.Lens
import Data.Maybe (fromMaybe)
import Control.Applicative ((<$>))


makeLensesFor [ ("versionBranch", "versionBranchL")
              , ("versionTags"  , "versionTagsL")
              ] ''Version


rangeToIntervals :: Iso' VersionRange [VersionInterval]
rangeToIntervals = iso asVersionIntervals toVersionRange
   where
      toVersionRange intervals =
         fromMaybe anyVersion (fromVersionIntervals <$> mkVersionIntervals intervals)


lowerBound :: Lens' VersionInterval LowerBound
lowerBound = _1


upperBound :: Lens' VersionInterval UpperBound
upperBound = _2


noLowerBound :: LowerBound
noLowerBound = LowerBound (Version [0] []) InclusiveBound
