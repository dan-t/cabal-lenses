{-# LANGUAGE TemplateHaskell #-}

-- |
-- Lenses for several data types of the 'Distribution.Version' module.
-- All lenses are named after their field names with a 'L' appended.

module CabalLenses.Version where
import CabalLenses.TH (suffixedFields) 

import Distribution.Version
import Control.Lens
import Data.Maybe (fromMaybe)


makeLensesWith suffixedFields ''Version


intervals :: Iso' VersionRange [VersionInterval]
intervals = iso asVersionIntervals toVersionRange
   where
      toVersionRange theIntervals =
         fromMaybe anyVersion (fromVersionIntervals <$> mkVersionIntervals theIntervals)


lowerBound :: Lens' VersionInterval LowerBound
lowerBound = _1


version :: Lens' LowerBound Version
version = lens getVersion setVersion
   where
      getVersion (LowerBound theVersion _)          = theVersion
      setVersion (LowerBound _ theBound) theVersion = LowerBound theVersion theBound 


bound :: Lens' LowerBound Bound
bound = lens getBound setBound
   where
      getBound (LowerBound _ theBound)            = theBound
      setBound (LowerBound theVersion _) theBound = LowerBound theVersion theBound


upperBound :: Lens' VersionInterval UpperBound
upperBound = _2


noLowerBound :: LowerBound
noLowerBound = LowerBound (Version [0] []) InclusiveBound

