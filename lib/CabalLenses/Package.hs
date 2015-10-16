{-# LANGUAGE TemplateHaskell, TypeFamilies, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-| Lenses for several data types of the 'Distribution.Package' module.
All lenses are named after their field names with a 'L' appended. 

-}
module CabalLenses.Package where
import CabalLenses.TH (makeLensesSuffixed) 

import Distribution.Package (PackageName(..) , PackageIdentifier(..) , Dependency(..))
import Distribution.Version (VersionRange)
import Control.Lens


makeLensesSuffixed ''PackageIdentifier

instance (t ~ PackageName) => Rewrapped PackageName t
instance Wrapped PackageName where
  type Unwrapped PackageName = String
  _Wrapped' = iso getPackageName PackageName
     where
        getPackageName (PackageName n) = n
  {-# INLINE _Wrapped' #-}


packageName :: Lens' Dependency PackageName
packageName = lens getPkgName setPkgName
   where
      getPkgName (Dependency thePackageName _)       = thePackageName
      setPkgName (Dependency _ range) thePackageName = Dependency thePackageName range


versionRange :: Lens' Dependency VersionRange
versionRange = lens getRange setRange
   where
      getRange (Dependency _ range)          = range
      setRange (Dependency thePackageName _) = Dependency thePackageName 
