{-# LANGUAGE TemplateHaskell #-}

-- |
-- Lenses for several data types of the 'Distribution.Package' module.
-- All lenses are named after their field names with a 'L' appended.

module CabalLenses.Package where

import Distribution.Package (PackageName(..) , PackageIdentifier(..) , Dependency(..))
import Distribution.Version (VersionRange)
import Control.Lens (makeLensesFor, Lens', lens)


pkgNameString :: Lens' PackageName String
pkgNameString = lens getString setString
   where
      getString (PackageName str) = str
      setString _                 = PackageName


makeLensesFor [ ("pkgName"   , "pkgNameL")
              , ("pkgVersion", "pkgVersionL")
              ] ''PackageIdentifier


depPackageName :: Lens' Dependency PackageName
depPackageName = lens getPkgName setPkgName
   where
      getPkgName (Dependency pkgName _)          = pkgName
      setPkgName (Dependency _ range) newPkgName = Dependency newPkgName range


depVersionRange :: Lens' Dependency VersionRange
depVersionRange = lens getRange setRange
   where
      getRange (Dependency _ range)   = range
      setRange (Dependency pkgName _) = Dependency pkgName
