{-# LANGUAGE TemplateHaskell, CPP #-}

-- |
-- Lenses for several data types of the 'Distribution.PackageDescription' module.
-- All lenses are named after their field names with a 'L' appended.

module CabalLenses.PackageDescription where
import CabalLenses.TH (makeLensesSuffixed) 

import Distribution.ModuleName (ModuleName) 
import Distribution.PackageDescription ( GenericPackageDescription(..)
                                       , PackageDescription(..)
                                       , Library(..)
                                       , Executable(..)
                                       , TestSuite(..)
                                       , Benchmark(..)
                                       , BuildInfo(..)
                                       , CondTree(..)
                                       )
import Control.Lens(Traversal',each) 

makeLensesSuffixed ''GenericPackageDescription

makeLensesSuffixed ''PackageDescription

makeLensesSuffixed ''Library

makeLensesSuffixed ''Executable

makeLensesSuffixed ''TestSuite

makeLensesSuffixed ''Benchmark

makeLensesSuffixed ''BuildInfo

makeLensesSuffixed ''CondTree

-- | a traversal into the library stanza 
packageLibraryL :: Traversal' PackageDescription Library
packageLibraryL = libraryL.each

-- | a traversal into the @exposes-modules@ field of the library stanza 
packageExposedModulesL :: Traversal' PackageDescription ModuleName
packageExposedModulesL = packageLibraryL.exposedModulesL.each

-- | a traversal into the @hs-sources@ field of the library stanza 
packageHsSourcesDirsL :: Traversal' PackageDescription FilePath
packageHsSourcesDirsL = packageLibraryL.libBuildInfoL.hsSourceDirsL.each

