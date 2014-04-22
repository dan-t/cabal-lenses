{-# LANGUAGE TemplateHaskell #-}

-- |
-- Lenses for several data types of the 'Distribution.PackageDescription' module.
-- All lenses are named after their field names with a 'L' appended.

module CabalLenses.PackageDescription where

import Distribution.PackageDescription ( GenericPackageDescription(..)
                                       , PackageDescription(..)
                                       , Library(..)
                                       , Executable(..)
                                       , TestSuite(..)
                                       , Benchmark(..)
                                       , BuildInfo(..)
                                       , CondTree(..)
                                       )
import Control.Lens (makeLensesFor)


makeLensesFor [ ("packageDescription", "packageDescriptionL")
              , ("genPackageFlags"   , "genPackageFlagsL")
              , ("condLibrary"       , "condLibraryL")
              , ("condExecutables"   , "condExecutablesL")
              , ("condTestSuites"    , "condTestSuitesL")
              , ("condBenchmarks"    , "condBenchmarksL")
              ] ''GenericPackageDescription


makeLensesFor [ ("package"       , "packageL")
              , ("license"       , "licenseL")
              , ("licenseFile"   , "licenseFileL")
              , ("copyright"     , "copyrightL")
              , ("maintainer"    , "maintainerL")
              , ("author"        , "authorL")
              , ("stability"     , "stabilityL")
              , ("testedWith"    , "testedWithL")
              , ("homepage"      , "homepageL")
              , ("pkgUrl"        , "pkgUrlL")
              , ("bugReports"    , "bugReports")
              , ("sourceRepos"   , "sourceReposL")
              , ("synopsis"      , "synopsisL")
              , ("description"   , "descriptionL")
              , ("category"      , "categoryL")
              , ("customFieldsPD", "customFieldsPDL")
              , ("buildDepends"  , "buildDependsL")
              , ("specVersionRaw", "specVersionRawL")
              , ("buildType"     , "buildTypeL")
              , ("library"       , "libraryL")
              , ("executables"   , "executablesL")
              , ("testSuites"    , "testSuitesL")
              , ("benchmarks"    , "benchmarksL")
              , ("dataFiles"     , "dataFilesL")
              , ("dataDir"       , "dataDirL")
              , ("extraSrcFiles" , "extraSrcFilesL")
              , ("extraTmpFiles" , "extraTmpFilesL")
              ] ''PackageDescription


makeLensesFor [ ("exposedModules", "exposedModulesL")
              , ("libExposed"    , "libExposedL")
              , ("libBuildInfo"  , "libBuildInfoL")
              ] ''Library


makeLensesFor [ ("exeName"   , "exeNameL")
              , ("modulePath", "modulePathL")
              , ("buildInfo" , "buildInfoL")
              ] ''Executable


makeLensesFor [ ("testName"     , "testNameL")
              , ("testInterface", "testInterfaceL")
              , ("testBuildInfo", "testBuildInfoL")
              , ("testEnabled"  , "testEnabledL")
              ] ''TestSuite


makeLensesFor [ ("benchmarkName", "benchmarkNameL")
              , ("benchmarkInterface", "benchmarkInterfaceL")
              , ("benchmarkBuildInfo", "benchmarkBuildInfoL")
              , ("benchmarkEnabled"  , "benchmarkEnabledL")
              ] ''Benchmark


makeLensesFor [ ("hsSourceDirs"      , "hsSourceDirsL")
              , ("options"           , "optionsL")
              , ("defaultLanguage"   , "defaultLanguageL")
              , ("cppOptions"        , "cppOptionsL")
              , ("cSources"          , "cSourcesL")
              , ("ccOptions"         , "ccOptionsL")
              , ("extraLibDirs"      , "extraLibDirsL")
              , ("extraLibs"         , "extraLibsL")
              , ("ldOptions"         , "ldOptionsL")
              , ("includeDirs"       , "includeDirsL")
              , ("includes"          , "includesL")
              ] ''BuildInfo


makeLensesFor [ ("condTreeData"       , "condTreeDataL")
              , ("condTreeConstraints", "condTreeConstraintsL")
              , ("condTreeComponents" , "condTreeComponentsL")
              ] ''CondTree
