{-# Language CPP, PatternGuards #-}

module CabalLenses.Utils
   ( findCabalFile
   , findPackageDB
   , findDistDir
   , findNewDistDir
   , symbolicPathListToFilePathList
   ) where

import Control.Monad.Trans.Except (ExceptT, throwE)
import Control.Monad.IO.Class
import Control.Monad (filterM)
import Control.Lens (Iso', iso)
import qualified System.IO.Strict as Strict
import qualified Filesystem.Path.CurrentOS as FP
import Filesystem.Path.CurrentOS ((</>))
import qualified Filesystem as FS
import qualified Data.List as L
import qualified Data.Text as T
import Distribution.Utils.Path (SymbolicPath, getSymbolicPath, unsafeMakeSymbolicPath, PackageDir, SourceDir)

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>))
#endif

type Error = String

io :: MonadIO m => IO a -> m a
io = liftIO

-- | Find a cabal file starting at the given directory, going upwards the directory
--   tree until a cabal file could be found. The returned file path is absolute.
findCabalFile :: FilePath -> ExceptT Error IO FilePath
findCabalFile file = do
   cabalFile <- io $ do
      dir <- absoluteDirectory file
      findCabalFile' dir

   if cabalFile == FP.empty
      then throwE "Couldn't find Cabal file!"
      else return $ FP.encodeString cabalFile

   where
      findCabalFile' dir = do
         files <- filterM FS.isFile =<< (FS.listDirectory dir)
         case L.find isCabalFile files of
              Just file -> return $ dir </> file
              _         -> do
                 let parent = FP.parent dir
                 if parent == dir
                    then return FP.empty
                    else findCabalFile' parent

      isCabalFile file
         | Just ext <- FP.extension file
         = ext == cabalExt

         | otherwise
         = False

      cabalExt = T.pack "cabal"


-- | Find the package database of the cabal sandbox from the given cabal file.
--   The returned file path is absolute.
findPackageDB :: FilePath -> ExceptT Error IO (Maybe FilePath)
findPackageDB cabalFile = do
   cabalDir <- io $ absoluteDirectory cabalFile
   let sandboxConfig = cabalDir </> sandbox_config
   isFile   <- io $ FS.isFile sandboxConfig
   if isFile
      then do
         packageDB <- io $ readPackageDB sandboxConfig
         case packageDB of
              Just db -> return $ Just db
              _       -> throwE $ "Couldn't find field 'package-db: ' in " ++ (show sandboxConfig)
      else
         return Nothing

   where
      -- | reads the 'package-db: ' field from the sandbox config file and returns the value of the field
      readPackageDB :: FP.FilePath -> IO (Maybe FilePath)
      readPackageDB sandboxConfig = do
         lines <- lines <$> Strict.readFile (FP.encodeString sandboxConfig)
         return $ do
            line <- L.find (package_db `L.isPrefixOf`) lines
            L.stripPrefix package_db line

      sandbox_config = FP.decodeString "cabal.sandbox.config"
      package_db     = "package-db: "


-- | Find the dist directory of the cabal build from the given cabal file. For a non sandboxed
--   build it's just the directory 'dist' in the cabal build directory. For a sandboxed build
--   it's the directory 'dist/dist-sandbox-*'. The returned file path is absolute.
findDistDir :: FilePath -> IO (Maybe FilePath)
findDistDir cabalFile = do
   cabalDir   <- absoluteDirectory cabalFile
   let distDir = cabalDir </> FP.decodeString "dist"
   hasDistDir <- FS.isDirectory distDir
   if hasDistDir
      then do
         files <- filterM FS.isDirectory =<< (FS.listDirectory distDir)
         return $ FP.encodeString <$> maybe (Just distDir) Just (L.find isSandboxDistDir files)
      else return Nothing

   where
      isSandboxDistDir file =
         "dist-sandbox-" `L.isPrefixOf` (FP.encodeString . FP.filename $ file)


-- | Find the new style dist directory of the cabal build from the given cabal file.
--   The returned file path is absolute.
findNewDistDir :: FilePath -> IO (Maybe FilePath)
findNewDistDir cabalFile = do
   cabalDir   <- absoluteDirectory cabalFile
   let distDir = cabalDir </> FP.decodeString "dist-newstyle"
   hasDistDir <- FS.isDirectory distDir
   return $ if hasDistDir then Just . FP.encodeString $ distDir else Nothing


absoluteDirectory :: FilePath -> IO FP.FilePath
absoluteDirectory file = do
   absFile <- absoluteFile file
   isDir   <- FS.isDirectory absFile
   if isDir
      then return absFile
      else return . FP.directory $ absFile


absoluteFile :: FilePath -> IO FP.FilePath
absoluteFile = FS.canonicalizePath . FP.decodeString


symbolicPathListToFilePathList :: Iso' [SymbolicPath PackageDir SourceDir] [FilePath]
symbolicPathListToFilePathList = iso toFilePathList fromFilePathList
   where
      toFilePathList :: [SymbolicPath PackageDir SourceDir] -> [FilePath]
      toFilePathList symPathList = map getSymbolicPath symPathList

      fromFilePathList :: [FilePath] -> [SymbolicPath PackageDir SourceDir]
      fromFilePathList strList   = map unsafeMakeSymbolicPath strList
