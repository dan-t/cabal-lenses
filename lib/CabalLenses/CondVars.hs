{-# Language TemplateHaskell, PatternGuards #-}

module CabalLenses.CondVars
   ( CondVars(..)
   , fromDefaults
   , enableFlag
   , disableFlag
   , eval
   ) where

import qualified Distribution.PackageDescription as PD
import Distribution.PackageDescription (Condition(..))
import qualified Distribution.System as S
import Distribution.System (OS(..), Arch(..))
import Distribution.Compiler (CompilerFlavor(..), buildCompilerFlavor)
import Distribution.Version (Version(..), withinRange)
import qualified Data.HashMap.Strict as HM
import Control.Lens

type FlagName = String
type FlagMap  = HM.HashMap FlagName Bool


-- | The variables that are used to resolve the conditionals inside of the cabal file.
--   Holds the enable state of the cabal flags, the used OS, ARCH, CompilerFlavor and
--   compiler version.
data CondVars = CondVars
   { flags           :: FlagMap          -- ^ the enable state of the flags, initialized with the default flag values in the cabal file
   , os              :: OS               -- ^ the used OS, by default the one cabal was build on
   , arch            :: Arch             -- ^ the used ARCH, by default the one cabal was build on
   , compilerFlavor  :: CompilerFlavor   -- ^ the used CompilerFlavor, by default the one cabal was build on
   , compilerVersion :: Maybe Version    -- ^ the user specified compiler version
   } deriving (Show)


makeLensesFor [ ("flags", "flagsL")
              ] ''CondVars


-- | Create a 'CondVars' from the default flags of the cabal package description.
--   The 'os', 'arch' and 'compilerFlavor' fields are initialized by the ones the cabal library was build on.
fromDefaults :: PD.GenericPackageDescription -> CondVars
fromDefaults pkgDescrp = CondVars { flags           = flags
                                  , os              = S.buildOS
                                  , arch            = S.buildArch
                                  , compilerFlavor  = buildCompilerFlavor
                                  , compilerVersion = Nothing
                                  }
   where
      flags = HM.fromList $ map nameWithDflt (PD.genPackageFlags pkgDescrp)

      nameWithDflt PD.MkFlag { PD.flagName = PD.FlagName name, PD.flagDefault = dflt } =
         (name, dflt)


-- | Enable the given flag in 'CondVars'.
enableFlag :: FlagName -> CondVars -> CondVars
enableFlag flag condVars =
   condVars & flagsL %~ HM.insert flag True


-- | Disable the given flag in 'CondVars'.
disableFlag :: FlagName -> CondVars -> CondVars
disableFlag flag condVars =
   condVars & flagsL %~ HM.insert flag False


-- | Evaluate the 'Condition' using the 'CondVars'.
eval :: CondVars -> Condition PD.ConfVar -> Bool
eval condVars = eval'
   where
      eval' (Var var)    = hasVar var
      eval' (Lit val)    = val
      eval' (CNot c)     = not $ eval' c
      eval' (COr c1 c2)  = eval' c1 || eval' c2
      eval' (CAnd c1 c2) = eval' c1 && eval' c2

      hasVar (PD.OS osVar)     = osVar == os condVars
      hasVar (PD.Arch archVar) = archVar == arch condVars
      hasVar (PD.Impl cflavor vrange)
         | Just version <- compilerVersion condVars
         = cflavor == compilerFlavor condVars && version `withinRange` vrange

         | otherwise
         = cflavor == compilerFlavor condVars

      hasVar (PD.Flag (PD.FlagName name))
         | Just v <- HM.lookup name (flags condVars)
         = v

         | otherwise
         = False
