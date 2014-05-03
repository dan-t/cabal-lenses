{-# LANGUAGE TemplateHaskell #-}

-- |
-- Lenses for several data types of the 'Distribution.Version' module.
-- All lenses are named after their field names with a 'L' appended.

module CabalLenses.Version where

import Distribution.Version (Version(..))
import Control.Lens (makeLensesFor)


makeLensesFor [ ("versionBranch", "versionBranchL")
              , ("versionTags"  , "versionTagsL")
              ] ''Version
