module CabalLenses.TH where

import Control.Lens

import Language.Haskell.TH (DecsQ, Name, mkName, nameBase) 


{-| @makeLensesSuffixed = 'makeLensesWith' 'suffixedFields'@ 

-}
makeLensesSuffixed :: Name -> DecsQ
makeLensesSuffixed = makeLensesWith suffixedFields 

-- | a field called @"field"@ makes a lens called @"fieldL"@
suffixedFields :: LensRules
suffixedFields = defaultFieldRules & lensField .~ suffixedNamer "L"

-- | accepts all fields, and suffixes them with the given suffix
suffixedNamer :: String -> Name -> [Name] -> Name -> [DefName]
suffixedNamer suffix _typeName _fieldNames fieldName =
 [TopName (mkName (nameBase fieldName ++ suffix))]

