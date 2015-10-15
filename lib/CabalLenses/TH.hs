module CabalLenses.TH where

import Control.Lens

import Language.Haskell.TH.Syntax (Name, mkName, nameBase) 


-- | a field called @"fieldName"@ makes a lens called @"fieldNameL"@
suffixedFields :: LensRules
suffixedFields = defaultFieldRules & lensField .~ suffixedNamer "L"

-- | accepts all fields, and suffixes them with the given suffix
suffixedNamer :: String -> Name -> [Name] -> Name -> [DefName]
suffixedNamer suffix _typeName _fieldNames fieldName =
 [TopName (mkName (nameBase fieldName ++ suffix))]

