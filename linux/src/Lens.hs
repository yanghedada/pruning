{-# LANGUAGE TemplateHaskell #-}
module Lens where

import Language.Haskell.TH
import Control.Lens

makeLensesL :: Name -> DecsQ
makeLensesL = makeLensesWith $ lensRules
    & lensField .~ \_ _ name -> [TopName (mkName $ nameBase name ++ "L")]
