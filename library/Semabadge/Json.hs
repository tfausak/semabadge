module Semabadge.Json
  ( optionsFor
  ) where

import qualified Data.Aeson.Types as Aeson
import qualified Semabadge.List as List

optionsFor :: String -> Aeson.Options
optionsFor prefix =
  Aeson.defaultOptions
    { Aeson.fieldLabelModifier =
        Aeson.camelTo2 '_' . List.unsafeDropPrefix prefix
    }
