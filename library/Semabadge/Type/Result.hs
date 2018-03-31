module Semabadge.Type.Result
  ( Result(..)
  ) where

import qualified Data.Aeson.Types as Aeson
import qualified Data.Text as Text

data Result
  = ResultFailed
  | ResultPassed
  | ResultPending
  deriving (Eq, Show)

instance Aeson.FromJSON Result where
  parseJSON value =
    Aeson.withText
      "Result"
      (\text ->
         case Text.unpack text of
           "failed" -> pure ResultFailed
           "passed" -> pure ResultPassed
           "pending" -> pure ResultPending
           _ -> Aeson.typeMismatch "Result" value)
      value
