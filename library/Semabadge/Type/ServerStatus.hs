{-# LANGUAGE DeriveGeneric #-}

module Semabadge.Type.ServerStatus
  ( ServerStatus(..)
  ) where

import qualified Data.Aeson.Types as Aeson
import qualified Data.Text as Text
import qualified GHC.Generics as Generics
import qualified Semabadge.Json as Json
import qualified Semabadge.Type.Result as Result

data ServerStatus = ServerStatus
  { serverStatusResult :: Result.Result
  , serverStatusServerName :: Text.Text
  } deriving (Eq, Generics.Generic, Show)

instance Aeson.FromJSON ServerStatus where
  parseJSON = Aeson.genericParseJSON (Json.optionsFor "serverStatus")
