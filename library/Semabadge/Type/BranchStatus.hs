{-# LANGUAGE DeriveGeneric #-}

module Semabadge.Type.BranchStatus
  ( BranchStatus(..)
  ) where

import qualified Data.Aeson.Types as Aeson
import qualified Data.Text as Text
import qualified GHC.Generics as Generics
import qualified Semabadge.Json as Json
import qualified Semabadge.Type.Result as Result

data BranchStatus = BranchStatus
  { branchStatusResult :: Result.Result
  , branchStatusBranchName :: Text.Text
  } deriving (Eq, Generics.Generic, Show)

instance Aeson.FromJSON BranchStatus where
  parseJSON = Aeson.genericParseJSON (Json.optionsFor "branchStatus")
