module Semabadge.Badge
  ( badgeColor
  , badgeForBranch
  , badgeForServer
  , badgeRightLabel
  ) where

import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Text as Text
import qualified Graphics.Badge.Barrier as Barrier
import qualified Semabadge.Lens as Lens
import qualified Semabadge.Type.BranchStatus as BranchStatus
import qualified Semabadge.Type.Result as Result
import qualified Semabadge.Type.ServerStatus as ServerStatus

badgeColor :: Result.Result -> Barrier.Color
badgeColor result =
  case result of
    Result.ResultFailed -> Barrier.red
    Result.ResultPassed -> Barrier.brightgreen
    Result.ResultPending -> Barrier.gray

badgeForBranch ::
     BranchStatus.BranchStatus -> Maybe String -> LazyByteString.ByteString
badgeForBranch branchStatus maybeLabel =
  Barrier.renderBadge
    (Lens.set
       Barrier.right
       (badgeColor (BranchStatus.branchStatusResult branchStatus))
       Barrier.flat)
    (maybe
       (BranchStatus.branchStatusBranchName branchStatus)
       Text.pack
       maybeLabel)
    (badgeRightLabel (BranchStatus.branchStatusResult branchStatus))

badgeForServer ::
     ServerStatus.ServerStatus -> Maybe String -> LazyByteString.ByteString
badgeForServer serverStatus maybeLabel =
  Barrier.renderBadge
    (Lens.set
       Barrier.right
       (badgeColor (ServerStatus.serverStatusResult serverStatus))
       Barrier.flat)
    (maybe
       (ServerStatus.serverStatusServerName serverStatus)
       Text.pack
       maybeLabel)
    (badgeRightLabel (ServerStatus.serverStatusResult serverStatus))

badgeRightLabel :: Result.Result -> Text.Text
badgeRightLabel result =
  Text.pack
    (case result of
       Result.ResultFailed -> "failed"
       Result.ResultPassed -> "passed"
       Result.ResultPending -> "pending")
