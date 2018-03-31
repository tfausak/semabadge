module Semabadge.Type.Branch
  ( Branch
  , makeBranch
  , unwrapBranch
  ) where

newtype Branch =
  Branch String
  deriving (Eq, Show)

makeBranch :: String -> Branch
makeBranch branch = Branch branch

unwrapBranch :: Branch -> String
unwrapBranch (Branch branch) = branch
