module Semabadge.Type.Project
  ( Project
  , makeProject
  , unwrapProject
  ) where

newtype Project =
  Project String
  deriving (Eq, Show)

makeProject :: String -> Project
makeProject project = Project project

unwrapProject :: Project -> String
unwrapProject (Project project) = project
