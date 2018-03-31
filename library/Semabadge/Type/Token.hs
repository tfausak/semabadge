module Semabadge.Type.Token
  ( Token
  , makeToken
  , unwrapToken
  ) where

newtype Token =
  Token String
  deriving (Eq, Show)

makeToken :: String -> Token
makeToken token = Token token

unwrapToken :: Token -> String
unwrapToken (Token token) = token
