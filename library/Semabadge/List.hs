module Semabadge.List
  ( dropPrefix
  , unsafeDropPrefix
  ) where

import qualified Data.Maybe as Maybe

dropPrefix :: Eq a => [a] -> [a] -> Maybe [a]
dropPrefix prefix list =
  case prefix of
    [] -> Just list
    ph:pt ->
      case list of
        [] -> Nothing
        lh:lt ->
          if lh == ph
            then dropPrefix pt lt
            else Nothing

unsafeDropPrefix :: (Eq a, Show a) => [a] -> [a] -> [a]
unsafeDropPrefix prefix list =
  Maybe.fromMaybe
    (error (unwords [show prefix, "is not a prefix of", show list]))
    (dropPrefix prefix list)
