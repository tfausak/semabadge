module Semabadge.Lens
  ( set
  ) where

import qualified Data.Functor.Identity as Identity

set :: ((a -> Identity.Identity b) -> s -> Identity.Identity t) -> b -> s -> t
set l b s = Identity.runIdentity (l (\_ -> Identity.Identity b) s)
