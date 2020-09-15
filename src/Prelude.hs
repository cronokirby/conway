-- This module is the prelude we use throughout the project
-- I like basing things off of the Relude project, and I prefer the <| and |>
-- operators to the standard things
module Prelude
  ( module Relude,
    (|>),
    (<|),
  )
where

import Relude

infixl 1 |>

-- A forward composition operator, that I find prettier than (&)
(|>) :: a -> (a -> b) -> b
(|>) = (&)

infixr 0 <|

-- A backwards composition operator, that I find prettier than ($)
(<|) :: (a -> b) -> a -> b
(<|) = ($)
