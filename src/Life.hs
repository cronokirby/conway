module Life where

import qualified Data.HashMap.Strict as HM
import Prelude

-- Represents a single (x, y) coordinate on the grid
type Pos = (Int, Int)

-- Represents a two-dimensional grid.
--
-- We have the width and height for a grid, a default value,
-- followed by a hashmap mapping actual positionss to values
data Grid a = Grid Int Int a (HM.HashMap Pos a)

-- Get the neighbors around a point
--
-- This will be all of the points surrounding one, except for the point itself
neighbors :: Pos -> Grid a -> [a]
neighbors (x, y) (Grid _ _ a mp) =
  let shifts = [-1, 0, 1]
      shifted = filter (/= (x, y)) [(x + dx, x + dy) | dx <- shifts, dy <- shifts]
   in map (\i -> HM.lookupDefault a i mp) shifted

-- Evolve a single step of the automaton given the current state of the grid, and a way of evaluating neighbors
evolve :: Eq a => ([a] -> a -> a) -> Grid a -> Grid a
evolve f grid@(Grid w h a mp) =
  let positions = [(x, y) | x <- [0 .. w - 1], y <- [0 .. h - 1]]
      contents = map (\i -> (i, f (neighbors i grid) (HM.lookupDefault a i mp))) positions
      -- For efficiency, don't store dead cells
      filtered = filter ((/= a) . snd) contents
   in Grid w h a (HM.fromList filtered)

-- Represents the state of the grid in Conway's game of life
data Life = Dead | Alive deriving (Eq)

-- A single step of evolution in conway's game of life
conway :: Grid Life -> Grid Life
conway = evolve (\nbrs a -> go a (length nbrs))
  where
    go :: Life -> Int -> Life
    go Alive 2 = Alive
    go Alive 3 = Alive
    go Dead 3 = Alive
    go _ _ = Dead
