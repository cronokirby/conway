module Life (Pos, Grid, Life (..), positions, conway, emptyLife, singleLife) where

import qualified Data.HashMap.Strict as HM
import Prelude

-- Represents a single (x, y) coordinate on the grid
type Pos = (Int, Int)

-- Represents a two-dimensional grid.
--
-- We have the width and height for a grid, a default value,
-- followed by a hashmap mapping actual positionss to values
data Grid a = Grid Int Int a (HM.HashMap Pos a) deriving (Show)

instance Semigroup a => Semigroup (Grid a) where
  (Grid w1 h1 dead1 mp1) <> (Grid w2 h2 dead2 mp2) = Grid (max w1 w2) (max h1 h2) (dead1 <> dead2) (mp1 <> mp2)

instance Monoid a => Monoid (Grid a) where
  mempty = Grid 0 0 mempty mempty

-- Get all of the indexed positions in the grid
positions :: Grid a -> [(Pos, a)]
positions (Grid w h a mp) =
  let is = [(x, y) | x <- [0 .. w - 1], y <- [0 .. h - 1]]
   in map (\i -> (i, HM.lookupDefault a i mp)) is

-- Get the neighbors around a point
--
-- This will be all of the points surrounding one, except for the point itself
neighbors :: Pos -> Grid a -> [a]
neighbors (x, y) (Grid _ _ a mp) =
  let shifts = [-1, 0, 1]
      shifted = filter (/= (x, y)) [(x + dx, y + dy) | dx <- shifts, dy <- shifts]
   in map (\i -> HM.lookupDefault a i mp) shifted

-- Evolve a single step of the automaton given the current state of the grid, and a way of evaluating neighbors
evolve :: Eq a => ([a] -> a -> a) -> Grid a -> Grid a
evolve f grid@(Grid w h a _) =
  let contents = map (\(i, x) -> (i, f (neighbors i grid) x)) (positions grid)
      filtered = filter ((/= a) . snd) contents
   in Grid w h a (HM.fromList filtered)

-- Represents the state of the grid in Conway's game of life
data Life = Dead | Alive deriving (Eq, Show)

instance Semigroup Life where
  Dead <> x = x
  Alive <> _ = Alive

instance Monoid Life where
  mempty = Alive

-- A single step of evolution in conway's game of life
conway :: Grid Life -> Grid Life
conway = evolve (\nbrs a -> go a (length (filter (== Alive) nbrs)))
  where
    go :: Life -> Int -> Life
    go Alive 2 = Alive
    go Alive 3 = Alive
    go Dead 3 = Alive
    go Dead _ = Dead
    go _ _ = Dead

-- An empty starting grid for the game of life
emptyLife :: Int -> Int -> Grid Life
emptyLife w h = Grid w h Dead mempty

-- Make a single position in the grid turn to alive
singleLife :: Pos -> Grid Life
singleLife pos@(x, y) = Grid x y Dead (HM.singleton pos Alive)
