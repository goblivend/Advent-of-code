module AOC.Utils (pairs, toTuple) where

import Data.List (tails)

pairs :: [a] -> [(a, a)]
pairs l = [(x, y) | (x : rest) <- tails l, y <- rest]

toTuple :: [a] -> (a, a)
toTuple (a : b : _) = (a, b)
toTuple _ = error "Not enough elements in list to call toTuple"
