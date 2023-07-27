module Data.Allen.IntervalGraph (updateGraph, modifyList) where

import Control.Monad.State (modify)

import Data.Allen.Types

updateGraph :: [(Int, Interval)] -> Allen ()
updateGraph values = modify $ flip modifyList values

modifyList :: [a] -> [(Int, a)] -> [a]
modifyList []  _ = []
modifyList xs [] = xs
modifyList xs ((index, y):ys) = modifyList newList ys
    where (left, right)       = splitAt index xs
          newList             = left <> [y] <> tail right
