module Data.Allen.Relation (inverse) where

import Data.Allen.Types

inverse :: Relation -> Relation
inverse r = snd $ head $ filter ((== r) . fst) zipped
    where zipped = zip [minBound..] (reverse [minBound..])
