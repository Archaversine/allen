{-# LANGUAGE ViewPatterns #-}

module Data.Allen.Relation ( inverse
                           , hasConstraint
                           ) where

import Data.Allen.Types
import Data.Bits

-- | Lookup table for inverse function
inverseLookup :: [(RelationBits, RelationBits)]
inverseLookup = zip bits (reverse bits)
    where bits = map toBits allRelations

inverse :: RelationBits -> RelationBits
inverse r = snd $ head $ filter ((== r) . fst) inverseLookup

hasConstraint :: Relation -> IntervalID -> IntervalID -> Allen Bool
hasConstraint (toBits -> r) (fromID -> a) b = do 
    let match (bits, i) | i == b = (r .&. bits) /= 0 
                        | otherwise = False

    any match . intervalRelations <$> a
