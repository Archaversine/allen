{-# LANGUAGE ViewPatterns #-}

module Data.Allen.Interval ( interval
                           , fromID
                           , assume
                           , assumeBits
                           , getConstraints
                           ) where

import Control.Monad.State

import Data.Allen.Types
import Data.Allen.Relation
import Data.Bits
import Data.List (partition)

import qualified Data.Vector as V

-- | Create a new interval. 
-- Returns the interval ID
interval :: Allen IntervalID 
interval = do
    intervals <- get

    let n = V.length intervals
        i = Interval n []

    put $ V.snoc intervals i 
    return n 

-- | Add a relation to an interval
-- Ensures no duplicates are created
addRelation :: Interval -> RelationBits -> IntervalID -> Interval 
addRelation i1 r i2 = i1 { intervalRelations = r' : filtered }
    where (existing, filtered) = partition ((== i2) . snd) $ intervalRelations i1
          r' = case existing of 
            []        -> (r, i2)        -- If there are no pre-existing relations
            ((x,_):_) -> (x .|. r, i2)  -- If there ARE pre-existing relations

-- | Define a relation between two intervals. 
assume :: IntervalID -> Relation -> IntervalID -> Allen ()
assume id1 (toBits -> r) = assumeBits id1 r

-- | Define a relation between two intervals using RelationBits.
assumeBits :: IntervalID -> RelationBits -> IntervalID -> Allen ()
assumeBits id1 r id2 = do 
    i1 <- fromID id1 
    i2 <- fromID id2 

    let i1' = addRelation i1 r id2 
        i2' = addRelation i2 (converse r) id1

    modify (V.// [(id1, i1'), (id2, i2')])

getConstraints :: IntervalID -> IntervalID -> Allen RelationBits
getConstraints id1 id2 = do 
    i1 <- fromID id1 

    return $ case filter ((== id2) . snd) $ intervalRelations i1 of 
        []        -> 0 -- If there is NOT a relation
        ((x,_):_) -> x -- If there IS a relation
