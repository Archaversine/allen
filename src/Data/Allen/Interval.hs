-- |
-- Module      : Data.Allen.Interval
-- Description : Functions for working with intervals.
-- Maintainer  : Archaversine 
--
-- This module provides functions for working with intervals. Note that almost 
-- all exposed functions only work with interval IDs. This is because the 
-- internal representation of intervals is subject to change, but the IDs will 
-- remain the same no matter what.
-- 
-- = Creating intervals
-- Intervals are created with the 'interval' function, which creates an interval 
-- adds it to the internal network representation, then returns its corresponding 
-- ID. Note that upon creating a new interval, it will have all relations to all 
-- other intervals. This is because the creation of an interval does not provide 
-- any meaningful information about its relations to other intervals.
--
-- Creating two intervals sleeps and snores:
--
-- @ 
-- sleeps <- 'interval' 
-- snores <- 'interval'
-- @
--
-- = Defining Relations Between Intervals
-- There are three main ways to define relations betweek intervals:
--
-- (1) Define a single relation using the 'Relation' type.
-- (2) Define a set of relations using a list of 'Relation' types.
-- (3) Define a set of relations using a 'RelationBits' type.
--
-- == Defining a single relation
-- This is the easiest to do, and is done with the 'assume' function. This 
-- function takes three arguments: the ID of the first interval, the relation 
-- between the two intervals, and the ID of the second interval. 
--
-- Example:
--
-- @ 
-- sleeps <- 'interval' 
-- snores <- 'interval' 
--
-- 'assume' snores 'During' sleeps
-- @
--
-- == Defining a Set of Relations 
-- This is done with the 'assumeSet' function. This function takes three 
-- arguments: the ID of the first interval, a list of relations between the 
-- two intervals, and the ID of the second interval. 
--
-- Example: 
--
-- @ 
-- sleeps <- 'interval' 
-- snores <- 'interval' 
--
-- 'assumeSet' snores ['StartedBy', 'During', 'FinishedBy'] sleeps
-- @
--
-- == Defining a Set of Relations Using Bit Representation
-- This is done with the 'assumeBits' function. This function takes three 
-- arguments: the ID of the first interval, a 'RelationBits' type representing 
-- the relations between the two intervals, and the ID of the second interval. 
-- Generally, this function should not be used directly, but it can be used 
-- to speed up calculations if you already know the bit representation.
--
-- Example: 
--
-- @ 
-- sleeps <- 'interval' 
-- snores <- 'interval' 
--
-- let relations = 'relationUnion' $ map 'toBits' ['StartedBy', 'During', 'FinishedBy']
--
-- 'assumeBits' snores relations sleeps
-- @
--
-- = Getting Constraints
-- The 'getConstraints' function returns a 'RelationBits' type representing the 
-- set of all possible relations between two intervals. This is useful for 
-- determining specific information between two intervals.
--
-- Example: 
--
-- @
-- sleeps <- 'interval' 
-- snores <- 'interval' 
--
-- 'assume' snores 'During' sleeps
--
-- 'fromBits' \<$\> 'getConstraints' snores sleeps
-- @

module Data.Allen.Interval ( interval
                           , intervalCount
                           , fromID
                           , assume
                           , assumeSet
                           , assumeBits
                           , setRelation
                           , getConstraints
                           ) where

import Control.Monad
import Control.Monad.State

import Data.Allen.Types
import Data.Allen.Relation

import Data.Bits

import qualified Data.Map.Strict as Map

-- | Create a new interval. 
-- Returns the interval ID.
interval :: Allen IntervalID 
interval = do
    intervals <- get

    let iD         = Map.size intervals
        iRelations = Map.fromList [(x, allRelationBits) | x <- [0 .. iD - 1]]
        intervals' = Map.map (\x -> setRelation x allRelationBits iD) intervals
        i          = Interval iD iRelations

    put $ Map.insert iD i intervals'
    return iD 

-- | Return the number of intervals that are currently in the graph.
intervalCount :: Allen Int 
intervalCount = gets Map.size

-- | Given two intervals, return a copy of the first interval such that it now 
-- has the specified set of relations to the second interval.
--
-- This has no effect on the second interval or the network representation.
setRelation :: Interval -> RelationBits -> IntervalID -> Interval 
setRelation i1 r i2 = i1 { intervalRelations = relations }
    where relations = Map.insert i2 r $ intervalRelations i1

-- | Define a relation between two intervals. 
assume :: IntervalID -> Relation -> IntervalID -> Allen ()
assume id1 r = assumeBits id1 (toBits r)

-- | Define a set of relations between two intervals.
assumeSet :: IntervalID -> [Relation] -> IntervalID -> Allen ()
assumeSet id1 = assumeBits id1 . relationUnion . map toBits

-- | Define a relation between two intervals using RelationBits.
assumeBits :: IntervalID -> RelationBits -> IntervalID -> Allen ()
assumeBits id1 r id2 = do 
    i1 <- fromID id1 
    i2 <- fromID id2 

    let i1' = setRelation i1 r id2 
        i2' = setRelation i2 (converse r) id1

    modify $ Map.insert id1 i1' . Map.insert id2 i2'
    propogate (id1, id2)

-- | Propogate the relations between two intervals to all other intervals 
-- that are related to either of the two intervals.
propogate :: (IntervalID, IntervalID) -> Allen ()
propogate r = evalStateT propogate' [r]

propogate' :: StateT [(IntervalID, IntervalID)] Allen ()
propogate' = do 
    toDo <- get
    case toDo of 
        [] -> return ()
        ((i, j):_) -> do 
            modify tail -- Remove the first element from the queue
            propogate'' (i, j)
            propogate'' (j, i)
            propogate'

propogate'' :: (IntervalID, IntervalID) -> StateT [(IntervalID, IntervalID)] Allen () 
propogate'' (i, j) = do 
    count <- lift intervalCount

    let range = [k | k <- [0 .. count - 1], k /= i, k /= j]

    forM_ range $ \k -> do 
        constraints <- lift $ compose <$> getConstraints k i <*> getConstraints i j
        nkj         <- lift $ getConstraints k j

        let rkj = nkj .&. constraints 

        -- If rkj is a proper subset of nkj, then add (k, j) to the queue
        when (rkj .|. nkj == nkj && rkj < nkj) $ do 
            modify ((k, j):)

        intervalK <- lift $ fromID k
        lift $ modify $ Map.insert k (setRelation intervalK rkj j)

    forM_ range $ \k -> do 
        constraints <- lift $ compose <$> getConstraints i j <*> getConstraints j k
        nik         <- lift $ getConstraints i k 

        let rik = nik .&. constraints

        -- If rik is a proper subset of nik, then add (i, k) to the queue
        when (rik .|. nik == nik && rik < nik) $ do 
            modify ((i, k):)

        intervalI <- lift $ fromID i
        lift $ modify $ Map.insert i (setRelation intervalI rik k)
   
-- | Return the set of possible constraints/relations between two intervals.
getConstraints :: IntervalID -> IntervalID -> Allen RelationBits
getConstraints id1 id2 = Map.findWithDefault 0 id2 . intervalRelations <$> fromID id1

