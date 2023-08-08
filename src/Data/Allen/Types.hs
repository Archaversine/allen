-- |
-- Module      : Data.Allen.Types
-- Description : Types for Allen's interval algebra.
-- Maintainer  : Archaversine 
--
-- This module provides types that are used throughout the rest of the library.
-- This includes types for intervals, relations, and the interval graph.
--
-- = Intervals
-- An Interval is a data type that represents a single interval. It contains 
-- an ID of type 'IntervalID' and a map of relations to other intervals of type 
-- Map 'IntervalID' 'RelationBits'.
--
-- An `IntervalID` is essentially the same as an @Int@, but it is helpful to 
-- have a dedicated type synonym to distinguish functions that perform operations 
-- interval IDs.
--
-- = Relations
-- A 'Relation' is a data type that represents a relation between two intervals.
-- It is defined in terms of thirteen constructors, where each constructor 
-- represents one of the thirteen possible relations in Allen's interval algebra.
--
-- The 'RelationBits' is used to represent a set of possible representation.
-- It is synonymous with a @Word16@, and is used to represent a set of possible 
-- relations. Since there are only thirteen different relations, only 13 of the 
-- 16 bits in the @Word16@ are used.
--
-- = Interval Graph
-- An interval graph is a map of 'IntervalID's to 'Interval's. It is used to
-- represent the network of intervals and their relations to each other.
--
-- = Allen Monad
-- The Allen monad is a state monad that is used to keep track of the interval 
-- graph that is being built up during the computation. Since it is a synonym 
-- of the @State@ monad, it is possible to use all of the functions in the 
-- @Control.Monad.State@ module.

module Data.Allen.Types ( Interval(..)
                        , Allen
                        , IntervalID
                        , IntervalGraph
                        , Relation(..)
                        , RelationBits
                        , allRelations
                        , allRelationBits
                        , toBits
                        , fromBits
                        , relationUnion
                        , relationIntersection
                        , relationToChar
                        , fromID
                        ) where  

import Control.Monad.State

import Data.Bits
import Data.List (intercalate, foldl')
import Data.Word (Word16)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

-- | How intervals are uniquely identified.
type IntervalID = Int

-- | This is the main type that is used to represent the network of intervals.
type IntervalGraph = Map IntervalID Interval

-- | An interval is a data type that represents a single interval. It contains 
-- an ID of type 'IntervalID' and a map of relations to other intervals of type 
-- Map 'IntervalID' 'RelationBits'. It should not be directly used in a  
-- computation unless the 'IntervalGraph' is in its final state.
data Interval = Interval { intervalID        :: Int 
                         , intervalRelations :: Map IntervalID RelationBits
                         } 

-- | Ex: Interval 3 (d 1, D 2)
instance Show Interval where 
    show (Interval iD rels) = "Interval " <> show iD <> " (" <> rels' <> ")"
        where rels' = intercalate ", " $ map showRel $ Map.toList rels
              showRel (n, r) | r == allRelationBits = "??? " <> show n
                             | otherwise = map relationToChar (fromBits r) <> " " <> show n

-- | Return the interval given it's ID.
-- Panics if ID doesn't exist.
fromID :: IntervalID -> Allen Interval 
fromID n = gets (Map.! n)

-- | A specific instance of the state monad that is used to keep track of the 
-- 'IntervalGraph' that is being built up during the computation.
type Allen = State IntervalGraph

-- | A type where each constructor represents one of the thirteen relations in 
-- Allen's interval algebra.
data Relation = Precedes      -- ^ In Char form: __p__
              | Meets         -- ^ In Char form: __m__ 
              | Overlaps      -- ^ In Char form: __o__ 
              | FinishedBy    -- ^ In Char form: __F__
              | Contains      -- ^ In Char form: __D__
              | Starts        -- ^ In Char form: __s__
              | Equals        -- ^ In Char form: __e__
              | StartedBy     -- ^ In Char form: __S__
              | During        -- ^ In Char form: __d__ 
              | Finishes      -- ^ In Char form: __f__
              | OverlappedBy  -- ^ In Char form: __O__ 
              | MetBy         -- ^ In Char form: __M__
              | PrecededBy    -- ^ In Char form: __P__
              deriving (Eq, Show, Enum, Bounded)

-- | Convert a relation to its Char representation.
relationToChar :: Relation -> Char 
relationToChar r = case r of 
    Precedes     -> 'p'
    Meets        -> 'm'
    Overlaps     -> 'o'
    FinishedBy   -> 'F'
    Contains     -> 'D'
    Starts       -> 's'
    Equals       -> 'e'
    StartedBy    -> 'S'
    During       -> 'd'
    Finishes     -> 'f'
    OverlappedBy -> 'O'
    MetBy        -> 'M'
    PrecededBy   -> 'P'

-- | A bit representation that acts as a set of possible relations between 
-- intervals.
type RelationBits = Word16

-- | List of all possible relations.
allRelations :: [Relation]
allRelations  = [minBound..]

-- | Bit representation of all possible relations.
allRelationBits :: RelationBits
allRelationBits = relationUnion $ map toBits allRelations

-- | Convert a Relation type to its bit representation.
toBits :: Relation -> RelationBits
toBits = bit . fromEnum

-- | Convert a bit representation to a list of Relation types.
fromBits :: RelationBits -> [Relation]
fromBits bits = [x | x <- allRelations, bits .&. toBits x /= 0]

-- | Calculate the union of a list of relations.
relationUnion :: [RelationBits] -> RelationBits
relationUnion = foldl' (.|.) 0

-- | Calculate the intersection of a list of relations.
relationIntersection :: [RelationBits] -> RelationBits 
relationIntersection = foldl' (.&.) 0

