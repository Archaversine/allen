-- |
-- Module      : Data.Allen.Types
-- Description : Types for Allen's interval algebra.
-- Maintainer  : Archaversine 
--

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
                        , fromID
                        ) where  

import Control.Monad.State

import Data.Bits
import Data.List (intercalate, foldl')
import Data.Word (Word16)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

type IntervalID = Int
type IntervalGraph = Map IntervalID Interval

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

type Allen = State IntervalGraph

data Relation = Precedes 
              | Meets 
              | Overlaps 
              | FinishedBy
              | Contains 
              | Starts 
              | Equals 
              | StartedBy 
              | During 
              | Finishes 
              | OverlappedBy 
              | MetBy
              | PrecededBy
              deriving (Eq, Show, Enum, Bounded)

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

