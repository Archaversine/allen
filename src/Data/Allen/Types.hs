{-# LANGUAGE BinaryLiterals #-}

module Data.Allen.Types ( Interval(..)
                        , Allen
                        , IntervalID
                        , IntervalGraph
                        , IntervalConstraint
                        , Relation(..)
                        , RelationBits
                        , allRelations
                        , allRelationBits
                        , relationSet
                        , relationList
                        , fromID
                        ) where  

import Control.Monad.State

import Data.Bits
import Data.List (intercalate)
import Data.Word (Word16)

import Data.Map (Map)
import qualified Data.Map as Map

type IntervalID = Int
type IntervalGraph = [Interval]
type IntervalConstraint = (RelationBits, IntervalID)

data Interval = Interval { intervalID        :: Int 
                         , intervalRelations :: Map IntervalID RelationBits
                         } 

-- | Show instance for Interval 
-- Ex: Interval 3 (During 1, Contains 2)
instance Show Interval where 
    show (Interval iD rels) = "Interval " <> show iD <> " (" <> rels' <> ")"
        where rels' = intercalate ", " $ map showRel $ Map.toList rels
              showRel (n, r) = unwords (map show $ relationList r) <> " " <> show n

-- | Return the interval given it's ID
-- Panics if ID doesn't exist
fromID :: IntervalID -> Allen Interval 
fromID n = gets (!! n)

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

type RelationBits = Word16

-- | List of all possible relations.
allRelations :: [Relation]
allRelations  = [minBound..]

-- | Bit representation of all possible relations.
allRelationBits :: RelationBits
allRelationBits = 0b0001111111111111

-- | Convert a Relation type to its bit representation.
relationSet :: Relation -> RelationBits
relationSet = bit . fromEnum

-- | Convert a bit representation to a list of Relation types.
relationList :: RelationBits -> [Relation]
relationList bits = [x | x <- allRelations, bits .&. relationSet x /= 0]

