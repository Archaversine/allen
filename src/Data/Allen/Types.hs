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
                        , toBits
                        , fromBits
                        , fromID
                        ) where  

import Control.Monad.State

import Data.Bits
import Data.List (intercalate)
import Data.Word (Word16)

type IntervalID = Int
type IntervalGraph = [Interval]
type IntervalConstraint = (RelationBits, IntervalID)

data Interval = Interval { intervalID        :: Int 
                         , intervalRelations :: [IntervalConstraint]
                         } 

-- | Show instance for Interval 
-- Ex: Interval 3 (During 1, Contains 2)
instance Show Interval where 
    show (Interval iD rels) = "Interval " <> show iD <> " (" <> rels' <> ")"
        where rels' = intercalate ", " $ map showRel rels
              showRel (r, n) = unwords (map show $ fromBits r) <> " " <> show n

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
toBits :: Relation -> RelationBits
toBits = bit . fromEnum

-- | Convert a bit representation to a list of Relation types.
fromBits :: RelationBits -> [Relation]
fromBits bits = [x | x <- allRelations, bits .&. toBits x /= 0]
