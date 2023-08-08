-- |
-- Module      : Data.Allen.Relation
-- Description : Functions for working with Allen's interval algebra relations.
-- Maintainer  : Archaversine 
--
-- This module provides functions for working with relations. Note that almost 
-- all exposed functions only work with relation bitsets. This is done mainly 
-- to optimize the speed in calculations involving relations. 
--
-- The 'RelationBits' type is a synonym for a 16 bit unsigned integer. Note that 
-- since Allen's interval algebra only defines 13 relations, the remaining 3 bits 
-- are unused. So the bit representation of every possible relation looks like 
-- this: 
--
-- @ 
-- 0b0001111111111111
-- @ 
--
-- Modifying the extra 3 bits will not affect the result of any calculations.
-- To view in exact detail how a `Relation` converted to a bit representation, 
-- see the `toBits` function.

module Data.Allen.Relation ( converse
                           , testRelation
                           , testRelationSet 
                           , testRelationBits
                           , composeSingle
                           , compose
                           , bitsFromString
                           ) where

import Data.Allen.Types
import Data.Bits

import qualified Data.Map.Strict as Map
import qualified Data.Vector.Unboxed as U

-- | Lookup table for converse function.
converseLookup :: [(RelationBits, RelationBits)]
converseLookup = zip bits (reverse bits)
    where bits = map toBits allRelations

-- | Return the converse of a Relation bitset.
converse :: RelationBits -> RelationBits 
converse 0 = 0
converse x = relationUnion $ map func [0 .. fromEnum (maxBound :: Relation)]
    where func i | testBit x i = snd $ head $ filter ((== bit i) . fst) converseLookup
                 | otherwise = 0

-- | Return if a relation exists between two intervals.
testRelation :: Relation -> IntervalID -> IntervalID -> Allen Bool
testRelation r id1 id2 = do 
    relations <- Map.findWithDefault 0 id2 . intervalRelations <$> fromID id1 
    return $ toBits r .&. relations /= 0

-- | Return if all relations in a set exist between two intervals. 
--
-- IF the set of relations between interval @a@ and interval @b@ is @full@, 
-- then the function will always return @True@.
testRelationSet :: [Relation] -> IntervalID -> IntervalID -> Allen Bool 
testRelationSet r = testRelationBits (relationUnion $ map toBits r)

-- | Return if all relations in a set exist between two intervals.
--  
-- If the set of relations between interval @a@ and interval @b@ is @full@, 
-- then the function will always return @True@.
testRelationBits :: RelationBits -> IntervalID -> IntervalID -> Allen Bool
testRelationBits r id1 id2 = do 
    relations <- Map.findWithDefault 0 id2 . intervalRelations <$> fromID id1 
    return $ r .&. relations >= r

-- | Valid Chars: pmoFDseSdfoMP.
relationFromChar :: Char -> Relation
relationFromChar x = case x of 
    'p' -> Precedes 
    'm' -> Meets 
    'o' -> Overlaps 
    'F' -> FinishedBy 
    'D' -> Contains 
    's' -> Starts 
    'e' -> Equals 
    'S' -> StartedBy 
    'd' -> During 
    'f' -> Finishes 
    'O' -> OverlappedBy 
    'M' -> MetBy 
    'P' -> PrecededBy 
    _   -> error $ "relationFromChar: invalid relation " <> [x]


-- | Given a string, return the bit representation of the set of relations.
-- Valid characters: pmoFDseSdfoMP.
--
-- You may also use @full@ to represent all relations, or @concur@ to represent
-- all relations excluding Precedes and PrecededBy.
--
-- Example:
-- 
-- @
-- let x = 'bitsFromString' "pms"    -- [Precedes, Meets, StartedBy]
--     y = 'bitsFromString' "full"   -- [Precedes .. PrecededBy]
--     z = 'bitsFromString' "concur" -- [Overlaps .. OverlappedBy]
-- @
bitsFromString :: String -> RelationBits
bitsFromString x | x == "full"   = rBits allRelations 
                 | x == "concur" = rBits [Overlaps .. OverlappedBy]
                 | otherwise = rBits $ map relationFromChar x
    where rBits = relationUnion . map toBits

-- Table referenced from here: https://www.ics.uci.edu/~alspaugh/cls/shr/allen.html
composeLookup :: U.Vector RelationBits
composeLookup = U.fromList $ map bitsFromString table 
--                |    p   |    m   |   o     |  F     |  D     |  s     |  e |   S    |     d   |    f   |     O   |    M   |    P
-- ---------------+--------+--------+---------+--------+--------+--------+----+--------+---------+--------+---------+--------+-------------
    where table = [     "p",     "p",      "p",     "p",     "p",     "p", "p",     "p",  "pmosd", "pmosd",  "pmosd", "pmosd",  "full" -- p
                  ,     "p",     "p",      "p",     "p",     "p",     "m", "m",     "m",    "osd",   "osd",    "osd",   "Fef", "DSOMP" -- m
                  ,     "p",     "p",    "pmo",   "pmo", "pmoFD",     "o", "o",   "oFD",    "osd",   "osd", "concur",   "DSO", "DSOMP" -- o
                  ,     "p",     "m",      "o",     "F",     "D",     "o", "F",     "D",    "osd",   "Fef",    "DSO",   "DSO", "DSOMP" -- F
                  , "pmoFD",   "oFD",    "oFD",     "D",     "D",   "oFD", "D",     "D", "concur",   "DSO",    "DSO",   "DSO", "DSOMP" -- D
                  ,     "p",     "p",    "pmo",   "pmo", "pmoFD",     "s", "s",   "seS",      "d",     "d",    "dfO",     "M",     "P" -- s
                  ,     "p",     "m",      "o",     "F",     "D",     "s", "e",     "S",      "d",     "f",      "O",     "M",     "P" -- e
                  , "pmoFD",   "oFD",    "oFD",     "D",     "D",   "seS", "S",     "S",    "dfO",     "O",      "O",     "M",     "P" -- S
                  ,     "p",     "p",  "pmosd", "pmosd",  "full",     "d", "d", "dfOMP",      "d",     "d",  "dfOMP",     "P",     "P" -- d
                  ,     "p",     "m",    "osd",   "Fef", "DSOMP",     "d", "f",   "OMP",      "d",     "f",    "OMP",     "P",     "P" -- f
                  , "pmoFD",   "oFD", "concur",   "DSO", "DSOMP",   "dfO", "O",   "OMP",    "dfO",     "O",    "OMP",     "P",     "P" -- O
                  , "pmoFD",   "seS",    "dfO",     "M",     "P",   "dfO", "M",     "P",    "dfO",     "M",      "P",     "P",     "P" -- M
                  ,  "full", "dfOMP", "dfOMOP",     "P",     "P", "dfOMP", "P",     "P",  "dfOMP",     "P",      "P",     "P",     "P" -- P
                  ]

-- | Compose two relations.
--
-- Composition table available at <https://www.ics.uci.edu/~alspaugh/cls/shr/allen.html>.
composeSingle :: Relation -> Relation -> RelationBits 
composeSingle r1 r2 = composeLookup U.! index
    where index = 13 * fromEnum r1 + fromEnum r2

-- | Compose two sets of relations.
--
-- Composition table available at <https://www.ics.uci.edu/~alspaugh/cls/shr/allen.html>.
compose :: RelationBits -> RelationBits -> RelationBits
compose r1 r2 = relationUnion [composeSingle a b | a <- fromBits r1, b <- fromBits r2]
