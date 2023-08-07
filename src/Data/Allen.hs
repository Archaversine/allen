{-|
 - Module      : Data.Allen 
 - Description : Main module for Allen's interval algebra.
 - Maintainer  : Archaversine 
 -}

-- | This module provides a monad for computing with Allen's interval algebra. 
--  The monad keeps track of the interval graph that is being built up during 
--  the computation. The interval graph is represented as a map from interval 
--  identifiers to intervals. 
--
--  = Intervals
--  Intervals can be created using the 'interval' function:
--
--  @ 
--  calc :: Allen ()
--  calc = do 
--    sleeps <- interval
--    snores <- interval
--    wakeup <- interval
--    ...
--  @ 
--
-- == Retrieving interval data
-- Most functions perform operations on intervals solely with the use of their 
-- IDs. However, sometimes it is useful to retrieve the actual interval data. 
-- To get the actual interval data, use the 'fromID' function:
--
-- @ 
-- calc :: Allen () 
-- calc = do 
--   sleeps         <- interval 
--   sleepsInterval <- fromID sleeps
--   ...
-- @
--
-- Note that in the above example, updating the interval @sleeps@ will not 
-- update the interval @sleepsInterval@.
--
-- == Combining calculations
-- Sometimes, it is useful to define a set of intervals in one place and use 
-- then repeatedly in other places. Here is an example that reuses the intervals 
-- @a@ and @b@:
--
-- @ 
-- network :: Allen (IntervalID, IntervalID)
-- network = do 
--   a <- interval 
--   b <- interval 
--
--   assume a During b 
--
--   return (a, b)
--
-- calc1 :: Allen () 
-- calc1 = do 
--   (a, b) <- network 
--   c      <- interval
--
--   assume a Precedes c
--   ...
--
-- calc2 :: Allen ()
-- calc2 = do 
--   (a, b) <- network 
--   c      <- interval 
--
--   assume a Contains c
--   ...
-- @
--
--  = Relations 
-- Intervals can have relations with one another. For example, in the above 
-- example a valid relation would be that one sleeps during snores. Adding  
-- relations is done using one of the assume functions:
--
-- @ 
-- calc :: Allen () 
-- calc = do 
--    sleeps <- interval
--    snores <- interval
--    wakeup <- interval
--
--    assume snores During sleeps
--    assume wakeup PrecededBy sleeps
-- @
--
-- Sometimes, intervals have more than one possible relation with one another.
-- For example, snores is During sleeps, but it could also by StartedBy sleeps, 
-- or it could be Equals sleeps. In such cases, the @assumeSet@ function can be 
-- used: 
--
-- @  
-- calc :: Allen () 
-- calc = do 
--    sleeps <- interval
--    snores <- interval
--    wakeup <- interval
--
--    assumeSet snores [During, StartedBy, Equals] sleeps
-- @
--
-- There are thirteen different relations intervals can have with each other. 
-- They are identified with the @Relation@ type:
--
-- @ 
-- data Relation = Precedes 
--               | Meets 
--               | Overlaps 
--               | FinishedBy
--               | Contains 
--               | Starts 
--               | Equals 
--               | StartedBy 
--               | During 
--               | Finishes 
--               | OverlappedBy 
--               | MetBy
--               | PrecededBy
--               deriving (Eq, Show, Enum, Bounded)
-- @

module Data.Allen ( module Data.Allen.Types 
                  , module Data.Allen.Interval
                  , module Data.Allen.Relation
                  , execAllen
                  , evalAllen
                  , runAllen
                  ) where

import Control.Monad.State

import Data.Allen.Types
import Data.Allen.Interval
import Data.Allen.Relation

import qualified Data.Map.Strict as Map

-- | Return the resulting interval graph of an allen computation
execAllen :: Allen a -> IntervalGraph
execAllen = snd . runAllen

-- | Return the result of an allen computation
evalAllen :: Allen a -> a
evalAllen = fst . runAllen

-- | Return the result of an allen computation and the resulting interval graph
runAllen :: Allen a -> (a, IntervalGraph)
runAllen = flip runState Map.empty
