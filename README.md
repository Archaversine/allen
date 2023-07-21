# Haskell - Allen's Interval Algebra Implementation

An Implementation of Allen's Interval Algebra in Haskell.

## Allen Monad 

Temporal reasoning calculations may be performed inside the Allen Monad, which 
is a State Monad accompanied with helper functions.

```haskell 
type Allen = State IntervalGraph
```

To get the resulting IntervalGraph, you may use the `runAllen` function, which 
is similar to the `runState` function but instead begins with an empty 
intervalGraph and discards the resulting value. If you want the resuling value 
still, there is a `runAllenState` which does just that.

## Intervals 

Intervals can be created inside the Allen Monad using the `interval` 
function:

```haskell 
calc :: Allen ()
calc = do 
    sleeps <- interval 
    snores <- interval 

    return ()
```

Both `sleeps` and `snores` will have unique IDs to distinguish themselves 
from each other. ()

## Relations

Intervals can have relations with one another. For example, in the above
example a valid relation would be: `snores during sleeps`.

To view a complete list of all possible relations: 

```haskell 
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
```

To compute the inverse of a relation, you may use the `inverse` function:

```haskell 
main :: IO ()
main = do 
    let r  = During 
        r' = inverse r

    print r  -- Prints "During"
    print r' -- Prints "Contains"
```

To compose two relations together, you may use the `compose` function. Note  
that the compose function returns the `RelationBits` type, not the `Relation`
type.

```haskell 
main :: IO ()
main = do 
    let a  = Contains 
        b  = Overlaps 
        c  = a `compose` b
        c' = fromBits c

    print c  -- Prints "28" (bit representation)
    print c' -- Prints "[Overlaps,FinishedBy,Contains]"
```

## Constraints

To specify relations to two intervals, or a constraint, you may use the 
`assume` function:

```haskell 
calc :: Allen ()
calc = do 
    sleeps <- interval 
    snores <- interval 

    assume snores During sleeps

main :: IO ()
main = do 
    let graph = runAllen calc 

    mapM_ print graph
```

This prints the following:

```
Interval 0 (Contains 1)
Interval 1 (During 0)
```
