# Haskell - Allen's Interval Algebra Implementation

An Implementation of Allen's Interval Algebra in Haskell.

This library provides a monadic way to perform computations related to Allen's 
interval algebra. The interval network strucutre is implicitly updated upon 
the creation of a new interval and when a set of relations is applied to two 
intervals. 

## Sources

This library is based off of the interval algebra described in
[Maintaining Knowledge about Temporal Intervals](https://cse.unl.edu/~choueiry/Documents/Allen-CACM1983.pdf), 
and [Allen's Interval Algebra](https://www.ics.uci.edu/~alspaugh/cls/shr/allen.html).

## Examples

### A Simple Example

Assume a situation with three intervals:

1. I am walking.
2. I am talking.
3. My friend calls me.

and assume that we know the following:

- When I am walking, I am not talking.
- When my friend called me, I started talking.

we can easily compute the relations between when I was walking and when my friend called me:

```haskell 
calc :: Allen [Relation]
calc = do 
    walking <- interval 
    talking <- interval 
    friend  <- interval

    assumeSet walking [Precedes, Meets, MetBy, PrecededBy] talking
    assume friend Starts talking

    relations <- getConstraints walking friend

    return (fromBits relations)

main :: IO ()
main = print $ evalAllen calc
```

And this gives the result:

```
[Precedes,Meets,PrecededBy]
```

Which means that we can deduce that walking either happens before, directly 
before, or after my friend calls.

### A Complex Example

Consider the following sentence:

*John was not in the room when I touched the switch to turn on the light.*
 
From this sentence we can derive three intervals ***R***, ***S***, and ***L*** where
***R*** is the time that John was in the room, ***S*** is the time where the light 
switched was touched, and ***L*** the time where the light was on. 

From the sentence, we know at least the following:

- ***S*** overlaps *or* meets ***L**
- ***S*** is before, meets, is met by, or is after ***R***.

To represent this as a reusable network, the following code can be written:

```haskell 
network :: Allen (IntervalID, IntervalID, IntervalID)
network = do 
    r <- interval 
    s <- interval 
    l <- interval 

    assumeSet s [Overlaps, Meets] l
    assumeSet s [Precedes, Meets, MetBy, PrecededBy] r

    return (r, s, l)
```

If we wanted to learn the possible relations between r and l the following code 
can be used (NOTE that `evalAllen` is used to actually evaluate the calculation):

```haskell 
relationsRL :: [Relation]
relationsRL = evalAllen $ do 
    -- Use the previously constructed network
    -- `s` is discarded since it is not used
    (r, _, l) <- network

    -- `getConstraints` returns the bitset of relations
    fromBits <$> getConstraints r l
```

Running the above code, we get the following result:

```
[Precedes,Starts,Equals,StartedBy,During,Finishes,OverlappedBy,MetBy,PrecededBy]
```

Assume that at some point we learn the following extra information:

***L*** overlaps, starts, or is during ***R***.

To calculate the updated relations between ***L*** and ***R*** and between 
***S*** and ***R*** the following code can be used.

```haskell 
updatedRelations :: ([Relation], [Relation])
updatedRelations = evalAllen $ do 
    (r, s, l) <- network

    assumeSet l [Starts, Overlaps, During] r

    lrRelations <- fromBits <$> getConstraints l r 
    srRelations <- fromBits <$> getConstraints s r

    return (lrRelations, srRelations)
```

This would provide the result:

```
([Overlaps,Starts],[Precedes,Meets])
```

## Documentation

To view more information for library functions, you can view the documentation 
for this library [here](https://archaversine.github.io/allen/Data-Allen.html).

## Interactive REPL

You can use an interactive REPL to perform calculations from the command line.
Executables are available for both Linux and Windows.

You can download the interactive version here: 

- [Linux](https://github.com/Archaversine/allen/releases/tag/v1.0.1-linux)
- [Windows](https://github.com/Archaversine/allen/releases/tag/v1.0.1-windows)
