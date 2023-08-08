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

## Documentation

To view more information for library functions, you can view the documentation 
for this library [here](https://archaversine.github.io/allen/Data-Allen.html).

## Interactive REPL

You can use an interactive REPL to perform calculations from the command line.
Executables are available for both Linux and Windows.

You can download the interactive version here: 

- [Linux](https://github.com/Archaversine/allen/releases/tag/v1.0.1-linux)
- [Windows](https://github.com/Archaversine/allen/releases/tag/v1.0.1-windows)
