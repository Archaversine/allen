module Main (main) where

import Data.Allen

calc :: Allen [IntervalConstraint]
calc = do 
    sleeps <- interval 
    snores <- interval 

    constrain snores During sleeps
    constrain sleeps Starts snores

    return []

main :: IO ()
main = do 
    let (relations, graph) = runAllenState calc 

    mapM_ print graph 

    putStrLn $ "Relations: " <> show relations
