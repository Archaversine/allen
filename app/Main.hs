module Main (main) where

import Data.Allen

calc :: Allen ()
calc = do 
    sleeps <- interval 
    snores <- interval 

    constrain snores During sleeps

main :: IO ()
main = do 
    let graph = runAllen calc
    mapM_ print graph
