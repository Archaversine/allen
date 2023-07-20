module Main (main) where

import Data.Allen

calc :: Allen ()
calc = do 
    sleeps <- interval 
    snores <- interval 

    constrain snores During sleeps

main :: IO ()
main = mapM_ print $ runAllen calc
