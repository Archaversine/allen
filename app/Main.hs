module Main (main) where

import Data.Allen

calc :: Allen ()
calc = do 
    sleeps <- interval 
    snores <- interval 

    assume snores During sleeps
    assume sleeps Starts snores

main :: IO ()
main = mapM_ print $ runAllen calc

