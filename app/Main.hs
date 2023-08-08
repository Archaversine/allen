module Main (main) where

import Data.Allen

data Command = CreateInterval String 
             | AssumeRelation String String RelationBits
             | GetConstraints String String
             | ShowGraph
             | ResetGraph
             | Quit
             | InvalidCommand

parseCommand :: String -> Command 
parseCommand str = case words str of 
    [ "create", name      ] -> CreateInterval name 
    [ "assume", a, rel, b ] -> AssumeRelation a b (bitsFromString rel)
    [ "constraints", a, b ] -> GetConstraints a b 
    [ "graph"             ] -> ShowGraph 
    [ "clear"             ] -> ResetGraph 
    [ "quit"              ] -> Quit 
    [ "exit"              ] -> Quit
    _                       -> InvalidCommand

readCommand :: IO Command 
readCommand = do 
    putStr "Allen > "
    command <- parseCommand <$> getLine

    case command of 
        InvalidCommand -> do 
            putStrLn "Invalid command"
            readCommand
        _ -> return command

calc :: Allen ([Relation], [Relation]) 
calc = do 
    a <- interval 
    b <- interval 

    assume a Precedes b 

    r1 <- fromBits <$> getConstraints a b 
    r2 <- fromBits <$> getConstraints b a 

    return (r1, r2)

main :: IO ()
main = print $ evalAllen calc

