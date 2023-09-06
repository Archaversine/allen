{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad
import Control.Monad.State

import Data.Aeson
import Data.Allen
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Text (Text, pack, unpack)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.ByteString as BS

import GHC.Generics (Generic)

import System.IO 
import System.Environment

data RelationJSON = RelationJSON { relationFrom :: Int
                                 , relationTo   :: Int
                                 , relChars     :: Text
                                 } deriving Generic

data NetworkJSON = NetworkJSON { ivCount   :: Int
                               , relations :: [RelationJSON]
                               }

data IntervalJSON = IntervalJSON { ivID   :: Int
                                 , ivRels :: [RelationJSON]
                                 } deriving Generic

newtype ResultJSON = ResultJSON { result :: [IntervalJSON] } deriving Generic

instance ToJSON RelationJSON where 
    toJSON (RelationJSON a b r) = object [ "from" .= a, "to" .= b, "relation" .= r ]
    
instance ToJSON IntervalJSON where 
    toJSON (IntervalJSON i r) = object [ "id" .= i, "relations" .= r ]

instance ToJSON ResultJSON where 
    toJSON (ResultJSON r) = object [ "intervals" .= r ]

instance FromJSON RelationJSON where 
    parseJSON = withObject "RelationJSON" $ \v -> RelationJSON 
        <$> v .: "from"
        <*> v .: "to"
        <*> v .: "relation"

instance FromJSON NetworkJSON where 
    parseJSON = withObject "NetworkJSON" $ \v -> NetworkJSON 
        <$> v .: "intervals" 
        <*> v .: "relations"

data REPLState = REPLState { graph :: Allen (), intervalNames :: Map String IntervalID }
type REPL = StateT REPLState IO

newREPLState :: REPLState 
newREPLState = REPLState (return ()) Map.empty

withIntervals :: String -> String -> (IntervalID -> IntervalID -> REPL ()) -> REPL ()
withIntervals a b func = do 
    names <- gets intervalNames

    case (Map.lookup a names, Map.lookup b names) of 
        (Just aID, Just bID) -> func aID bID
        (Nothing, _)         -> liftIO $ putStrLn $ "Interval " <> a <> " does not exist"
        (_, Nothing)         -> liftIO $ putStrLn $ "Interval " <> b <> " does not exist"

createInterval :: String -> REPL ()
createInterval name = do 
    REPLState calc names <- get

    case Map.lookup name names of 
        Just _  -> error $ "Interval " ++ name ++ " already exists"
        Nothing -> do 
            let newID = Map.size names
            put $ REPLState (void $ calc >> interval) (Map.insert name newID names)

assumeRelation :: String -> RelationBits -> String -> REPL ()
assumeRelation a r b = do 
    REPLState calc names <- get

    withIntervals a b $ \aID bID -> do 
        put $ REPLState (calc >> assumeBits aID r bID) names

showConstraints :: String -> String -> REPL ()
showConstraints a b = do 
    calc <- gets graph 

    withIntervals a b $ \aID bID -> do 
        let constraints = fromBits $ evalAllen $ calc >> getConstraints aID bID
        liftIO $ putStrLn $ a <> " --(" <> map relationToChar constraints <> ")--> " <> b

showGraph :: REPL ()
showGraph = do 
    REPLState calc names <- get

    let sorted = sortBy (comparing fst) $ Map.toList names

    liftIO $ mapM_ printPair sorted
    liftIO $ putStrLn "---------------------------------------------"
    liftIO $ mapM_ (print . snd) $ Map.toList $ execAllen calc
        where printPair (name, iD) = putStrLn $ show iD <> ": " <> name

resetGraph :: REPL ()
resetGraph = put newREPLState

data Command = CreateInterval String 
             | AssumeRelation String RelationBits String
             | GetConstraints String String
             | ShowGraph
             | ResetGraph
             | Help
             | Quit
             | InvalidCommand

parseCommand :: String -> Command 
parseCommand str = case words str of 
    [ "create", name      ] -> CreateInterval name 
    [ "assume", a, rel, b ] -> AssumeRelation a (bitsFromString rel) b
    [ "constraints", a, b ] -> GetConstraints a b 
    [ "graph"             ] -> ShowGraph 
    [ "clear"             ] -> ResetGraph 
    [ "help"              ] -> Help
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

printHelp :: IO ()
printHelp = do 
    putStrLn "Commands:"
    putStrLn "  create <name>           Create a new interval with the given name"
    putStrLn "  assume <a> <rel> <b>    Assume that a and b have the given relation"
    putStrLn "                          (Use: pmoFDseSdfoMP to represent relations)"
    putStrLn "  constraints <a> <b>     Show the constraints between a and b"
    putStrLn "  graph                   Show the current graph"
    putStrLn "  clear                   Clear the current graph"
    putStrLn "  help                    Show this help message"
    putStrLn "  exit                    Exit the program"

repl :: REPL () 
repl = do 
    command <- liftIO readCommand

    case command of 
        CreateInterval name  -> createInterval name  >> repl
        AssumeRelation a r b -> assumeRelation a r b >> repl
        GetConstraints a b   -> showConstraints a b  >> repl
        ShowGraph            -> showGraph            >> repl
        ResetGraph           -> resetGraph           >> repl
        Help                 -> liftIO printHelp     >> repl
        Quit                 -> return ()
        InvalidCommand       -> do 
            liftIO $ putStrLn "Error: Invalid Command"
            repl

addRelationJSON :: RelationJSON -> Allen ()
addRelationJSON rel = assumeBits a r b
    where a = relationFrom rel
          b = relationTo   rel
          r = bitsFromString $ unpack $ relChars rel

jsonToGraph :: NetworkJSON -> IntervalGraph
jsonToGraph network = execAllen $ do 
    replicateM_ (ivCount network) interval
    mapM_ addRelationJSON (relations network)

toResultJSON :: IntervalGraph -> ResultJSON
toResultJSON = ResultJSON . map toIntervalJSON . Map.toList
    where toIntervalJSON (iD, iv  ) = IntervalJSON iD (map toRelationJSON $ Map.toList $ intervalRelations iv)
          toRelationJSON (iD, bits) = RelationJSON iD (iD + 1) (pack $ map relationToChar $ fromBits bits)

calcJSON :: NetworkJSON -> ResultJSON 
calcJSON = toResultJSON . jsonToGraph

startRepl :: IO ()
startRepl = do 
    putStrLn "Interactive Allen's Interval Algebra Solver"
    putStrLn "Author: Archaversine"
    putStrLn "Type 'help' for a list of commands"
    putStrLn "------------------------------------------"
    evalStateT repl newREPLState

main :: IO ()
main = do 
    -- Fix buffering so that we can see the prompt
    hSetBuffering stdout NoBuffering

    args <- getArgs

    case args of 
        [        ] -> startRepl
        [from, to] -> do 
            contents <- BS.readFile from

            let  decoded = eitherDecode (BS.fromStrict contents) :: Either String NetworkJSON
            case decoded of 
                Left  m -> errorWithoutStackTrace m
                Right j -> BS.writeFile to (BS.toStrict $ encode $ calcJSON j)

        _ -> do 
            putStrLn "Usage: allen [input] [output]"
            putStrLn "       allen"

            errorWithoutStackTrace "Invalid Arguments."

