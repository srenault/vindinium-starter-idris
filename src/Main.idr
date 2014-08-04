module Main

import Data.SortedMap
import System
import Json
import Model
import Http
import Vindinium
import Kernel
import Debug.Trace

ParsedArgs : Type
ParsedArgs = SortedMap String (List String)

foldArgs : String -> (Maybe String, ParsedArgs) -> (Maybe String, ParsedArgs)
foldArgs arg acc =
         let (maybeLast, parsed) = acc in
             if(strHead arg == '-') then
               (Just (strTail arg), insert (strTail arg) [] parsed)
             else
               case maybeLast of
                    Just last =>
                         case (lookup last parsed) of
                              Just values => (maybeLast, insert last (arg :: values) parsed)
                              Nothing => (maybeLast, parsed)
                    Nothing => (maybeLast, parsed)

parseArgs : List String -> ParsedArgs
parseArgs args =
          let (_, parsed) = List.foldrImpl foldArgs (Nothing, empty) id (reverse args) in
          parsed

printUsage : IO ()
printUsage = putStrLn "USAGE"

main : IO ()
main = do
     args <- System.getArgs
     case (parseArgs args) of
       parsedArgs =>
         let token = lookup "token" parsedArgs
             trainingMode = lookup "training" parsedArgs
             arenaMode = lookup "arena" parsedArgs in
             case (token, trainingMode, arenaMode) of
                  (Just token, Just [], _) => Kernel.training "kw2q1es1" 100 Nothing
                  (Just token, Just [turns], _) => putStrLn "training with turns"
                  (Just token, Just [turns, map], _) => putStrLn "training with turns and map"
                  (Just token, Nothing, Just []) => putStrLn "arena with default value"
                  (Just token, Nothing, Just [games]) => putStrLn ("arena with " ++ (show games))
                  _ => printUsage



     -- response <- post "/api/training" "key=kw2q1es1"
     -- case response of
     --      Just (headers, body) =>
     --           case (parseInput body) of
     --                Just parsed => putStrLn $ show parsed
     --                Nothing => putStrLn "Unable to read parse JSON"
     --      Nothing => putStrLn "Unexpected error!"
