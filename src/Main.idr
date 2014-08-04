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
                  -- Training
                  (Just token, Just [], _) =>
                        Kernel.training "kw2q1es1" 100 Nothing
                  (Just token, Just [turns], _) =>
                        let t = fromMaybe 100 (parseInt turns) in
                        Kernel.training "kw2q1es1" t Nothing
                  (Just token, Just [turns, map], _) =>
                        let t = fromMaybe 100 (parseInt turns) in
                        Kernel.training "kw2q1es1" t (Just map)
                  -- Arena
                  (Just token, Nothing, Just []) =>
                        Kernel.arena "kw2q1es1" 65536
                  (Just token, Nothing, Just [games]) =>
                        let g = fromMaybe 65536 (parseInt games) in
                        Kernel.arena "kw2q1es1" g
                  _ => printUsage
