module Main

import Data.SortedMap
import System
import Kernel
import Bot

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
printUsage = putStrLn ("USAGE\r\n" ++
                      "vindinium -token abcd -training\r\n" ++
                      "vindinium -token abcd -training <turn>\r\n" ++
                      "vindinium -token abcd -training <turns> <map>\r\n" ++
                      "vindinium -token abcd -training <turns> <map>\r\n" ++
                      "vindinium -token abcd -arena\r\n" ++
                      "vindinium -token abcd -arena <games>")

main : IO ()
main = do
     args <- System.getArgs
     case (parseArgs args) of
       parsedArgs =>
         let token = (lookup "token" parsedArgs) >>= head'
             trainingMode = lookup "training" parsedArgs
             arenaMode = lookup "arena" parsedArgs
             defaultTurns: Int = 100
             defaultGames: Int = 65536 in
             case (token, trainingMode, arenaMode) of
                  -- Training
                  (Just token, Just [], _) =>
                        Kernel.training token defaultTurns Nothing
                  (Just token, Just [turns], _) =>
                        let t = fromMaybe defaultTurns (parseInt turns) in
                        Kernel.training token t Nothing
                  (Just token, Just [turns, map], _) =>
                        let t = fromMaybe defaultTurns (parseInt turns) in
                        Kernel.training token t (Just map)
                  -- Arena
                  (Just token, Nothing, Just []) =>
                        Kernel.arena token defaultGames
                  (Just token, Nothing, Just [games]) =>
                        let g = fromMaybe defaultGames (parseInt games) in
                        Kernel.arena token g
                  _ => printUsage
