module Main

import System

import Control.Monad.Identity

import Lightyear.Core
import Lightyear.Combinators
import Lightyear.Text

import Data.Text

import Json

record State : Type where
  St
  :  (stateToken : Text)
  -> (stateViewUrl : Text)
  -> (statePlayUrl : Text)
  -> State

parseJson : String -> IO ()
parseJson s = case parse jsonToplevelValue s of
  Left err => putStrLn $ "error: " ++ err
  Right v => print v

main : IO ()
main = do
     args <- System.getArgs
     case args of
          [_, name] => do
              putStrLn name
          [self] => do
              putStrLn "Parsing JSON!"
