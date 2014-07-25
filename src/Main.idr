module Main

import System

main : IO ()
main = do
     args <- System.getArgs
     case args of
          [_, name] => do
              putStrLn name
          [self] => putStrLn "Usage: "
