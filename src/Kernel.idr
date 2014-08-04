module Kernel

import Model
import Http
import Debug.Trace
import Silly

move : Input -> Direction -> IO $ Either String Input
move input direction = Http.move (playUrl input) direction

private
steps : (Lazy $ IO $ Either String Input) -> IO ()
steps nextInput = do
      maybeInput <- nextInput
      case (maybeInput) of
           Right input =>
                let game = game input in
                if(finished game) then putStrLn "Game finished"
                else
                   do
                   _ <- steps (move input East)
                   pure ()
           Left error => putStrLn $ "Unexpected error: \n" ++ error

public
training : String -> Int -> Maybe String -> IO ()
training token turns map = do
         let nextInput = Http.training token turns map
         maybeInput <- nextInput
         case maybeInput of
              Right input => do
                _ <- log ("Training game " ++ (viewUrl input))
                _ <- steps nextInput
                putStrLn ("Finished training game " ++ (viewUrl input))
              Left error => putStrLn $ "Unexpected error: \n" ++ error

private
oneGame : Lazy $ IO (Either String Input) -> Int -> Int -> IO ()
oneGame nextInput games current =
        if(current <= games) then
          do
          _ <- putStrLn "Waiting for pairing..."
          maybeInput <- nextInput
          case maybeInput of
               Right input => do
                    _ <- log ("Start arena game " ++ (viewUrl input))
                    _ <- steps nextInput
                    _ <- log ("Finished arena game" ++ (viewUrl input))
                    oneGame nextInput games (current + 1)
               Left error => putStrLn $ "Unexpected error: \n" ++ error
        else
          pure ()

public
arena : String -> Int -> IO ()
arena token games =
      let nextInput = Http.arena token in
          oneGame nextInput games 0
