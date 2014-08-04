module Kernel

import Model
import Http

move : Input -> Direction -> IO $ Maybe Input
move input direction = Http.move (playUrl input) direction

private
steps : (Lazy $ IO $ Maybe Input) -> IO ()
steps nextInput = do
      maybeInput <- nextInput
      case (maybeInput) of
           Just input =>
                let game = game input in
                if(finished game) then putStrLn "Game finished"
                else
                   do
                   _ <- steps (move input East)
                   pure ()
           _ => putStrLn "Unexpected error"

public
training : String -> Int -> Maybe String -> IO ()
training token turns map = do
         let nextInput = Http.training token turns map
         maybeInput <- nextInput
         case maybeInput of
              Just input => do
                _ <- putStrLn ("Training game " ++ (viewUrl input))
                _ <- steps nextInput
                putStrLn ("Finished training game " ++ (viewUrl input))
              Nothing => putStrLn "Unexpected error"

private
oneGame : Lazy $ IO (Maybe Input) -> Int -> Int -> IO ()
oneGame nextInput games current =
        if(current <= games) then
          do
          _ <- putStrLn "Waiting for pairing..."
          maybeInput <- nextInput
          case maybeInput of
               Just input => do
                    _ <- putStrLn ("Start arena game " ++ (viewUrl input))
                    _ <- steps nextInput
                    _ <- putStrLn ("Finished arena game" ++ (viewUrl input))
                    oneGame nextInput games (current + 1)
               Nothing => putStrLn ""
        else
          pure ()

public
arena : String -> Int -> IO ()
arena token games =
      let nextInput = Http.arena token in
          oneGame nextInput games 0
