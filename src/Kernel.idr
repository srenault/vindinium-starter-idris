module Kernel

import Model
import Http
import Debug.Trace
import Silly
import Bot

private
move : Input -> Direction -> IO $ Either String Input
move input direction = Http.move (playUrl input) direction

private
steps : Bot -> (Lazy $ IO $ Either String Input) -> IO ()
steps bot nextInput = do
      maybeInput <- nextInput
      case (maybeInput) of
           Right input =>
                let game = game input in
                if(finished game) then log "Game finished"
                else do
                dir <- bot input
                steps bot (move input dir) >>= (\_ => pure ())
           Left error => log $ "Unexpected error: \n" ++ error

public
training : Bot -> String -> Int -> Maybe String -> IO ()
training bot token turns map = do
         let nextInput = Http.training token turns map
         maybeInput <- nextInput
         case maybeInput of
              Right input => do
                _ <- log ("Training game " ++ (viewUrl input))
                _ <- steps bot nextInput
                log ("Finished training game " ++ (viewUrl input))
              Left error => log $ "Unexpected error: \n" ++ error

private
oneGame : Bot -> Lazy $ IO (Either String Input) -> Int -> Int -> IO ()
oneGame bot nextInput games current =
        if(current <= games) then
          do
          _ <- log "Waiting for pairing..."
          maybeInput <- nextInput
          case maybeInput of
               Right input => do
                    _ <- log ("Start arena game " ++ (viewUrl input))
                    _ <- steps bot nextInput
                    _ <- log ("Finished arena game" ++ (viewUrl input))
                    oneGame bot nextInput games (current + 1)
               Left error => log $ "Unexpected error: \n" ++ error
        else
          pure ()

public
arena : Bot -> String -> Int -> IO ()
arena bot token games =
      let nextInput = Http.arena token in
          oneGame bot nextInput games 0
