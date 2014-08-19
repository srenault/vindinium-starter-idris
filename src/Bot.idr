module Bot

import Effect.Random
import Model
import Silly
import System
import RandC

Bot : Type
Bot = Input -> IO Direction

defaultBot : Bot
defaultBot input = do
           i <- getRandom 0 4
           let dir = index' (cast i) [Stay, North, South, East, West]
           case dir of
                Just d => pure d
                Nothing => pure Stay

inBoard : Board -> Position -> Bool
inBoard board position =
    let s = size board
        x = x position
        y = y position in
    x >= 0 && x < s && y >= 0 && y < s

tileAt : Board -> Position -> Maybe Tile
tileAt board position =
    let x = x position
        y = y position
        idx = cast (y * (size board) + x) in
    if inBoard board position then
       index' idx (tiles board)
    else Nothing
