module Model

import Json
import Silly
import Debug.Trace

-- Direction

data Direction = Stay
               | North
               | South
               | East
               | West

instance Show Direction where
  show Stay = "Stay"
  show North = "North"
  show South = "South"
  show East = "East"
  show West = "West"

-- Position

record Position : Type where
  MkPosition
  :  (x : Int)
  -> (y : Int)
  -> Position

parsePosition : JsonValue -> Maybe Position
parsePosition o = do
    let getI = getInt o
    x <- getI "x"
    y <- getI "y"
    Just $ MkPosition x y

instance Show Position where
  show position = "{ x: " ++ show (x position) ++ ", y: " ++ show (y position) ++ " }"

-- Hero

record Hero : Type where
  MkHero
  :  (userId    : Int)
  -> (name      : String)
  -> (pos       : Position)
  -> (life      : Int)
  -> (gold      : Int)
  -> (mineCount : Int)
  -> (spawnPos  : Position)
  -> (crashed   : Bool)
  -> Hero

instance Show Hero where
  show hero = "{ id: " ++ show (userId hero) ++ ", name: " ++ (name hero) ++ ", pos: " ++ show (pos hero) ++ ", life: " ++ show (life hero) ++ ", gold: " ++ show (gold hero)  ++ ", mineCount: " ++ show (mineCount hero)  ++ ", spawnPos: " ++ show (spawnPos hero)  ++ ", crashed: " ++ show (crashed hero)  ++ " }"

parseHero : JsonValue -> Maybe Hero
parseHero o =
    let getS = getString o
        getI = getInt o
        getB = getBool o in
    do
    id <- getI "id"
    name <- getS "name"
    posObj <- getVal o "pos"
    pos <- parsePosition posObj
    life <- getI "life"
    gold <- getI "gold"
    mineCount <- getI "mineCount"
    spawnPosObj <- getVal o "spawnPos"
    spawnPos <- parsePosition spawnPosObj
    crashed <- getB "crashed"
    Just $ MkHero id name pos life gold mineCount spawnPos crashed
-- Tile

data Tile = AirTile
          | WallTile
          | TavernTile
          | MineTile (Maybe Int)
          | HeroTile Int

instance Show Tile where
  show AirTile = "  "
  show WallTile = "##"
  show TavernTile = "[]"
  show (MineTile $ Just n) = "$" ++ show n
  show (MineTile Nothing) = "$ "
  show (HeroTile n) = "@" ++ show n

parseTile : List Char -> Maybe Tile
parseTile tile = case tile of
          [' ', ' '] => Just AirTile
          ['#', '#'] => Just WallTile
          ['[', ']'] => Just TavernTile
          ['$', x] => Just $ MineTile $ charToInt x
          ['@', x] => case charToInt x of
                           Just n => Just $ HeroTile n
                           _ => Nothing
          _ => Nothing

foldTiles : Char -> List $ List Char -> List $ List Char
foldTiles c ((h :: Nil) :: xs) = ((c :: h :: Nil) :: xs)
foldTiles c acc = (c :: Nil) :: acc

groupTiles : String -> List $ List Char
groupTiles str =
           let chars = reverse $ unpack str in
           List.foldrImpl foldTiles [] id chars

parseTiles : String -> List Tile
parseTiles tiles =
           let grouped = groupTiles tiles in
           mapMaybe (\t => parseTile t) grouped

-- Board

record Board : Type where
  MkBoard
  : (size: Int)
  -> (tiles: List Tile)
  -> Board

instance Show Board where
  show board = "{ size: " ++ show (size board) ++ ", titles: " ++ show (tiles board) ++ " }"

parseBoard : JsonValue -> Maybe Board
parseBoard o =
    let getI = getInt o
        getS = getString o in
    do
    size <- getI "size"
    tilesStr <- getS "tiles"
    tiles <- Just $ parseTiles tilesStr
    Just $ MkBoard size tiles

-- Game

record Game : Type where
  MkGame
  : (gameId : String)
  -> (turn : Int)
  -> (maxTurns : Int)
  -> (heroes : List Hero)
  -> (board : Board)
  -> (finished : Bool)
  -> Game

instance Show Game where
  show game = "{ id: " ++ show (gameId game) ++ ", turn: " ++ show (turn game) ++ ", maxTurns: " ++ show (maxTurns game) ++ ", heroes: " ++ show (heroes game) ++ ", board: " ++ show (board game) ++ ", finished: " ++ show (finished game) ++ " }"

parseGame : JsonValue -> Maybe Game
parseGame o =
    let getS = getString o
        getI = getInt o
        getB = getBool o
        getA = getArray o in
    do
    id <- getS "id"
    turn <- getI "turn"
    maxTurns <- getI "maxTurns"
    heroesArray <- getA "heroes"
    heroes <- Just $ mapMaybe (parseHero) heroesArray
    boardObj <- getVal o "board"
    board <- parseBoard boardObj
    finished <- getB "finished"
    Just $ MkGame id turn maxTurns heroes board finished

-- Input

record Input : Type where
  MkInput
  : (game: Game)
  -> (hero : Hero)
  -> (token : String)
  -> (viewUrl : String)
  -> (playUrl : String)
  -> Input

instance Show Input where
  show input = "{ game: " ++ (show (game input)) ++ ", token: " ++ (token input) ++ ", viewUrl: " ++ (viewUrl input) ++ ", playUrl: " ++ (playUrl input) ++ ", hero: " ++ (show (hero input)) ++ " }"

parseInput : String -> Maybe Input
parseInput str =
    case (parseJson str) of
         Right o => do
              getS <- Just $ getString o
              gameObj <- getVal o "game"
              game <- parseGame gameObj
              token <- getS "token"
              viewUrl <- getS "viewUrl"
              playUrl <- getS "playUrl"
              heroObj <- getVal o "hero"
              hero <- parseHero heroObj
              Just $ MkInput game hero token viewUrl playUrl
         Left _  => Nothing
