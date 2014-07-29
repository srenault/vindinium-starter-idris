module Main

import System

import Control.Monad.Identity

import Lightyear.Core
import Lightyear.Combinators
import Lightyear.Text

import Json

record Position : Type where
  Pos
  :  (posX : Int)
  -> (posY : Int)
  -> Position

record Hero : Type where
  He
  :  (heroId        : Int)
  -> (heroName      : String)
  -> (heroUserId    : Maybe String)
  -> (heroElo       : Maybe Int)
  -> (heroPos       : Position)
  -> (heroLife      : Int)
  -> (heroGold      : Int)
  -> (heroMineCount : Int)
  -> (heroSpawnPos  : Position)
  -> (heroCrashed   : Bool)
  -> Hero

record ServerData : Type where
  SD
  :  (serverDataToken   : String)
  -> (serverDataViewUrl : String)
  -> (serverDataPlayUrl : String)
  -> (serverDataHero    : Hero)
  -> ServerData

instance Show ServerData where
  show sd = "{ token: " ++ (serverDataToken sd) ++ ", view: " ++ (serverDataViewUrl sd) ++ ", play: " ++ (serverDataPlayUrl sd) ++ " }"

parseJson : String -> Either String JsonValue
parseJson s = parse jsonToplevelValue s

getVal : JsonValue -> String -> Maybe JsonValue
getVal (JsonObject o) key = SortedMap.lookup key o
getVal _ _ = Nothing

getString : JsonValue -> String -> Maybe String
getString o key = case (getVal o key) of
  Just (JsonString s) => Just s
  _ => Nothing

getInt : JsonValue -> String -> Maybe Int
getInt o key = case (getVal o key) of
  Just (JsonNumber s) => Just s
  _ => Nothing

mapPosition : JsonValue -> Maybe Position
mapServerData o = do
    let getI = getInt o
    x <- getI "posX"
    y <- getI "posY"
    Just $ Pos x y

mapHero : JsonValue -> Maybe ServerData
mapServerData o = do
    let getS = getString o
    token <- getS "token"
    viewUrl <- getS "viewUrl"
    playUrl <- getS "playUrl"
    Just $ SD token viewUrl playUrl

mapServerData : JsonValue -> Maybe ServerData
mapServerData o = do
    let getS = getString o
    token <- getS "token"
    viewUrl <- getS "viewUrl"
    playUrl <- getS "playUrl"
    heroObj <- getVal o "hero"
    hero <- mapHero heroObj
    Just $ SD token viewUrl playUrl hero

parseServerData : String -> Either String ServerData
parseServerData str = do
    json <- parseJson str
    case mapServerData json of
      Just d => Right d
      _ => Left "Can't parse data"

sampleJson : String
sampleJson = """{"game":{"id":"2vbaf1wc","turn":0,"maxTurns":1200,"heroes":[{"id":1,"name":"wolfie","userId":"0hgzoiln","elo":1850,"pos":{"x":1,"y":3},"life":100,"gold":0,"mineCount":0,"spawnPos":{"x":1,"y":3},"crashed":false},{"id":2,"name":"random","pos":{"x":18,"y":3},"life":100,"gold":0,"mineCount":0,"spawnPos":{"x":18,"y":3},"crashed":false},{"id":3,"name":"random","pos":{"x":18,"y":16},"life":100,"gold":0,"mineCount":0,"spawnPos":{"x":18,"y":16},"crashed":false},{"id":4,"name":"random","pos":{"x":1,"y":16},"life":100,"gold":0,"mineCount":0,"spawnPos":{"x":1,"y":16},"crashed":false}],"board":{"size":20,"tiles":"          ####################            ##  @1    ################    @4  ##      []##  ####################  ##[]                ################              $-    ##      ########      ##    $-          ##        ####        ##        ##        $-####        ####$-        ####    ##  ######$-    $-######  ##    ##      ##  ####################  ##                ####################                    ####################                ##  ####################  ##      ##    ##  ######$-    $-######  ##    ####        $-####        ####$-        ##        ##        ####        ##          $-    ##      ########      ##    $-              ################                []##  ####################  ##[]      ##  @2    ################    @3  ##            ####################          "},"finished":false},"hero":{"id":1,"name":"wolfie","userId":"0hgzoiln","elo":1850,"pos":{"x":1,"y":3},"life":100,"gold":0,"mineCount":0,"spawnPos":{"x":1,"y":3},"crashed":false},"token":"b1oy","viewUrl":"http://vindinium.org/2vbaf1wc","playUrl":"http://vindinium.org/api/2vbaf1wc/b1oy/play"}"""

main : IO ()
main = do
     args <- System.getArgs
     case args of
          [_, name] => do
              putStrLn name
          [self] => do
              let serverData = parseServerData sampleJson
              case serverData of
                  Left err => putStrLn $ "error: " ++ err
                  Right sd => putStrLn $ "serverData: " ++ (show sd)
