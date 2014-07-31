module Main

import System
import Json
import Model
import Http

sampleJson : String
sampleJson = """{"game":{"id":"2vbaf1wc","turn":0,"maxTurns":1200,"heroes":[{"id":1,"name":"wolfie","userId":"0hgzoiln","elo":1850,"pos":{"x":1,"y":3},"life":100,"gold":0,"mineCount":0,"spawnPos":{"x":1,"y":3},"crashed":false},{"id":2,"name":"random","pos":{"x":18,"y":3},"life":100,"gold":0,"mineCount":0,"spawnPos":{"x":18,"y":3},"crashed":false},{"id":3,"name":"random","pos":{"x":18,"y":16},"life":100,"gold":0,"mineCount":0,"spawnPos":{"x":18,"y":16},"crashed":false},{"id":4,"name":"random","pos":{"x":1,"y":16},"life":100,"gold":0,"mineCount":0,"spawnPos":{"x":1,"y":16},"crashed":false}],"board":{"size":20,"tiles":"          ####################            ##  @1    ################    @4  ##      []##  ####################  ##[]                ################              $-    ##      ########      ##    $-          ##        ####        ##        ##        $-####        ####$-        ####    ##  ######$-    $-######  ##    ##      ##  ####################  ##                ####################                    ####################                ##  ####################  ##      ##    ##  ######$-    $-######  ##    ####        $-####        ####$-        ##        ##        ####        ##          $-    ##      ########      ##    $-              ################                []##  ####################  ##[]      ##  @2    ################    @3  ##            ####################          "},"finished":false},"hero":{"id":1,"name":"wolfie","userId":"0hgzoiln","elo":1850,"pos":{"x":1,"y":3},"life":100,"gold":0,"mineCount":0,"spawnPos":{"x":1,"y":3},"crashed":false},"token":"b1oy","viewUrl":"http://vindinium.org/2vbaf1wc","playUrl":"http://vindinium.org/api/2vbaf1wc/b1oy/play"}"""

main : IO ()
main = do
     args <- System.getArgs
     case args of
          [_, name] => do
              putStrLn name
          [self] => do
              response <- post "/api/training" "key=kw2q1es1"
              case response of
                   Just (headers, body) =>
                        case (parseInput body) of
                             Just parsed => putStrLn $ show parsed
                             Nothing => putStrLn "Unable to read parse JSON"
                   Nothing => putStrLn "Unexpected error!"
