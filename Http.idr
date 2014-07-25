module Main

import Network.Socket

f : IO (Either Int Socket)
f = socket AF_INET Stream 0

-- SOCKET CREATED
f1 : IO (Maybe Socket)
f1 = do
   s <- f
   case s of
        Left err => pure Nothing
        Right sock => pure $ Just sock

-- SOCKET OPENED
f2 : Socket -> IO Int
f2 socket = let address = IPv4Addr 91 209 78 59
                port = 80 in
                connect socket address port

-- WRITE
f3 : Socket -> String -> IO(Maybe ByteLength)
f3 socket d = do 
  res <- send socket d
  case res of
       Left err => pure Nothing
       Right byteLength => pure $ Just byteLength

-- POST
post : Socket -> String -> String -> IO(Maybe ByteLength)
post socket path params =
     let size = show(Strings.length(params)) in
     f3 socket ("POST " ++ path ++ " HTTP/1.1\r\n" ++ "Host: vindinium.org" ++ "\r\nContent-Length: " ++ size ++ "\r\n" ++ "Content-Type: application/x-www-form-urlencoded\r\n" ++ "\r\n" ++ params)

read : Socket -> ByteLength -> IO ()
read socket len = do
    res <- recvFrom socket len
    case res of
      Left err => putStrLn "READ ERROR"
      Right (addr, d, bl) => putStrLn d

main : IO ()
main = do
     x <- f1
     case x of
          Nothing => putStrLn "Nothing"
          Just socket => do
                         res <- f2 socket
                         if(res == 0) then
                           do
                           r <- post socket "/api/training" "key=kw2q1es1"
                           case r of
                             Just bl => read socket 100000
                             Nothing => putStrLn ("SEND PAS OK")
                         else
                           putStrLn "FAILED"

