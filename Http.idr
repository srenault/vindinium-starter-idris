module Main

import Network.Socket
import Debug.Trace

private
f : IO (Either Int Socket)
f = socket AF_INET Stream 0

-- SOCKET CREATED
private
f1 : IO (Maybe Socket)
f1 = do
   s <- f
   case s of
        Left err => pure Nothing
        Right sock => pure $ Just sock

-- SOCKET OPENED
private
f2 : Socket -> IO Int
f2 socket = let address = IPv4Addr 91 209 78 59
                port = 80 in
                connect socket address port

-- WRITE
private
f3 : Socket -> String -> IO(Maybe ByteLength)
f3 socket d = do 
  res <- send socket d
  case res of
       Left err => pure Nothing
       Right byteLength => pure $ Just byteLength

-- POST
public
post : Socket -> String -> String -> IO(Maybe ByteLength)
post socket path params =
     let size = show(Strings.length(params)) in
     f3 socket ("POST " ++ path ++ " HTTP/1.1\r\n" ++ "Host: vindinium.org" ++ "\r\nContent-Length: " ++ size ++ "\r\n" ++ "Content-Type: application/x-www-form-urlencoded\r\n" ++ "\r\n" ++ params)

private
fs : Char -> (String, String) -> (String, String)
fs c (prev, acc) =
   let p = if(length(prev) == 4) then strTail (prev ++ singleton(c)) else prev ++ singleton(c) in
   if(prev == "\r\n\r\n") then
        (prev, acc ++ singleton(c))
   else
        (p, acc)

private
fs1 : Char -> (String, String, String) -> (String, String, String)
fs1 c (prev, headers, body) =
   let p = if(length(prev) == 4) then strTail (prev ++ singleton(c)) else prev ++ singleton(c) in
   if(prev == "\r\n\r\n") then
        (prev, headers, body ++ singleton(c))
   else
        (p, headers ++ singleton(c), body)

parseResponse : Socket -> ByteLength -> IO(String, String)
parseResponse socket len = do
    res <- recvFrom socket len
    case res of
      Left err => pure ("", "")
      Right (addr, d, bl) =>
            let chars = List.reverse $ Strings.unpack d
                (_, headers, body) = List.foldrImpl fs1 ("", "", "") id chars in
                pure (headers, body)

private
read : Socket -> ByteLength -> IO ()
read socket len = do
    res <- recvFrom socket len
    case trace "read" res of
      Left err => putStrLn "READ ERROR"
      Right (addr, d, bl) =>
            let chars = List.reverse $ Strings.unpack d
                z = ("", "")
                (n, m) = List.foldrImpl fs z id chars in
                putStrLn $ d

public
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
                             Just _ => do
                                  x <- read socket 10000000
                                  y <- read socket 10000000
                                  putStrLn "OK"
                             Nothing => putStrLn "SEND PAS OK"
                         else
                           putStrLn "FAILED"

