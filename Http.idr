module Main

import Network.Socket
import Debug.Trace

private
httpConnect : SocketAddress -> IO(Maybe Socket)
httpConnect address = do
        eventuallySocket <- socket AF_INET Stream 0
        case eventuallySocket of
             Left _ => pure Nothing
             Right sock => do
                    res <- connect sock address 80
                    if(res == 0) then pure (Just sock) else pure Nothing

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

private
parseResponse : Socket -> ByteLength -> Bool -> IO(Maybe String, Maybe String)
parseResponse socket len headersRead = do
    res <- recvFrom socket len
    case res of
      Left err => pure (Nothing, Nothing)
      Right (addr, d, bl) =>
            let chars = List.reverse $ Strings.unpack d
                (_, headers, body) = List.foldrImpl fs1 ("", "", "") id chars in
                pure (Just headers, Just body)

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
     maybeSocket <- httpConnect $ IPv4Addr 91 209 78 59
     case maybeSocket of
          Just sock => do
               maybeBytesSent <- post sock "/api/training" "key=kw2q1es1"
               case maybeBytesSent of
                    Just _ => do
                         x <- read sock 10000000
                         y <- read sock 10000000
                         putStrLn "OK"
                    Nothing => putStrLn "SEND FAILED"
          Nothing => putStrLn "SOCKET FAILED"

