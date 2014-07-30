module Http

import Network.Socket
import Debug.Trace
import Silly

private
foldHeaders : String -> Maybe Int -> Maybe Int
foldHeaders header (Just contentLength) = Just contentLength
foldHeaders header Nothing =
            let (name :: value :: xs) = split (== ':') header in
            if(name == "Content-Length") then parseInt (ltrim value) else Nothing

private
extractContentLength : String -> Maybe Int
extractContentLength headers =
                     let lines = Strings.lines headers in
                     List.foldrImpl foldHeaders Nothing id lines

private
foldResponse : Bool -> Char -> (String, String, String) -> (String, String, String)
foldResponse b c (prev, headers, body) =
   let p = if(length(prev) == 4) then strTail (prev ++ singleton(c)) else prev ++ singleton(c) in
   if(b || prev == "\r\n\r\n") then
        (prev, headers, body ++ singleton(c))
   else
        (p, headers ++ singleton(c), body)

private
parseResponse : Socket -> ByteLength -> Maybe Int -> String -> String -> IO(String, String)
parseResponse socket toRead maybeContentLength accHeaders accBody = do
    res <- recvFrom socket toRead
    case res of
      Left err => pure ("", "")
      Right (_, d, _) =>
            let chars = List.reverse $ Strings.unpack d
                f = foldResponse $ Maybe.isJust maybeContentLength
                (_, partialHeaders, partialBody) = List.foldrImpl f ("", "", "") id chars
                eventuallyContentLength = maybe (extractContentLength partialHeaders) (\_ => maybeContentLength) maybeContentLength in

                let totalRead = (length accBody) + (length partialBody)
                    headers = accHeaders ++ partialHeaders
                    body = accBody ++ partialBody in

                case eventuallyContentLength of
                     Just contentLength =>
                          if(length body < cast {to=Nat} contentLength) then
                            parseResponse socket toRead eventuallyContentLength headers body
                          else
                            pure (headers, body)
                     _  => parseResponse socket toRead Nothing accHeaders accBody

private
httpConnect : SocketAddress -> IO(Maybe Socket)
httpConnect address = do
        eventuallySocket <- socket AF_INET Stream 0
        case eventuallySocket of
             Left _ => pure Nothing
             Right sock => do
                    res <- connect sock address 80
                    if(res == 0) then pure (Just sock) else pure Nothing

private
write : Socket -> String -> IO(Maybe ByteLength)
write socket d = do 
  res <- send socket d
  case res of
       Left err => pure Nothing
       Right byteLength => pure $ Just byteLength

public
post : String -> String -> IO (Maybe (String, String))
post path params = do
     maybeSocket <- httpConnect $ IPv4Addr 91 209 78 59
     case maybeSocket of
          Just sock =>
               let size = show(length params)
                   res = write sock ("POST " ++ path ++ " HTTP/1.1\r\n" ++ "Host: vindinium.org" ++ "\r\nContent-Length: " ++ size ++ "\r\n" ++ "Content-Type: application/x-www-form-urlencoded\r\n" ++ "\r\n" ++ params) in
                   do
                   eventuallySent <- res
                   case eventuallySent of
                        Just _ => do
                             response <- parseResponse sock 10000 Nothing "" ""
                             pure $ Just response
                        Nothing => pure Nothing
          Nothing => pure Nothing

public
main : IO ()
main = do
     response <- post "/api/training" "key=kw2q1es1"
     case response of
          Just (headers, body) => putStrLn body
          Nothing => putStrLn "Unexpected error"
