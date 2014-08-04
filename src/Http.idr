module Http

import Network.Socket
import Debug.Trace
import Silly
import Model

-- Basic HTTP support

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

foldMkString : String -> String -> String -> String
foldMkString separator s acc = acc ++ separator ++ s

private
mkString : String -> List String -> String
mkString separator list = List.foldrImpl (foldMkString separator) "" id (reverse list)

private
parseURL : String -> Maybe (String, String)
parseURL url =
    let splitted = split (== '/') url in
        case splitted of
             ("http:" :: _ :: host :: components) => Just $ (host, mkString "/" components)
             _ => Nothing

private
post : String -> String -> IO (Maybe (String, String))
post url params = do
     case (parseURL url) of
          Just (host, path) => do
               maybeSocket <- httpConnect $ (Hostname host)
               case maybeSocket of
                    Just sock =>
                         let size = show(length params)
                             content = "POST " ++ path ++ " HTTP/1.1\r\n" ++ "Host: vindinium.org" ++ "\r\nContent-Length: " ++ size ++ "\r\n" ++ "Content-Type: application/x-www-form-urlencoded\r\n" ++ "\r\n" ++ params
                             res = write sock (content) in
                             do
                             eventuallySent <- res
                             case eventuallySent of
                                  Just _ => do
                                       response <- parseResponse sock 10000 Nothing "" ""
                                       pure $ Just response
                                  Nothing => pure Nothing
                    Nothing => pure Nothing
          Nothing => pure Nothing

-- Vindinium HTTP

private
parseInput : IO $ Maybe (String, String) -> IO $ Either String Input
parseInput r = do
              response <- r
              case response of
                   Just (_, body) => case Model.parseInput body of
                        Just input => pure $ Right input
                        Nothing => pure $ Left body
                   Nothing => pure $ Left "Unable to read response."

public
arena : String -> IO $ Either String Input
arena key = parseInput (post "http://vindinium.org/api/arena" ("key=" ++ key))

public
training : String -> Int -> Maybe String -> IO $ Either String Input
training key turns maybeMap =
         let map = fromMaybe "" maybeMap
             params = ("key=" ++ key) ++ ("&turns=" ++ show turns) ++ ("&map=" ++ map) in
         parseInput (post "http://vindinium.org/api/training" params)

public
move : String -> Direction -> IO $ Either String Input
move url direction = parseInput (post url ("dir=" ++ show direction))
