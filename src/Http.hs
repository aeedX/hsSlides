{-# LANGUAGE ScopedTypeVariables #-}
-- | Минимальный HTTP/1.1-сервер: один запрос на соединение, только 127.0.0.1.
module Http
  ( Request (..), Response (..), serve
  , parseForm, utf8, respond, html, plain, redirectTo
  ) where

import Control.Concurrent (forkIO)
import Control.Exception (SomeException, evaluate, finally, try)
import Control.Monad (forever, void, when)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Char (digitToInt, isHexDigit, ord, toLower)
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import qualified Network.Socket as NS
import Network.Socket.ByteString (recv, sendAll)
import System.Info (os)
import Text.Read (readMaybe)

data Request = Request
  { reqMethod  :: String
  , reqPath    :: String
  , reqQuery   :: [(String, String)]
  , reqHeaders :: [(String, String)]   -- ^ имена в нижнем регистре
  , reqBody    :: BS.ByteString
  }

data Response = Response
  { resStatus  :: Int
  , resHeaders :: [(String, String)]
  , resBody    :: BS.ByteString
  }

utf8 :: String -> BS.ByteString
utf8 = TE.encodeUtf8 . T.pack

respond :: String -> BS.ByteString -> Response
respond ctype = Response 200 [("Content-Type", ctype)]

html :: String -> Response
html = respond "text/html; charset=utf-8" . utf8

plain :: Int -> String -> Response
plain code msg = Response code [("Content-Type", "text/plain; charset=utf-8")] (utf8 msg)

redirectTo :: String -> Response
redirectTo loc = Response 303 [("Location", loc)] BS.empty

------------------------------------------------------------------------
-- Сервер

maxHead, maxBody :: Int
maxHead = 65536
maxBody = 4 * 1024 * 1024

-- | Слушает 127.0.0.1:port и обрабатывает каждое соединение в своём потоке.
serve :: Int -> (Request -> IO Response) -> IO ()
serve port handler = NS.withSocketsDo $ do
  sock <- NS.socket NS.AF_INET NS.Stream NS.defaultProtocol
  -- На Windows SO_REUSEADDR позволяет занять чужой порт, поэтому не включаем.
  when (os /= "mingw32") $ NS.setSocketOption sock NS.ReuseAddr 1
  NS.bind sock (NS.SockAddrInet (fromIntegral port) (NS.tupleToHostAddress (127, 0, 0, 1)))
  NS.listen sock 128
  forever $ do
    accepted <- try (NS.accept sock)
    case accepted of
      Left (_ :: SomeException) -> pure ()
      Right (conn, _) -> void (forkIO (talk conn `finally` NS.close conn))
  where
    talk conn = do
      parsed <- try (readRequest conn)
      case parsed of
        Left (_ :: SomeException) -> pure ()
        Right (Left (0, _)) -> pure ()
        Right (Left (code, msg)) -> reply conn (plain code msg)
        Right (Right req) -> do
          result <- try (handler req >>= \r -> evaluate (BS.length (resBody r)) >> pure r)
          case result of
            Left (e :: SomeException) -> reply conn (plain 500 ("Internal error: " ++ show e))
            Right r -> reply conn r
    reply conn r = do
      _ <- try (sendAll conn (renderResponse r)) :: IO (Either SomeException ())
      pure ()

-- | Читает запрос целиком. Left (0, _) — клиент закрыл соединение молча.
readRequest :: NS.Socket -> IO (Either (Int, String) Request)
readRequest sock = go BS.empty
  where
    crlf2 = BC.pack "\r\n\r\n"
    go acc = case BS.breakSubstring crlf2 acc of
      (hd, rest) | not (BS.null rest) -> withHead hd (BS.drop 4 rest)
      _ | BS.length acc > maxHead -> pure (Left (431, "Request header too large"))
        | otherwise -> do
            chunk <- recv sock 4096
            if BS.null chunk then pure (Left (0, "")) else go (BS.append acc chunk)
    withHead hd rest = case parseHead hd of
      Nothing -> pure (Left (400, "Bad request"))
      Just (method, target, headers) -> do
        let len = max 0 (fromMaybe 0 (lookup "content-length" headers >>= readMaybe))
            (path, q) = break (== '?') target
        if len > maxBody
          then pure (Left (413, "Request body too large"))
          else do
            when (map toLower (fromMaybe "" (lookup "expect" headers)) == "100-continue") $
              sendAll sock (BC.pack "HTTP/1.1 100 Continue\r\n\r\n")
            body <- readBody len rest
            pure $ if BS.length body < len
              then Left (0, "")
              else Right Request
                { reqMethod = method, reqPath = path
                , reqQuery = parseForm (drop 1 q)
                , reqHeaders = headers, reqBody = body }
    readBody len acc
      | BS.length acc >= len = pure (BS.take len acc)
      | otherwise = do
          chunk <- recv sock (min 65536 (len - BS.length acc))
          if BS.null chunk then pure acc else readBody len (BS.append acc chunk)

parseHead :: BS.ByteString -> Maybe (String, String, [(String, String)])
parseHead hd = case map (filter (/= '\r') . BC.unpack) (BC.split '\n' hd) of
  (reqLine : hdrLines) -> case words reqLine of
    [method, target, _version] -> Just (method, target, mapMaybe header hdrLines)
    _ -> Nothing
  [] -> Nothing
  where
    header l = case break (== ':') l of
      (k, ':' : v) -> Just (map toLower k, dropWhile (== ' ') v)
      _ -> Nothing

renderResponse :: Response -> BS.ByteString
renderResponse (Response st hs body) = BS.append (BC.pack headTxt) body
  where
    headTxt = "HTTP/1.1 " ++ show st ++ " " ++ reason st ++ "\r\n"
      ++ concatMap (\(k, v) -> k ++ ": " ++ v ++ "\r\n") allHeaders ++ "\r\n"
    allHeaders = hs ++
      [ ("Content-Length", show (BS.length body))
      , ("Connection", "close")
      , ("Cache-Control", "no-store")
      , ("X-Content-Type-Options", "nosniff")
      , ("X-Frame-Options", "DENY") ]

reason :: Int -> String
reason c = case c of
  200 -> "OK"
  204 -> "No Content"
  303 -> "See Other"
  400 -> "Bad Request"
  403 -> "Forbidden"
  404 -> "Not Found"
  413 -> "Payload Too Large"
  431 -> "Request Header Fields Too Large"
  _ -> "Internal Server Error"

------------------------------------------------------------------------
-- Разбор форм (application/x-www-form-urlencoded)

-- | Вход — строка, где каждый символ соответствует одному байту запроса.
parseForm :: String -> [(String, String)]
parseForm s = [pair kv | kv <- splitOn '&' s, not (null kv)]
  where
    pair kv = let (k, v) = break (== '=') kv in (urlDecode k, urlDecode (drop 1 v))

splitOn :: Char -> String -> [String]
splitOn c s = case break (== c) s of
  (a, []) -> [a]
  (a, _ : rest) -> a : splitOn c rest

urlDecode :: String -> String
urlDecode = T.unpack . TE.decodeUtf8With TE.lenientDecode . BS.pack . go
  where
    go ('+' : cs) = 32 : go cs
    go ('%' : a : b : cs)
      | isHexDigit a && isHexDigit b = fromIntegral (digitToInt a * 16 + digitToInt b) : go cs
    go (c : cs) = fromIntegral (ord c) : go cs
    go [] = []
