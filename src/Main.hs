{-# LANGUAGE ScopedTypeVariables #-}
-- | HsSlides: мини-аналог PowerPoint. Редактор открывается в браузере,
-- вся логика, хранение и вёрстка слайдов написаны на Haskell.
module Main (main) where

import Control.Concurrent.MVar (MVar, modifyMVar, newMVar, readMVar)
import Control.Exception (IOException, catch, try)
import Control.Monad (when)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import Data.Char (toLower)
import Data.Maybe (fromMaybe)
import System.Directory (doesFileExist, getFileSize)
import System.Environment (getArgs)
import System.Exit (die, exitSuccess)
import System.IO (hFlush, hPutStrLn, hSetEncoding, stderr, stdout, utf8)
import Text.Read (readMaybe)

import Deck
import Editor
import Http hiding (utf8)
import qualified Http
import Render

usage :: String
usage = unlines
  [ "HsSlides - a small PowerPoint-like presentation editor (opens in the browser)"
  , ""
  , "Usage: hs-slides [FILE] [PORT]"
  , "  FILE  presentation file (default: presentation.hsl, created if missing)"
  , "  PORT  local port (default: 8080)"
  ]

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  args <- getArgs
  when (any (`elem` ["-h", "--help"]) args) $ putStr usage >> exitSuccess
  let file = case args of
        (f : _) -> f
        [] -> "presentation.hsl"
      port = case args of
        (_ : p : _) -> fromMaybe 0 (readMaybe p)
        _ -> 8080 :: Int
  when (port < 1 || port > 65535) $ die "Port must be a number from 1 to 65535."
  exists <- doesFileExist file
  deck <- if exists
    then loadDeck file >>= either (\err -> die ("Cannot open " ++ file ++ ": " ++ err)) pure
    else do
      saveDeck file initialDeck `catch` \(e :: IOException) ->
        hPutStrLn stderr ("Warning: cannot create " ++ file ++ ": " ++ show e)
      pure initialDeck
  st <- newMVar (newEditor file deck)
  putStrLn ("HsSlides, file: " ++ file)
  putStrLn ("Open in your browser: http://127.0.0.1:" ++ show port ++ "/")
  putStrLn "Press Ctrl+C to stop."
  hFlush stdout
  serve port (route st port) `catch` \(e :: IOException) ->
    die ("Cannot listen on 127.0.0.1:" ++ show port ++ ": " ++ show e)

------------------------------------------------------------------------
-- Защита локального сервера

-- | Принимаем только запросы, адресованные нам по локальному имени.
-- Это закрывает атаку через подмену DNS (DNS rebinding).
hostOk :: Int -> Request -> Bool
hostOk port req = case lookup "host" (reqHeaders req) of
  Nothing -> True
  Just h -> map toLower h `elem` ["127.0.0.1:" ++ show port, "localhost:" ++ show port]

-- | Изменения принимаем только со страниц самого редактора (защита от CSRF).
originOk :: Int -> Request -> Bool
originOk port req = case lookup "origin" (reqHeaders req) of
  Nothing -> True
  Just o -> map toLower o `elem` ["http://127.0.0.1:" ++ show port, "http://localhost:" ++ show port]

------------------------------------------------------------------------
-- Маршруты

segments :: String -> [String]
segments s = case break (== '/') s of
  (a, []) -> [a | not (null a)]
  (a, _ : rest) -> [a | not (null a)] ++ segments rest

route :: MVar Editor -> Int -> Request -> IO Response
route st port req
  | not (hostOk port req) = pure (plain 403 "Forbidden: unexpected Host header")
  | reqMethod req == "POST" && not (originOk port req) =
      pure (plain 403 "Forbidden: cross-site request")
  | otherwise = case (reqMethod req, segments (reqPath req)) of
      ("GET", []) -> modifyMVar st (\ed -> pure (ed { edMsg = "" }, html (editorPage ed)))
      ("GET", ["slide", n]) -> navigate (maybe id (\k -> goSlide (k - 1)) (readMaybe n))
      ("GET", ["select", n]) -> navigate (maybe id pickElement (readMaybe n))
      ("GET", ["deselect"]) -> navigate clearSelection
      ("GET", ["show"]) -> do
        ed <- readMVar st
        pure (html (standalonePage True (edDeck ed) localImageSrc))
      ("GET", ["export"]) -> exportPage
      ("GET", ["img", n]) -> serveImage n
      ("GET", ["favicon.ico"]) -> pure (Response 204 [] BS.empty)
      ("POST", ["act"]) -> act
      _ -> pure (plain 404 "Not found")
  where
    navigate f = modifyMVar st (\ed -> pure (f ed, redirectTo "/"))

    act = do
      let params = parseForm (BC.unpack (reqBody req))
      modifyMVar st $ \ed -> do
        ed' <- applyAction params ed
        ed'' <- if edDeck ed' /= edDeck ed then persist ed' else pure ed'
        pure (ed'', redirectTo "/")

    exportPage = do
      ed <- readMVar st
      let d = edDeck ed
      images <- loadImages d
      let src e
            | isUrl (elSrc e) = esc (elSrc e)
            | otherwise = imageMarker e
          body = spliceImages images (utf8String (standalonePage False d src))
      pure (Response 200
              [ ("Content-Type", "text/html; charset=utf-8")
              , ("Content-Disposition", "attachment; filename=\"presentation.html\"") ]
              body)

    serveImage n = do
      ed <- readMVar st
      case readMaybe n >>= \i -> findElement i (edDeck ed) of
        Just (_, e)
          | elKind e == Image, not (isUrl (elSrc e)), Just mime <- imageMime (elSrc e) -> do
              r <- readImageFile (elSrc e)
              pure $ case r of
                Just bytes -> Response 200
                  [ ("Content-Type", mime)
                  , ("Content-Security-Policy", "default-src 'none'; style-src 'unsafe-inline'; sandbox") ]
                  bytes
                Nothing -> plain 404 "File not found"
        _ -> pure (plain 404 "Not found")

utf8String :: String -> BS.ByteString
utf8String = Http.utf8

-- | Сохраняет файл; при ошибке оставляет понятное сообщение в редакторе.
persist :: Editor -> IO Editor
persist ed = do
  r <- try (saveDeck (edFile ed) (edDeck ed))
  pure $ case r of
    Right () -> ed
    Left (e :: IOException) ->
      ed { edMsg = "Не удалось сохранить файл " ++ edFile ed ++ ": " ++ show e }

-- | Читает файл картинки, если он существует и не слишком велик.
readImageFile :: FilePath -> IO (Maybe BS.ByteString)
readImageFile path = do
  r <- try (do size <- getFileSize path
               if size > maxImageBytes then pure Nothing else Just <$> BS.readFile path)
  pure $ case r of
    Right m -> m
    Left (_ :: IOException) -> Nothing

-- | Для экспорта: data:-адреса всех локальных картинок документа.
loadImages :: Deck -> IO [(Int, BS.ByteString)]
loadImages d = fmap concat (mapM load images)
  where
    images = [e | s <- dkSlides d, e <- slElems s, elKind e == Image, not (isUrl (elSrc e))]
    load e = case imageMime (elSrc e) of
      Nothing -> pure []
      Just mime -> do
        r <- readImageFile (elSrc e)
        pure $ case r of
          Just bytes ->
            [(elId e, BS.concat [BC.pack ("data:" ++ mime ++ ";base64,"), base64 bytes])]
          Nothing -> []

-- | Заменяет метки картинок (U+E000 номер U+E001, в UTF-8) готовыми адресами.
-- Картинка, которую не удалось прочитать, даёт пустой адрес.
spliceImages :: [(Int, BS.ByteString)] -> BS.ByteString -> BS.ByteString
spliceImages imgs = BS.concat . go
  where
    open = BS.pack [0xEE, 0x80, 0x80]
    close = BS.pack [0xEE, 0x80, 0x81]
    go bs = case BS.breakSubstring open bs of
      (before, rest)
        | BS.null rest -> [before]
        | otherwise ->
            let (num, rest') = BS.breakSubstring close (BS.drop 3 rest)
                uri = case BC.readInt num of
                  Just (i, _) -> fromMaybe BS.empty (lookup i imgs)
                  Nothing -> BS.empty
            in before : uri : go (BS.drop 3 rest')
