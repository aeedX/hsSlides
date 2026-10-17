-- | Модель презентации: слайды, элементы, чистые операции над ними,
-- сохранение и загрузка файла.
module Deck
  ( Kind (..), Align (..), Font (..), Layout (..)
  , Element (..), Slide (..), Deck (..)
  , slideW, slideH
  , initialDeck, newElement, minSide, clampElement
  , validColor, readEnum, readInt, trim, lookupLast
  , isUrl, hasImageExt, imageMime, maxImageBytes
  , slideAt, slideElems, findElement
  , updateElement, removeElement, addElement, duplicateElement
  , bringToFront, sendToBack
  , insertSlide, duplicateSlide, deleteSlide, moveSlide, updateSlide
  , saveDeck, loadDeck
  ) where

import Control.Exception (IOException, try)
import qualified Data.ByteString.Char8 as BC
import Data.Char (isHexDigit, isSpace, toLower)
import Data.List (dropWhileEnd, isPrefixOf, partition)
import Data.Maybe (isJust)
import System.Directory (renameFile)
import System.FilePath (takeExtension)

-- | Размер слайда в условных пикселях (16:9).
slideW, slideH :: Int
slideW = 960
slideH = 540

data Kind = TextBox | Rect | RoundRect | Ellipse | Triangle | Line | Image
  deriving (Show, Read, Eq, Enum, Bounded)

data Align = AlignLeft | AlignCenter | AlignRight
  deriving (Show, Read, Eq, Enum, Bounded)

data Font = Sans | Serif | Mono
  deriving (Show, Read, Eq, Enum, Bounded)

-- | Макеты нового слайда.
data Layout = LBlank | LTitle | LTitleContent
  deriving (Show, Read, Eq, Enum, Bounded)

-- | Элемент слайда. Цвета хранятся как "#rrggbb", пустая строка — «нет».
data Element = Element
  { elId     :: Int
  , elKind   :: Kind
  , elX      :: Int
  , elY      :: Int
  , elW      :: Int
  , elH      :: Int
  , elText   :: String
  , elSize   :: Int
  , elBold   :: Bool
  , elItalic :: Bool
  , elAlign  :: Align
  , elFont   :: Font
  , elColor  :: String
  , elFill   :: String
  , elStroke :: String
  , elLineW  :: Int
  , elFlip   :: Bool
  , elSrc    :: String
  } deriving (Show, Read, Eq)

data Slide = Slide
  { slBg    :: String
  , slElems :: [Element]
  } deriving (Show, Read, Eq)

data Deck = Deck
  { dkTitle  :: String
  , dkSlides :: [Slide]
  , dkNextId :: Int
  } deriving (Show, Read, Eq)

------------------------------------------------------------------------
-- Вспомогательные функции

clamp :: Int -> Int -> Int -> Int
clamp lo hi = max lo . min hi

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

readInt :: String -> Maybe Int
readInt s = case reads (trim s) of
  [(n, "")] -> Just n
  _ -> Nothing

readEnum :: (Show a, Enum a, Bounded a) => String -> Maybe a
readEnum s = case [x | x <- [minBound .. maxBound], show x == s] of
  (x : _) -> Just x
  [] -> Nothing

-- | Последнее значение параметра формы (нужно для пары hidden + checkbox).
lookupLast :: String -> [(String, String)] -> Maybe String
lookupLast k = foldl (\acc (k', v) -> if k' == k then Just v else acc) Nothing

validColor :: String -> Bool
validColor ('#' : cs) = length cs == 6 && all isHexDigit cs
validColor _ = False

isUrl :: String -> Bool
isUrl s = any (`isPrefixOf` map toLower s) ["http://", "https://"]

imageMime :: FilePath -> Maybe String
imageMime p = lookup (map toLower (takeExtension p))
  [ (".png", "image/png"), (".jpg", "image/jpeg"), (".jpeg", "image/jpeg")
  , (".gif", "image/gif"), (".webp", "image/webp"), (".svg", "image/svg+xml")
  , (".bmp", "image/bmp") ]

hasImageExt :: FilePath -> Bool
hasImageExt = isJust . imageMime

-- | Больше этого размера локальные картинки не читаем (защита памяти).
maxImageBytes :: Integer
maxImageBytes = 50 * 1024 * 1024

-- | Минимальная сторона: у линии может быть нулевая высота (горизонталь).
minSide :: Kind -> Int
minSide Line = 0
minSide _ = 10

-- | Приводит геометрию и размеры к допустимым значениям.
clampElement :: Element -> Element
clampElement e = e
  { elX = clamp (-5000) 5000 (elX e)
  , elY = clamp (-5000) 5000 (elY e)
  , elW = clamp (minSide k) 5000 (elW e)
  , elH = clamp (minSide k) 5000 (elH e)
  , elSize = clamp 6 400 (elSize e)
  , elLineW = clamp 0 40 (elLineW e)
  , elText = take 20000 (elText e)
  , elSrc = take 1000 (elSrc e)
  }
  where k = elKind e

------------------------------------------------------------------------
-- Создание элементов и слайдов

newElement :: Int -> Kind -> Element
newElement i k = case k of
  TextBox   -> base { elText = "Текст", elW = 400, elH = 80 }
  Rect      -> shape
  RoundRect -> shape
  Ellipse   -> shape { elH = 160 }
  Triangle  -> shape { elW = 200, elH = 180 }
  Line      -> base { elW = 300, elH = 0, elStroke = "#444444", elLineW = 4 }
  Image     -> base { elW = 320, elH = 200 }
  where
    base = Element
      { elId = i, elKind = k, elX = 100, elY = 100, elW = 240, elH = 140
      , elText = "", elSize = 28, elBold = False, elItalic = False
      , elAlign = AlignLeft, elFont = Sans, elColor = "#222222"
      , elFill = "", elStroke = "", elLineW = 0, elFlip = False, elSrc = "" }
    shape = base
      { elFill = "#4472c4", elStroke = "#2f528f", elLineW = 2
      , elColor = "#ffffff", elAlign = AlignCenter }

layoutElements :: Layout -> Int -> ([Element], Int)
layoutElements LBlank n = ([], n)
layoutElements LTitle n =
  ( [ (newElement n TextBox)
        { elText = "Заголовок презентации", elX = 80, elY = 170, elW = 800, elH = 110
        , elSize = 54, elBold = True, elAlign = AlignCenter }
    , (newElement (n + 1) TextBox)
        { elText = "Подзаголовок", elX = 80, elY = 300, elW = 800, elH = 60
        , elSize = 28, elColor = "#666666", elAlign = AlignCenter } ]
  , n + 2 )
layoutElements LTitleContent n =
  ( [ (newElement n TextBox)
        { elText = "Заголовок слайда", elX = 60, elY = 30, elW = 840, elH = 80
        , elSize = 40, elBold = True }
    , (newElement (n + 1) TextBox)
        { elText = "• Первый пункт\n• Второй пункт\n• Третий пункт"
        , elX = 60, elY = 140, elW = 840, elH = 340, elSize = 28 } ]
  , n + 2 )

initialDeck :: Deck
initialDeck = insertSlide (-1) LTitle (Deck "Новая презентация" [] 1)

------------------------------------------------------------------------
-- Операции над слайдами

mapAt :: Int -> (a -> a) -> [a] -> [a]
mapAt i f xs = [if j == i then f x else x | (j, x) <- zip [0 ..] xs]

slideAt :: Int -> Deck -> Maybe Slide
slideAt i d
  | i < 0 = Nothing
  | otherwise = case drop i (dkSlides d) of
      (s : _) -> Just s
      [] -> Nothing

slideElems :: Int -> Deck -> [Element]
slideElems i d = maybe [] slElems (slideAt i d)

updateSlide :: Int -> (Slide -> Slide) -> Deck -> Deck
updateSlide i f d = d { dkSlides = mapAt i f (dkSlides d) }

-- | Вставляет новый слайд после слайда с номером i (i = -1 — в начало).
insertSlide :: Int -> Layout -> Deck -> Deck
insertSlide i layout d =
  d { dkSlides = before ++ [Slide "#ffffff" es] ++ after, dkNextId = next }
  where
    (es, next) = layoutElements layout (dkNextId d)
    (before, after) = splitAt (max 0 (i + 1)) (dkSlides d)

duplicateSlide :: Int -> Deck -> Deck
duplicateSlide i d = case slideAt i d of
  Nothing -> d
  Just s ->
    let es = zipWith (\n e -> e { elId = n }) [dkNextId d ..] (slElems s)
        (before, after) = splitAt (i + 1) (dkSlides d)
    in d { dkSlides = before ++ [s { slElems = es }] ++ after
         , dkNextId = dkNextId d + length es }

-- | Удаляет слайд; последний оставшийся слайд только очищается.
deleteSlide :: Int -> Deck -> Deck
deleteSlide i d = case dkSlides d of
  [_] -> d { dkSlides = [Slide "#ffffff" []] }
  slides -> d { dkSlides = [s | (j, s) <- zip [0 ..] slides, j /= i] }

moveSlide :: Int -> Int -> Deck -> Deck
moveSlide from to d = case splitAt from (dkSlides d) of
  (a, s : b) | from >= 0 && to >= 0 && to < length (dkSlides d) ->
    let rest = a ++ b
        (x, y) = splitAt to rest
    in d { dkSlides = x ++ [s] ++ y }
  _ -> d

------------------------------------------------------------------------
-- Операции над элементами

findElement :: Int -> Deck -> Maybe (Int, Element)
findElement i d =
  case [(si, e) | (si, s) <- zip [0 ..] (dkSlides d), e <- slElems s, elId e == i] of
    (x : _) -> Just x
    [] -> Nothing

updateElement :: Int -> (Element -> Element) -> Deck -> Deck
updateElement i f d = d { dkSlides = map onSlide (dkSlides d) }
  where
    onSlide s = s { slElems = map g (slElems s) }
    g e | elId e == i = clampElement (f e)
        | otherwise = e

removeElement :: Int -> Deck -> Deck
removeElement i d = d { dkSlides = map onSlide (dkSlides d) }
  where onSlide s = s { slElems = filter ((/= i) . elId) (slElems s) }

-- | Добавляет элемент на слайд si (поверх остальных); возвращает его номер.
addElement :: Int -> Kind -> (Element -> Element) -> Deck -> (Deck, Int)
addElement si k tweak d =
  ( (updateSlide si (\s -> s { slElems = slElems s ++ [e] }) d) { dkNextId = i + 1 }
  , i )
  where
    i = dkNextId d
    e = clampElement (tweak (newElement i k))

duplicateElement :: Int -> Deck -> (Deck, Maybe Int)
duplicateElement i d = case findElement i d of
  Nothing -> (d, Nothing)
  Just (si, e) ->
    let n = dkNextId d
        e' = clampElement e { elId = n, elX = elX e + 20, elY = elY e + 20 }
    in ( (updateSlide si (\s -> s { slElems = slElems s ++ [e'] }) d) { dkNextId = n + 1 }
       , Just n )

bringToFront, sendToBack :: Int -> Deck -> Deck
bringToFront = reorder (\e rest -> rest ++ [e])
sendToBack = reorder (:)

reorder :: (Element -> [Element] -> [Element]) -> Int -> Deck -> Deck
reorder f i d = d { dkSlides = map go (dkSlides d) }
  where
    go s = case partition ((== i) . elId) (slElems s) of
      ([e], rest) -> s { slElems = f e rest }
      _ -> s

------------------------------------------------------------------------
-- Файл презентации

fileHeader :: String
fileHeader = "HSSLIDES 1"

-- | Записывает файл атомарно (через временный файл). Содержимое — чистый
-- ASCII, потому что show экранирует все не-ASCII символы.
saveDeck :: FilePath -> Deck -> IO ()
saveDeck path d = do
  let tmp = path ++ ".tmp"
  writeFile tmp (fileHeader ++ "\n" ++ show d ++ "\n")
  renameFile tmp path

loadDeck :: FilePath -> IO (Either String Deck)
loadDeck path = do
  r <- try (BC.readFile path)
  pure $ case r of
    Left e -> Left (show (e :: IOException))
    Right bytes -> case break (== '\n') (BC.unpack bytes) of
      (h, '\n' : body) | trim h == fileHeader -> case reads body of
        [(d, rest)] | all isSpace rest -> Right (sanitize d)
        _ -> Left "file is corrupted"
      _ -> Left "not a HsSlides file (missing HSSLIDES header)"

-- | Защита от ручной правки файла: корректные цвета, размеры, номера.
sanitize :: Deck -> Deck
sanitize d = d { dkSlides = slides, dkNextId = max (dkNextId d) (maxId + 1) }
  where
    slides0 = if null (dkSlides d) then [Slide "#ffffff" []] else dkSlides d
    slides = map fixSlide slides0
    maxId = maximum (0 : [elId e | s <- slides, e <- slElems s])
    fixColor def c = if validColor c then map toLower c else def
    fixSlide s = s { slBg = fixColor "#ffffff" (slBg s)
                   , slElems = map (clampElement . fixElement) (slElems s) }
    fixElement e = e { elColor = fixColor "#222222" (elColor e)
                     , elFill = fixColor "" (elFill e)
                     , elStroke = fixColor "" (elStroke e) }
