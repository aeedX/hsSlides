-- | Генерация HTML: слайды, страница редактора, страница показа/экспорта.
module Render
  ( esc, base64, imageMarker, localImageSrc, editorPage, standalonePage
  ) where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import qualified Data.ByteString as BS
import Data.Maybe (fromMaybe, isJust)
import Data.Word (Word8)

import Assets
import Deck
import Editor

------------------------------------------------------------------------
-- Мелкие помощники

-- | Экранирует текст для HTML. Символы U+E000 и U+E001 служат метками картинок
-- при экспорте (см. 'imageMarker'), поэтому в пользовательском тексте их нет.
esc :: String -> String
esc = concatMap f
  where
    f '\xE000' = ""
    f '\xE001' = ""
    f '&' = "&amp;"
    f '<' = "&lt;"
    f '>' = "&gt;"
    f '"' = "&quot;"
    f '\'' = "&#39;"
    f c = [c]

attr :: String -> String -> String
attr k v = " " ++ k ++ "=\"" ++ esc v ++ "\""

px :: Int -> String
px n = show n ++ "px"

css :: [(String, String)] -> String
css = concatMap (\(k, v) -> k ++ ":" ++ v ++ ";")

-- | Base64 без промежуточных списков: i-й символ вычисляется по индексу.
base64 :: BS.ByteString -> BS.ByteString
base64 bs = fst (BS.unfoldrN outLen (\k -> Just (sym k, k + 1)) 0)
  where
    n = BS.length bs
    outLen = 4 * ((n + 2) `div` 3)
    byte :: Int -> Int
    byte i = if i < n then fromIntegral (BS.index bs i) else 0
    sym :: Int -> Word8
    sym k
      | pad = 61
      | otherwise = alphabet idx
      where
        j = 3 * (k `div` 4)
        r = k `mod` 4
        b0 = byte j
        b1 = byte (j + 1)
        b2 = byte (j + 2)
        idx = case r of
          0 -> b0 `shiftR` 2
          1 -> ((b0 .&. 3) `shiftL` 4) .|. (b1 `shiftR` 4)
          2 -> ((b1 .&. 15) `shiftL` 2) .|. (b2 `shiftR` 6)
          _ -> b2 .&. 63
        pad = (r == 2 && j + 1 >= n) || (r == 3 && j + 2 >= n)

alphabet :: Int -> Word8
alphabet i
  | i < 26 = fromIntegral (65 + i)
  | i < 52 = fromIntegral (97 + i - 26)
  | i < 62 = fromIntegral (48 + i - 52)
  | i == 62 = 43
  | otherwise = 47

-- | Метка вместо адреса картинки: при экспорте вместо неё подставляется
-- data:-адрес, причём уже на уровне байтов (так большие файлы не раздувают память).
imageMarker :: Element -> String
imageMarker e = "\xE000" ++ show (elId e) ++ "\xE001"

-- | Адрес картинки для страниц, которые отдаёт сам сервер. Функции такого типа
-- возвращают значение, уже готовое для HTML-атрибута (экранированное).
localImageSrc :: Element -> String
localImageSrc e
  | isUrl (elSrc e) = esc (elSrc e)
  | otherwise = "/img/" ++ show (elId e)

------------------------------------------------------------------------
-- Элементы и слайды

fontStack :: Font -> String
fontStack Sans = "'Segoe UI',Roboto,'Helvetica Neue',Arial,sans-serif"
fontStack Serif = "Georgia,'Times New Roman',serif"
fontStack Mono = "Consolas,'Courier New',monospace"

alignCss :: Align -> String
alignCss AlignLeft = "left"
alignCss AlignCenter = "center"
alignCss AlignRight = "right"

elementStyle :: Element -> String
elementStyle e = css (geometry ++ typography ++ decoration)
  where
    k = elKind e
    geometry =
      [ ("left", px (elX e)), ("top", px (elY e))
      , ("width", px (elW e)), ("height", px (elH e)) ]
    typography =
      [ ("font-size", px (elSize e)), ("color", elColor e)
      , ("font-family", fontStack (elFont e))
      , ("font-weight", if elBold e then "700" else "400")
      , ("font-style", if elItalic e then "italic" else "normal")
      , ("align-items", if k == TextBox then "flex-start" else "center") ]
    decoration
      | k `elem` [TextBox, Rect, RoundRect, Ellipse] =
          [ ("background", if null (elFill e) then "none" else elFill e)
          , ("border", border) ] ++ radius
      | otherwise = []
    border
      | null (elStroke e) || elLineW e <= 0 = "none"
      | otherwise = px (elLineW e) ++ " solid " ++ elStroke e
    radius = case k of
      Ellipse -> [("border-radius", "50%")]
      RoundRect -> [("border-radius", px (min (elW e) (elH e) `div` 6))]
      _ -> []

-- | SVG-обёртка для линии и треугольника. Нулевой размер svg не рисуется,
-- поэтому берём не меньше 1; всё, что вылезает за рамку, остаётся видимым.
svgWrap :: Element -> String -> String
svgWrap e inner = concat
  [ "<svg class=\"sv\" width=\"", show sw, "\" height=\"", show sh
  , "\" viewBox=\"0 0 ", show sw, " ", show sh, "\""
  , if elFlip e then " style=\"transform:scaleY(-1)\"" else ""
  , ">", inner, "</svg>" ]
  where
    sw = max 1 (elW e)
    sh = max 1 (elH e)

textDiv :: Element -> String
textDiv e = concat
  [ "<div class=\"t\" style=\"text-align:", alignCss (elAlign e), "\">", esc (elText e), "</div>" ]

elementInner :: (Element -> String) -> Element -> String
elementInner imgSrc e = case elKind e of
  Image -> "<img class=\"im\" draggable=\"false\" alt=\"\" src=\"" ++ imgSrc e ++ "\">"
  Line -> svgWrap e $ concat
    [ "<line x1=\"0\" y1=\"0\" x2=\"", show (elW e), "\" y2=\"", show (elH e)
    , "\" stroke=\"", if null (elStroke e) then "#444444" else elStroke e
    , "\" stroke-width=\"", show (max 1 (elLineW e)), "\" stroke-linecap=\"round\"/>" ]
  Triangle -> svgWrap e (concat
    [ "<polygon points=\"", show (w `div` 2), ",", show i, " ", show (w - i), ",", show (h - i)
    , " ", show i, ",", show (h - i), "\" fill=\"", if null (elFill e) then "none" else elFill e
    , "\" stroke=\"", if null (elStroke e) || elLineW e <= 0 then "none" else elStroke e
    , "\" stroke-width=\"", show (elLineW e), "\" stroke-linejoin=\"round\"/>" ]) ++ textDiv e
  _ -> textDiv e
  where
    w = max 1 (elW e)
    h = max 1 (elH e)
    i = elLineW e `div` 2

renderElement :: (Element -> String) -> Element -> String
renderElement imgSrc e = concat
  [ "<div class=\"el k-", show (elKind e), "\""
  , attr "data-id" (show (elId e))
  , attr "data-x" (show (elX e)), attr "data-y" (show (elY e))
  , attr "data-w" (show (elW e)), attr "data-h" (show (elH e))
  , " style=\"", elementStyle e, "\">", elementInner imgSrc e, "</div>" ]

-- | Рамка выделения с восемью маркерами изменения размера.
selectionOverlay :: Element -> String
selectionOverlay e = concat
  [ "<div id=\"sel\""
  , attr "data-id" (show (elId e))
  , attr "data-x" (show (elX e)), attr "data-y" (show (elY e))
  , attr "data-w" (show (elW e)), attr "data-h" (show (elH e))
  , attr "data-min" (show (minSide (elKind e)))
  , " style=\"", css [ ("left", px (elX e)), ("top", px (elY e))
                     , ("width", px (elW e)), ("height", px (elH e)) ], "\">"
  , concat ["<div class=\"h\" data-d=\"" ++ d ++ "\"></div>" | d <- ["nw", "n", "ne", "e", "se", "s", "sw", "w"]]
  , "</div>" ]

------------------------------------------------------------------------
-- Страница редактора

layoutName :: Layout -> String
layoutName LBlank = "Пустой слайд"
layoutName LTitle = "Титульный слайд"
layoutName LTitleContent = "Заголовок и текст"

editorPage :: Editor -> String
editorPage ed = concat
  [ "<!doctype html>\n<html lang=\"ru\"><head><meta charset=\"utf-8\">\n"
  , "<meta name=\"viewport\" content=\"width=device-width,initial-scale=1\">\n"
  , "<title>", esc (dkTitle d), " — HsSlides</title>\n"
  , "<style>\n", slideCss, editorCss, "</style></head>\n"
  , "<body", attr "data-cur" (show (edSlide ed)), attr "data-count" (show (length (dkSlides d))), ">\n"
  , headerHtml ed, messageHtml ed, ribbonHtml (isJust (selectedElement ed))
  , "<main>\n", slidesPanel ed, workPanel ed, propsPanel ed, "</main>\n"
  , "<script>\n", editorJs, "</script>\n</body></html>\n" ]
  where d = edDeck ed

button' :: String -> String -> Bool -> String
button' act label enabled = concat
  [ "<button name=\"a\" value=\"", act, "\"", if enabled then "" else " disabled", ">", label, "</button>" ]

headerHtml :: Editor -> String
headerHtml ed = concat
  [ "<header><span class=\"logo\">HsSlides</span>"
  , "<form class=\"ttl\" method=\"post\" action=\"/act\"><input type=\"hidden\" name=\"a\" value=\"title\">"
  , "<input name=\"title\" maxlength=\"120\" aria-label=\"Название презентации\"", attr "value" (dkTitle (edDeck ed)), ">"
  , "<button>Переименовать</button></form>"
  , "<span class=\"file\">Файл ", esc (edFile ed), ", сохраняется автоматически</span><div class=\"sp\"></div>"
  , "<form class=\"grp\" method=\"post\" action=\"/act\">"
  , button' "undo" "Отменить" (canUndo ed), button' "redo" "Вернуть" (canRedo ed), "</form>"
  , "<a class=\"btn\" href=\"/show#", show (edSlide ed + 1), "\">Показ</a>"
  , "<a class=\"btn\" href=\"/export\">Экспорт в HTML</a></header>\n" ]

messageHtml :: Editor -> String
messageHtml ed
  | null (edMsg ed) = ""
  | otherwise = "<div class=\"msg\" role=\"alert\">" ++ esc (edMsg ed) ++ "</div>\n"

ribbonHtml :: Bool -> String
ribbonHtml hasSel = concat
  [ "<div class=\"ribbon\">"
  , "<form class=\"grp\" method=\"post\" action=\"/act\"><input type=\"hidden\" name=\"a\" value=\"add\"><small>Вставить</small>"
  , concat [ "<button name=\"kind\" value=\"" ++ k ++ "\">" ++ l ++ "</button>"
           | (k, l) <- [ ("TextBox", "Текст"), ("Rect", "Прямоугольник"), ("RoundRect", "Скруглённый")
                        , ("Ellipse", "Овал"), ("Triangle", "Треугольник"), ("Line", "Линия") ] ]
  , "</form>"
  , "<form class=\"grp\" method=\"post\" action=\"/act\">"
  , "<input type=\"hidden\" name=\"a\" value=\"add\"><input type=\"hidden\" name=\"kind\" value=\"Image\">"
  , "<input name=\"src\" size=\"42\" required aria-label=\"Путь к файлу или ссылка на картинку\""
  , " placeholder=\"Путь к файлу или ссылка на картинку\"><button>Вставить картинку</button></form>"
  , "<form class=\"grp\" method=\"post\" action=\"/act\"><small>Объект</small>"
  , button' "dup" "Дублировать" hasSel, button' "front" "На передний план" hasSel
  , button' "back" "На задний план" hasSel, button' "del" "Удалить" hasSel
  , "</form></div>\n" ]

option :: String -> String -> Bool -> String
option v label selected = concat
  [ "<option", attr "value" v, if selected then " selected" else "", ">", esc label, "</option>" ]

slidesPanel :: Editor -> String
slidesPanel ed = concat
  [ "<aside id=\"slides\">"
  , "<form class=\"sl-ops\" method=\"post\" action=\"/act\">"
  , "<select name=\"layout\" aria-label=\"Макет нового слайда\">"
  , concat [option (show l) (layoutName l) (l == LTitleContent) | l <- [minBound .. maxBound]]
  , "</select>"
  , "<button name=\"a\" value=\"slide-add\">Новый слайд</button>"
  , "<button name=\"a\" value=\"slide-dup\">Дублировать слайд</button>"
  , "<div class=\"r2\">", button' "slide-up" "Выше" (si > 0)
  , button' "slide-down" "Ниже" (si < n - 1), "</div>"
  , "<button name=\"a\" value=\"slide-del\">Удалить слайд</button>"
  , "</form>"
  , concat (zipWith thumb [0 ..] slides)
  , "</aside>\n" ]
  where
    si = edSlide ed
    slides = dkSlides (edDeck ed)
    n = length slides
    thumb :: Int -> Slide -> String
    thumb i s = concat
      [ "<a class=\"th", if i == si then " cur" else "", "\" href=\"/slide/", show (i + 1), "\">"
      , "<span class=\"num\">", show (i + 1), "</span><div class=\"thumb\">"
      , "<div class=\"slide\" style=\"background:", slBg s, "\">"
      , concatMap (renderElement localImageSrc) (slElems s)
      , "</div></div></a>" ]

workPanel :: Editor -> String
workPanel ed = concat
  [ "<section id=\"work\"><div id=\"wrap\" style=\"height:540px\">"
  , "<div id=\"stage\" class=\"slide\" style=\"background:", slBg slide, "\">"
  , concatMap (renderElement localImageSrc) (slElems slide)
  , maybe "" selectionOverlay (selectedElement ed)
  , "</div></div>"
  , "<p class=\"hint\">Перетаскивайте объекты мышью и меняйте размер за маркеры. "
  , "Стрелки сдвигают выбранный объект, Delete удаляет, Ctrl+Z и Ctrl+Y отменяют и возвращают. "
  , "Двойной щелчок по объекту переводит к полю текста.</p>"
  , "</section>\n" ]
  where slide = fromMaybe (Slide "#ffffff" []) (slideAt (edSlide ed) (edDeck ed))

------------------------------------------------------------------------
-- Панель свойств

propsPanel :: Editor -> String
propsPanel ed = case selectedElement ed of
  Just e -> elementProps e
  Nothing -> slideProps (fromMaybe (Slide "#ffffff" []) (slideAt (edSlide ed) (edDeck ed)))

slideProps :: Slide -> String
slideProps s = concat
  [ "<aside id=\"props\"><h2>Слайд</h2>"
  , "<form id=\"bgf\" method=\"post\" action=\"/act\"><input type=\"hidden\" name=\"a\" value=\"bg\">"
  , "<label>Цвет фона<input type=\"color\" name=\"bg\"", attr "value" (slBg s), "></label></form>"
  , "<p class=\"hint\">Выберите объект на слайде, чтобы изменить его свойства.</p></aside>\n" ]

hidden :: String -> String -> String
hidden k v = "<input type=\"hidden\"" ++ attr "name" k ++ attr "value" v ++ ">"

numberField :: String -> String -> Int -> Int -> String
numberField label name lo v = concat
  [ "<label>", label, "<input type=\"number\"", attr "name" name, attr "min" (show lo)
  , attr "value" (show v), "></label>" ]

colorField :: String -> String -> String -> String
colorField label name v = concat
  [ "<label>", label, "<input type=\"color\"", attr "name" name, attr "value" v, "></label>" ]

-- | Флажок: скрытое поле со значением 0 идёт перед флажком, поэтому
-- сервер берёт последнее значение и различает «снят» и «нет такого поля».
checkField :: String -> String -> Bool -> String
checkField label name on = concat
  [ "<label class=\"chk\">", hidden name "0", "<input type=\"checkbox\"", attr "name" name
  , " value=\"1\"", if on then " checked" else "", ">", label, "</label>" ]

selectField :: (Show a, Eq a) => String -> String -> [(a, String)] -> a -> String
selectField label name opts cur = concat
  [ "<label>", label, "<select", attr "name" name, ">"
  , concat [option (show v) l (v == cur) | (v, l) <- opts], "</select></label>" ]

elementProps :: Element -> String
elementProps e = concat
  [ "<aside id=\"props\"><form id=\"pf\" method=\"post\" action=\"/act\">"
  , hidden "a" "update", hidden "id" (show (elId e))
  , if hasText then textFields else ""
  , if k == Image then imageFields else ""
  , appearance
  , "<h2>Положение и размер</h2>"
  , "<div class=\"row\">", numberField "X" "x" (-5000) (elX e), numberField "Y" "y" (-5000) (elY e), "</div>"
  , "<div class=\"row\">", numberField "Ширина" "w" (minSide k) (elW e)
  , numberField "Высота" "h" (minSide k) (elH e), "</div>"
  , "<button class=\"apply\">Применить</button></form></aside>\n" ]
  where
    k = elKind e
    hasText = k `elem` [TextBox, Rect, RoundRect, Ellipse, Triangle]
    textFields = concat
      [ "<h2>Текст</h2>"
      , "<textarea id=\"f-text\" name=\"text\" rows=\"4\" aria-label=\"Текст\">", esc (elText e), "</textarea>"
      , "<div class=\"row\">", numberField "Размер" "size" 6 (elSize e)
      , selectField "Шрифт" "font" [(Sans, "Без засечек"), (Serif, "С засечками"), (Mono, "Моноширинный")] (elFont e), "</div>"
      , "<div class=\"row\">"
      , selectField "Выравнивание" "align" [(AlignLeft, "Слева"), (AlignCenter, "По центру"), (AlignRight, "Справа")] (elAlign e)
      , colorField "Цвет текста" "color" (elColor e), "</div>"
      , "<div class=\"row\">", checkField "Жирный" "bold" (elBold e), checkField "Курсив" "italic" (elItalic e), "</div>" ]
    imageFields = concat
      [ "<h2>Картинка</h2><label>Файл или ссылка<input type=\"text\" name=\"src\"", attr "value" (elSrc e), "></label>" ]
    appearance = case k of
      Image -> ""
      Line -> concat
        [ "<h2>Линия</h2><div class=\"row\">", colorField "Цвет" "stroke" (orElse "#444444" (elStroke e))
        , numberField "Толщина" "linew" 1 (elLineW e), "</div>"
        , checkField "Отразить наклон" "flip" (elFlip e) ]
      _ -> concat
        [ "<h2>Оформление</h2>"
        , colorField "Заливка" "fill" (orElse "#cccccc" (elFill e))
        , checkField "Без заливки" "nofill" (null (elFill e))
        , colorField "Контур" "stroke" (orElse "#444444" (elStroke e))
        , checkField "Без контура" "nostroke" (null (elStroke e))
        , numberField "Толщина контура" "linew" 0 (elLineW e)
        , if k == Triangle then checkField "Перевернуть" "flip" (elFlip e) else "" ]
    orElse def v = if null v then def else v

------------------------------------------------------------------------
-- Страница показа и экспортируемый файл

-- | Самодостаточная страница со всеми слайдами. imgSrc выбирает адрес
-- картинки: для показа — адрес сервера, для экспорта — data:-адрес.
standalonePage :: Bool -> Deck -> (Element -> String) -> String
standalonePage isShow d imgSrc = concat
  [ "<!doctype html>\n<html lang=\"ru\"><head><meta charset=\"utf-8\">\n"
  , "<meta name=\"viewport\" content=\"width=device-width,initial-scale=1\">\n"
  , "<title>", esc (dkTitle d), "</title>\n<style>\n", slideCss, showCss, "</style></head>\n"
  , "<body", attr "data-esc" (if isShow then "/" else ""), ">\n"
  , concatMap slideHtml (dkSlides d)
  , "<div id=\"hud\"></div>\n<script>\n", showJs, "</script>\n</body></html>\n" ]
  where
    slideHtml s = concat
      [ "<section class=\"slide\" style=\"background:", slBg s, "\">"
      , concatMap (renderElement imgSrc) (slElems s), "</section>\n" ]
