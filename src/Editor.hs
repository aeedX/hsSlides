-- | Состояние редактора (текущий слайд, выделение, история) и обработка
-- действий, пришедших из браузера.
module Editor
  ( Editor (..), newEditor, applyAction
  , goSlide, pickElement, clearSelection, selectedElement
  , canUndo, canRedo
  ) where

import Data.Char (toLower)
import Data.Maybe (fromMaybe)
import System.Directory (doesFileExist, getFileSize)

import Deck

data Editor = Editor
  { edDeck  :: Deck
  , edSlide :: Int          -- ^ номер текущего слайда (с нуля)
  , edSel   :: Maybe Int    -- ^ номер выделенного элемента
  , edUndo  :: [Deck]
  , edRedo  :: [Deck]
  , edMsg   :: String       -- ^ сообщение, которое показывается один раз
  , edFile  :: FilePath
  }

newEditor :: FilePath -> Deck -> Editor
newEditor file d = Editor
  { edDeck = d, edSlide = 0, edSel = Nothing
  , edUndo = [], edRedo = [], edMsg = "", edFile = file }

canUndo, canRedo :: Editor -> Bool
canUndo = not . null . edUndo
canRedo = not . null . edRedo

-- | Номер слайда в допустимых границах, выделение — только на текущем слайде.
normalize :: Editor -> Editor
normalize ed = ed { edSlide = si, edSel = sel }
  where
    n = length (dkSlides (edDeck ed))
    si = max 0 (min (n - 1) (edSlide ed))
    sel = case edSel ed of
      Just i | any ((== i) . elId) (slideElems si (edDeck ed)) -> Just i
      _ -> Nothing

goSlide :: Int -> Editor -> Editor
goSlide i ed = normalize (ed { edSlide = i, edSel = Nothing })

pickElement :: Int -> Editor -> Editor
pickElement i ed = normalize (ed { edSel = Just i })

clearSelection :: Editor -> Editor
clearSelection ed = ed { edSel = Nothing }

selectedElement :: Editor -> Maybe Element
selectedElement ed = do
  i <- edSel ed
  case filter ((== i) . elId) (slideElems (edSlide ed) (edDeck ed)) of
    (e : _) -> Just e
    [] -> Nothing

-- | Применяет изменение к документу и запоминает прежнюю версию для отмены.
commit :: (Deck -> Deck) -> Editor -> Editor
commit f ed
  | d' == d = ed
  | otherwise = ed { edDeck = d', edUndo = take 200 (d : edUndo ed), edRedo = [] }
  where
    d = edDeck ed
    d' = f d

undo, redo :: Editor -> Editor
undo ed = case edUndo ed of
  (d : rest) -> ed { edDeck = d, edUndo = rest, edRedo = edDeck ed : edRedo ed }
  [] -> ed
redo ed = case edRedo ed of
  (d : rest) -> ed { edDeck = d, edRedo = rest, edUndo = edDeck ed : edUndo ed }
  [] -> ed

-- | Обрабатывает одно действие (параметр "a" формы).
applyAction :: [(String, String)] -> Editor -> IO Editor
applyAction p ed0 = normalize <$> run (get "a")
  where
    ed = ed0 { edMsg = "" }
    d = edDeck ed
    si = edSlide ed
    n = length (dkSlides d)
    get k = lookupLast k p
    geti k = get k >>= readInt
    onSlide i = any ((== i) . elId) (slideElems si d)

    run (Just "add") = case get "kind" >>= readEnum of
      Just Image -> addImage
      Just k -> pure (addKind k id)
      Nothing -> pure ed
    run (Just "update") = pure $ case geti "id" of
      Just i | onSlide i ->
        (commit (updateElement i (applyForm p)) ed) { edSel = Just i }
      _ -> ed
    run (Just "geom") = pure $
      case (geti "id", geti "x", geti "y", geti "w", geti "h") of
        (Just i, Just x, Just y, Just w, Just h) | onSlide i ->
          (commit (updateElement i (\e -> e { elX = x, elY = y, elW = w, elH = h })) ed)
            { edSel = Just i }
        _ -> ed
    run (Just "nudge") = pure $ case (edSel ed, geti "dx", geti "dy") of
      (Just i, Just dx, Just dy) ->
        commit (updateElement i (\e -> e { elX = elX e + dx, elY = elY e + dy })) ed
      _ -> ed
    run (Just "del") = pure $ case edSel ed of
      Just i -> (commit (removeElement i) ed) { edSel = Nothing }
      Nothing -> ed
    run (Just "dup") = pure $ case edSel ed of
      Just i ->
        let (d', new) = duplicateElement i d
        in (commit (const d') ed) { edSel = new }
      Nothing -> ed
    run (Just "front") = pure (withSel bringToFront)
    run (Just "back") = pure (withSel sendToBack)
    run (Just "slide-add") =
      let layout = fromMaybe LTitleContent (get "layout" >>= readEnum)
      in pure (commit (insertSlide si layout) ed) { edSlide = si + 1, edSel = Nothing }
    run (Just "slide-dup") =
      pure (commit (duplicateSlide si) ed) { edSlide = si + 1, edSel = Nothing }
    run (Just "slide-del") =
      pure (commit (deleteSlide si) ed) { edSel = Nothing }
    run (Just "slide-up")
      | si > 0 = pure (commit (moveSlide si (si - 1)) ed) { edSlide = si - 1 }
    run (Just "slide-down")
      | si < n - 1 = pure (commit (moveSlide si (si + 1)) ed) { edSlide = si + 1 }
    run (Just "undo") = pure (undo ed) { edSel = Nothing }
    run (Just "redo") = pure (redo ed) { edSel = Nothing }
    run (Just "title") = pure $ case trim (fromMaybe "" (get "title")) of
      "" -> ed
      t -> commit (\dk -> dk { dkTitle = take 120 t }) ed
    run (Just "bg") = pure $ case get "bg" of
      Just c | validColor c ->
        commit (updateSlide si (\s -> s { slBg = map toLower c })) ed
      _ -> ed
    run _ = pure ed

    withSel f = case edSel ed of
      Just i -> commit (f i) ed
      Nothing -> ed

    -- Новый элемент ставится со сдвигом, чтобы не лечь точно поверх прежнего.
    addKind k tweak =
      let off = 30 * (length (slideElems si d) `mod` 6)
          shift e = tweak (e { elX = elX e + off, elY = elY e + off })
          (d', i) = addElement si k shift d
      in (commit (const d') ed) { edSel = Just i }

    addImage = do
      let src = maybe "" trim (get "src")
      ok <- imageUsable src
      pure $ if ok
        then addKind Image (\e -> e { elSrc = src })
        else ed { edMsg = "Не удалось вставить картинку: " ++ take 80 src
                       ++ ". Нужен существующий файл .png, .jpg, .gif, .webp, .svg или .bmp"
                       ++ " размером до 50 МБ либо ссылка, которая начинается с http:// или https://." }

imageUsable :: String -> IO Bool
imageUsable src
  | null src = pure False
  | isUrl src = pure True
  | hasImageExt src = do
      exists <- doesFileExist src
      if exists then (<= maxImageBytes) <$> getFileSize src else pure False
  | otherwise = pure False

-- | Применяет поля формы свойств к элементу. Отсутствующее поле не меняет значение.
applyForm :: [(String, String)] -> Element -> Element
applyForm p e = e
  { elText   = maybe (elText e) (filter (/= '\r')) (get "text")
  , elSize   = int "size" (elSize e)
  , elFont   = enum "font" (elFont e)
  , elAlign  = enum "align" (elAlign e)
  , elBold   = flag "bold" (elBold e)
  , elItalic = flag "italic" (elItalic e)
  , elFlip   = flag "flip" (elFlip e)
  , elColor  = fromMaybe (elColor e) (colorOf "color")
  , elFill   = optColor "nofill" "fill" (elFill e)
  , elStroke = optColor "nostroke" "stroke" (elStroke e)
  , elLineW  = int "linew" (elLineW e)
  , elX      = int "x" (elX e)
  , elY      = int "y" (elY e)
  , elW      = int "w" (elW e)
  , elH      = int "h" (elH e)
  , elSrc    = maybe (elSrc e) trim (get "src")
  }
  where
    get k = lookupLast k p
    int k def = fromMaybe def (get k >>= readInt)
    enum k def = fromMaybe def (get k >>= readEnum)
    flag k def = case get k of
      Just "1" -> True
      Just _ -> False
      Nothing -> def
    colorOf k = case get k of
      Just c | validColor c -> Just (map toLower c)
      _ -> Nothing
    optColor offKey colKey def = case get offKey of
      Just "1" -> ""
      _ -> fromMaybe def (colorOf colKey)
