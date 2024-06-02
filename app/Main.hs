{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.GI.Base
import qualified GI.Gtk as Gtk
import Data.Maybe (isJust, listToMaybe, mapMaybe)
import Data.List (nub, transpose, find)
import Control.Monad (forM_)
import Data.Text (pack, unpack)
import qualified Data.Text as T
import Data.Char (isDigit)
import qualified GI.Gdk as Gdk

type Sudoku = [[Int]]

-- | Функция проверки допустимости введенного значения(допустимы значения от 1 до 9)
-- \param n Число для проверки
-- \return True, если значение допустимо, False в противном случае
isValidInput :: Int -> Bool
isValidInput n = n >= 1 && n <= 9

-- | Функция проверки корректности решения судоку
-- \param board Судоку
-- \return True, если решение корректно, False в противном случае
isValidSolution :: Sudoku -> Bool
isValidSolution board =
  all isValidRow board &&
  all isValidRow (transpose board) &&
  all isValidRow (getAllSquares board)
  where
  -- | Возвращает все 3x3 квадраты судоку
  -- \param board Доска судоку
  -- \return Список всех квадратов 3x3
    getAllSquares :: Sudoku -> [[Int]]
    getAllSquares board = [concatMap (take 3 . drop c) (take 3 $ drop r board) | r <- [0,3,6], c <- [0,3,6]]

-- | Функция проверки корректности строки судоку
-- \param row Строка судоку
-- \return True, если строка корректна, False в противном случае
isValidRow :: [Int] -> Bool
isValidRow row =
  let nums = filter (/= 0) row
  in length nums == length (nub nums)

-- | Функция поиска первой пустой ячейки судоку
-- \param board Судоку
-- \return Координаты первой пустой ячейки или Nothing, если таких ячеек нет
findEmptyCell :: Sudoku -> Maybe (Int, Int)
findEmptyCell board =
  listToMaybe [(row, col) | row <- [0..8], col <- [0..8], board !! row !! col == 0]

-- | Функция установки значения в указанную ячейку судоку.
-- \param board Судоку
-- \param row Номер строки
-- \param col Номер столбца
-- \param value Значение для установки
-- \return Новое судоку с установленным значением
setCell :: Sudoku -> Int -> Int -> Int -> Sudoku
setCell board row col value =
  take row board ++ [take col (board !! row) ++ [value] ++ drop (col + 1) (board !! row)] ++ drop (row + 1) board

-- | Функция решения судоку
-- \param board Судоку
-- \return Решенное судоку или Nothing, если решение невозможно
solveSudoku :: Sudoku -> Maybe Sudoku
solveSudoku board =
  case findEmptyCell board of
    Nothing -> Just board
    Just (row, col) ->
      let possibleValues = filter (isValidValue board row col) [1..9]
      in listToMaybe $ mapMaybe (\value -> solveSudoku (setCell board row col value)) possibleValues

-- | Функция проверки допустимости значения для указанной ячейки
-- \param board Судоку
-- \param row Номер строки
-- \param col Номер столбца
-- \param value Значение для проверки
-- \return True, если значение допустимо, False в противном случае
isValidValue :: Sudoku -> Int -> Int -> Int -> Bool
isValidValue board row col value =
  let rowValid = notElem value (board !! row)
      colValid = notElem value ((transpose board) !! col)
      squareValid = let startRow = 3 * (row `div` 3)
                        startCol = 3 * (col `div` 3)
                        square = concatMap (take 3 . drop startCol) (take 3 $ drop startRow board)
                    in notElem value square
  in rowValid && colValid && squareValid

-- | Главная функция программы
main :: IO ()
main = do
  _ <- Gtk.init Nothing
  window <- new Gtk.Window [ #title := "Sudoku Solver"]

  on window #destroy Gtk.mainQuit

  screen <- Gtk.windowGetScreen window
  provider <- Gtk.cssProviderNew
  Gtk.cssProviderLoadFromPath provider "style.css"
  Gtk.styleContextAddProviderForScreen screen provider (fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_USER)

  grid <- new Gtk.Grid [ #columnSpacing := 2, #rowSpacing := 2 ]
  #add window grid

  entries <- sequence [ sequence [ createEntry | _ <- [1..9] ] | _ <- [1..9] ]
  
  mapM_ (\(row, cols) -> mapM_ (\(col, entry) -> Gtk.gridAttach grid entry col row 1 1) (zip [0..] cols)) (zip [0..] entries)

  buttonSolve <- new Gtk.Button [ #label := "Решить" ]
  buttonClear <- new Gtk.Button [ #label := "Очистить" ]
  messageLabel <- new Gtk.Label [ #label := "", #halign := Gtk.AlignCenter ]

  context <- Gtk.widgetGetStyleContext messageLabel
  Gtk.styleContextAddClass context "error-message"

  Gtk.gridAttach grid buttonSolve 0 9 4 1
  Gtk.gridAttach grid messageLabel 4 9 1 1
  Gtk.gridAttach grid buttonClear 5 9 4 1

  -- | Обработчик нажатия кнопки "Решить"
  on buttonSolve #clicked $ do
      putStrLn "Solve button pressed"
      result <- extractSudoku entries
      case result of
          Left errorMsg -> Gtk.labelSetText messageLabel (pack errorMsg)
          Right sudoku -> do
              let solvedSudoku = solveSudoku sudoku
              case solvedSudoku of
                  Just solution -> do
                      Gtk.labelSetText messageLabel ""
                      updateEntries entries solution
                  Nothing -> Gtk.labelSetText messageLabel "Решение не найдено"

  -- | Обработчик нажатия кнопки "Очистить"
  on buttonClear #clicked $ do
      putStrLn "Clear button pressed"
      clearEntries entries
      Gtk.labelSetText messageLabel ""

  #showAll window
  Gtk.main

-- | Функция создания новой текстовой ячейки (ячейки ввода)
-- \return Текстовая ячейка
createEntry :: IO Gtk.Entry
createEntry = do
    entry <- new Gtk.Entry [ #maxWidthChars := 1, #inputPurpose := Gtk.InputPurposeDigits ]
    Gtk.widgetSetSizeRequest entry 10 10
    context <- Gtk.widgetGetStyleContext entry
    Gtk.styleContextAddClass context "sudoku-entry"
    return entry

-- | Функция извлечения значения из текстовых ячеек
-- \param entries Список текстовых ячеек
-- \return Либо сообщение об ошибке ввода, либо матрица значений судоку
extractSudoku :: [[Gtk.Entry]] -> IO (Either String Sudoku)
extractSudoku entries = do
    values <- mapM (mapM getEntryValue) entries
    let invalidEntries = [value | Left value <- concat values]
    if null invalidEntries
        then return $ Right [ [ x | Right x <- row ] | row <- values ]
        else return $ Left $ "Некорректный ввод: " ++ unwords invalidEntries
  where
  -- | Функция извлечения значений из текстовой ячейки
  -- \param entry Текстовая ячейка
  -- \return Либо сообщение об ошибке, либо значение
    getEntryValue entry = do
        text <- Gtk.entryGetText entry
        let value = if T.null text then 0 else read (T.unpack text) :: Int
        if isValidInput value || value == 0
            then return $ Right value
            else return $ Left (T.unpack text)

-- | Функция обновления текстовых ячеек значениями судоку
-- \param entries Список текстовых ячеек
-- \param sudoku Матрица значений судоку
updateEntries :: [[Gtk.Entry]] -> Sudoku -> IO ()
updateEntries entries solution = forM_ (zip entries solution) $ \(rowEntries, rowValues) ->
    forM_ (zip rowEntries rowValues) $ \(entry, value) -> do
        text <- Gtk.entryGetText entry
        let userValue = if T.null text then 0 else read (T.unpack text) :: Int
        if userValue == 0 && value /= 0
          then do
            Gtk.entrySetText entry (pack $ show value)
            context <- Gtk.widgetGetStyleContext entry
            Gtk.styleContextAddClass context "solver-added"
          else
            return ()

-- | Функция очищения всех текстовых ячеек судоку
-- \param entries Список текстовых ячеек
clearEntries :: [[Gtk.Entry]] -> IO ()
clearEntries entries = forM_ entries $ \rowEntries ->
    forM_ rowEntries $ \entry -> do
        Gtk.entrySetText entry ""
        context <- Gtk.widgetGetStyleContext entry
        Gtk.styleContextRemoveClass context "solver-added"
