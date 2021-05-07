{-   Вариант 2 - Структура данных: предприятие-изготовитель изделия; тип; мощность.
  Создать два запроса, позволяющих найти список предприятий,
   выпускающих изделие максимальной мощности и диапазон мощностей
-}
import System.Environment
import System.IO
import System.IO.Error
import Control.Exception
import StructFunct
import SupportFunct
import FileFunct

main = do
    argv <- getArgs
    if (length argv == 0)
        then putStrLn "You have to enter 2 agruments: input file and output file"
        else if (length argv > 2 || length argv < 2)
          then putStrLn "You need only 2 arguments"
          else toTry `catch` handler

mainMenu :: ([Factory] -> FilePath -> FilePath -> IO ())
mainMenu factories inputFile outFile = do
  let tempList = factories
  putStrLn "1. Add"
  putStrLn "2. Edit"
  putStrLn "3. Delete"
  putStrLn "4. Show all"
  putStrLn "5. Query factories with maximum capacity"
  putStrLn "6. Query factories within selected range"
  putStrLn "7. Save to file"
  putStrLn "8. Load from file"
  putStrLn "0. Close program"
  choice <- getLine
  case choice of
    "1" -> do
      factories <- addFactoryIO factories
      mainMenu factories inputFile outFile
    "2" -> do
      factories <- editFactoryIO factories
      mainMenu factories inputFile outFile
    "3" -> do
      factories <- deleteFactoryIO factories
      mainMenu factories inputFile outFile
    "4" -> do
      if length factories <= 0
        then do
          putStrLn "Empty list"
          mainMenu factories inputFile outFile
        else do
          showIO factories
          mainMenu factories inputFile outFile
    "5" -> do
      tempList <- queryMaxIO factories
      showIO tempList
      mainMenu factories inputFile outFile
    "6" -> do
      tempList <- queryRangeIO factories
      showIO tempList
      mainMenu factories inputFile outFile
    "7" -> do
      if length factories > 0
        then do
          saveFile factories outFile
          putStrLn "Data has been written to file"
          mainMenu factories inputFile outFile
        else do
          putStrLn "You have nothing to write"
          mainMenu factories inputFile outFile
    "8" -> do
      filename <- openFile inputFile ReadMode
      content <- hGetContents filename
      if length content == 0
        then do
          putStrLn "Empty file"
          mainMenu factories inputFile outFile
        else do
          let factories = loadFile content
          mainMenu factories inputFile outFile
      hClose filename
    "0" -> do
      putStrLn "Closing program..."
    _ -> do
      putStrLn ""
      mainMenu factories inputFile outFile

toTry :: IO ()
toTry = do
    (inputFile:outFile:_) <- getArgs
    filename <- openFile inputFile ReadMode
    hClose filename
    mainMenu factoryList inputFile outFile

handler :: IOError -> IO ()
handler e
    | isDoesNotExistError e = putStrLn "The file doesn't exist!"
    | otherwise = ioError e
