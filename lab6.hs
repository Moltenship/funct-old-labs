{-   Вариант 2 - Структура данных: предприятие-изготовитель изделия; тип; мощность.
  Создать два запроса, позволяющих найти список предприятий,
   выпускающих изделие максимальной мощности и диапазон мощностей

-}

import Data.IORef
import Data.Typeable
import System.IO

data Factory = Factory {
  producer :: String,
  productType :: String,
  capacity :: Int
} deriving (Show)

factory1 = Factory {
  producer = "Volvo",
  productType = "Engine",
  capacity = 1000
}

factory2 = Factory {
  producer = "Jet",
  productType = "Fan",
  capacity = 300
}

factory3 = Factory {
  producer = "Slow road Inc.",
  productType = "Wheel",
  capacity = 100
}

factory4 = Factory {
  producer = "Sea Cowboys",
  productType = "Pipe",
  capacity = 150
}

factory5 = Factory {
  producer = "SpaceX",
  productType = "Space ship",
  capacity = 1000
}

factoryList :: [Factory]
factoryList = [factory1, factory2, factory3, factory4, factory5]

addFactory :: [Factory] -> Factory -> [Factory]
addFactory factoryList added = factoryList ++ [added]

testAdd :: [Factory]
testAdd = addFactory [factory1, factory2, factory3] factory5

editFactory :: [Factory] -> Int -> Factory -> [Factory]
editFactory factoryList index newFactory
  | index < 1 || index > length factoryList = error "Invalid index"
  | index == 1 = [newFactory] ++ tail factoryList
  | index == length factoryList = init factoryList ++ [newFactory]
  | otherwise = take (index - 1) factoryList ++ [newFactory] ++ drop (index) factoryList

deleteFactory :: [Factory] -> Int -> [Factory]
deleteFactory factoryList index
  | index < 1 || index > length factoryList = error "Invalid index"
  | index == 1 = tail factoryList
  | index == length factoryList = init factoryList
  | otherwise = head factoryList : deleteFactory (tail factoryList) (index - 1)

sortCapacity :: [Factory] -> [Int]
sortCapacity list = qsort $ capL list

capList :: [Int]
capList = [capacity factory1] ++ [capacity factory2] ++ [capacity factory3] ++
          [capacity factory4] ++ [capacity factory5]

qsort :: [Int] -> [Int]
qsort []  = []
qsort (x:xs) = qsort (filter (>= x) xs) ++ [x] ++ qsort (filter (< x) xs)

-- Функция, выводящая самое мощное изделие
queryMaximumCapacity :: [Factory] -> [Int] -> [Factory]
queryMaximumCapacity tempList sortedCapacityList
  | length tempList < 1 = []
  | capacity (head tempList) == head (sortCapacity tempList) = [head tempList] ++ queryMaximumCapacity (tail tempList) (sortCapacity tempList)
  | otherwise = queryMaximumCapacity (tail tempList) (sortCapacity tempList)
  where
    sortedCapacityList = sortCapacity tempList

-- Функция, выводящая элементы списка в выбранном диапазоне мощностей
queryCapacityRange :: [Factory] -> Int -> Int -> [Factory]
queryCapacityRange tempList startPoint endPoint
  | length tempList < 1 = []
  | capacity (head tempList) >= startPoint && capacity (head tempList) <= endPoint = [head tempList] ++ queryCapacityRange (tail tempList) startPoint endPoint
  | otherwise = queryCapacityRange (tail tempList) startPoint endPoint

testQueryCapacityRange :: [Factory]
testQueryCapacityRange = queryCapacityRange [factory5, factory2, factory4, factory3] 100 500

main = do
  mainMenu factoryList

mainMenu :: ([Factory] -> IO ())
mainMenu factories = do
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
      mainMenu factories
    "2" -> do
      factories <- editFactoryIO factories
      mainMenu factories
    "3" -> do
      factories <- deleteFactoryIO factories
      mainMenu factories
    "4" -> do
      if length factories <= 0
        then do
          putStrLn "Empty list"
          mainMenu factories
        else do
          showIO factories
          mainMenu factories
    "5" -> do
      tempList <- queryMaxIO factories
      showIO tempList
      mainMenu factories
    "6" -> do
      tempList <- queryRangeIO factories
      showIO tempList
      mainMenu factories
    "7" -> do
      if length factories > 0
        then do
          saveFile factories
          putStrLn "Data has been written to file"
          mainMenu factories
        else do
          putStrLn "You have nothing to write"
          mainMenu factories
    "8" -> do
      filename <- openFile "factories.txt" ReadMode
      content <- hGetContents filename
      if length content == 0
        then do
          putStrLn "Empty file"
          mainMenu factories
        else do
          let factories = loadFile content
          mainMenu factories
      hClose filename
    "0" -> do
      putStrLn "Closing program..."
    _ -> do
      putStrLn ""
      mainMenu factories

addFactoryIO :: [Factory] -> IO [Factory]
addFactoryIO factories = do
  putStr "Enter producer: "
  producer <- getLine
  putStr "Enter product type: "
  productType <- getLine
  putStr "Enter capacity: "
  capacity <- checkValue
  putStrLn ""
  return $ addFactory factories (Factory producer productType capacity)

editFactoryIO :: [Factory] -> IO [Factory]
editFactoryIO factories = do
  putStr "Index of element to edit: "
  index <- checkPos factories
  let tempList = factories !! (index - 1)
  putStr "Enter producer or press enter to keep current: "
  line <- getLine
  let newProducer = if line == ""
      then producer tempList
      else line
  putStr "Enter product type or press enter to keep current: "
  line <- getLine
  let newProductType = if line == ""
      then productType tempList
      else line
  putStr "Enter capacity: "
  line <- getLine
  newCapacity <- if line == ""
      then return $ capacity tempList
      else toInt line
  putStrLn ""
  return $ editFactory factories index (Factory newProducer newProductType newCapacity)

deleteFactoryIO :: [Factory] -> IO [Factory]
deleteFactoryIO factories = do
  putStr "Enter index of element that you want to delete: "
  index <- checkPos factories
  return $ deleteFactory factories index

showIO :: [Factory] -> IO ()
showIO [] = do
  putStrLn ""
showIO (x:xs) = do
  putStrLn $ "Producer: " ++ (producer x)
  putStrLn $ "Product type: " ++ (productType x)
  putStrLn $ "Capacity: " ++ show (capacity x)
  putStrLn ""
  showIO (xs)

queryMaxIO :: [Factory] -> IO [Factory]
queryMaxIO factories = do
  putStr "Done"
  return $ queryMaximumCapacity factories capList

queryRangeIO :: [Factory] -> IO [Factory]
queryRangeIO factories = do
  putStr "Enter start of range: "
  startPoint <- checkValue
  let tmp = startPoint
  putStr "Enter end of range: "
  endPoint <- checkValue
  if (startPoint > endPoint)
    then return []
    else return $ queryCapacityRange factories startPoint endPoint

saveFile :: [Factory] -> IO ()
saveFile factories = do
  str <- getString factories ""
  writeFile "factories.txt" str
  where
    getString :: [Factory] -> [Char] -> IO [Char]
    getString list str = do
      let fieldDivider = ";"
      let structDivider = ['\n']
      let fList = head list
      let newString = str ++ (producer fList) ++ fieldDivider
                          ++ (productType fList) ++ fieldDivider
                          ++ (show $ capacity fList) ++ structDivider
      if length list == 1
        then return newString
        else getString (tail list) newString

loadFile :: [Char] -> [Factory]
loadFile content = do
  let str = content
  let listStrings = listOfStrings str
  let listFields = listOfFields listStrings
  let listFactories = listOfFactories listFields
  result <- listFactories
  return result

toInt :: [Char] -> IO Int
toInt line = do
  let checker = readMaybe line :: Maybe Int
  case checker of
    Nothing -> do
      putStr "Incorrect number. Thy again: "
      line <- getLine
      toInt line
    Just n -> return n

checkValue :: IO Int
checkValue = do
  line <- getLine
  n <- toInt line
  if n < 0
    then putStr "Wrong value. Try again" >> checkValue
    else return n

checkPos :: [Factory] -> IO Int
checkPos list = do
  line <- getLine
  n <- toInt line
  if (n > length list) || (n < 0)
    then putStr "Wrong value. Try again" >> checkPos list
    else return n

readMaybe :: Read a => [Char] -> Maybe a
readMaybe tmp = case reads tmp of
  [(x, "")] -> Just x
  _ -> Nothing

split :: [Char] -> Char -> [[Char]]
split [] delim = [""]
split (c:cs) delim
  | c == delim = "" : rest
  | otherwise = (c : head rest) : tail rest
  where
    rest = split cs delim

listOfStrings :: [Char] -> [[Char]]
listOfStrings str = init $ split str '\n'

listOfFields :: [[Char]] -> [[[Char]]]
listOfFields [str] = [split str ';']
listOfFields listOfStrings = [split (head listOfStrings) ';'] ++ listOfFields (tail listOfStrings)

listOfFactories :: [[[Char]]] -> [Factory]
listOfFactories listOfFields
  | length listOfFields == 1 = [(Factory producer productType capacity)]
  | otherwise = (Factory producer productType capacity) : (listOfFactories $ tail listOfFields)
    where
      fac = head listOfFields
      producer = fac !! 0
      productType = fac !! 1
      capacity = read $ fac !! 2

capL :: [Factory] -> [Int]
capL [] = []
capL listOfFactories = [capacity (head listOfFactories)] ++ capL (tail listOfFactories)
