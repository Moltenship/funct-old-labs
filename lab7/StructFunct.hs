module StructFunct
where

import SupportFunct

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

capL :: [Factory] -> [Int]
capL [] = []
capL listOfFactories = [capacity (head listOfFactories)] ++ capL (tail listOfFactories)

checkPos :: [Factory] -> IO Int
checkPos list = do
  line <- getLine
  n <- toInt line
  if (n > length list) || (n < 0)
    then putStrLn "Wrong value. Try again" >> checkPos list
    else return n

factoryList :: [Factory]
factoryList = [factory1, factory2, factory3, factory4, factory5]

addFactory :: [Factory] -> Factory -> [Factory]
addFactory factoryList added = factoryList ++ [added]

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

addFactoryIO :: [Factory] -> IO [Factory]
addFactoryIO factories = do
  putStrLn "Enter producer: "
  producer <- getLine
  putStrLn "Enter product type: "
  productType <- getLine
  putStrLn "Enter capacity: "
  capacity <- checkValue
  putStrLn ""
  return $ addFactory factories (Factory producer productType capacity)

editFactoryIO :: [Factory] -> IO [Factory]
editFactoryIO factories = do
  putStrLn "Index of element to edit: "
  index <- checkPos factories
  let tempList = factories !! (index - 1)
  putStrLn "Enter producer or press enter to keep current: "
  line <- getLine
  let newProducer = if line == ""
      then producer tempList
      else line
  putStrLn "Enter product type or press enter to keep current: "
  line <- getLine
  let newProductType = if line == ""
      then productType tempList
      else line
  putStrLn "Enter capacity: "
  line <- getLine
  newCapacity <- if line == ""
      then return $ capacity tempList
      else toInt line
  putStrLn ""
  return $ editFactory factories index (Factory newProducer newProductType newCapacity)

deleteFactoryIO :: [Factory] -> IO [Factory]
deleteFactoryIO factories = do
  putStrLn "Enter index of element that you want to delete: "
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
  return $ queryMaximumCapacity factories capList

queryRangeIO :: [Factory] -> IO [Factory]
queryRangeIO factories = do
  putStrLn "Enter start of range: "
  startPoint <- checkValue
  let tmp = startPoint
  putStrLn "Enter end of range: "
  endPoint <- checkValue
  if (startPoint > endPoint)
    then return []
    else return $ queryCapacityRange factories startPoint endPoint

listOfFactories :: [[[Char]]] -> [Factory]
listOfFactories listOfFields
  | length listOfFields == 1 = [(Factory producer productType capacity)]
  | otherwise = (Factory producer productType capacity) : (listOfFactories $ tail listOfFields)
    where
      fac = head listOfFields
      producer = fac !! 0
      productType = fac !! 1
      capacity = read $ fac !! 2
