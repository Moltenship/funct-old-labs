{-
  Вариант 2 - Структура данных: предприятие-изготовитель изделия; тип; мощность.
  Создать два запроса, позволяющих найти список предприятий,
   выпускающих изделие максимальной мощности и диапазон мощностей
-}

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

-- Функция добавления элемента в список
addFactory :: [Factory] -> Factory -> [Factory]
addFactory factoryList added = factoryList ++ [added]

testAdd :: [Factory]
testAdd = addFactory [factory1, factory2, factory3] factory5

-- Функция замены элемента в списке(модификация)
editFactory :: [Factory] -> Int -> Factory -> [Factory]
editFactory factoryList index newFactory
  | index < 1 || index > length factoryList = error "Invalid index"
  | index == 1 = [newFactory] ++ tail factoryList
  | index == length factoryList = init factoryList ++ [newFactory]
  | otherwise = take (index - 1) factoryList ++ [newFactory] ++ drop (index) factoryList

testEdit :: [Factory]
testEdit = editFactory [factory4, factory3, factory2] 2 factory5

-- Функция удаления элемента из списка
deleteFactory :: [Factory] -> Int -> [Factory]
deleteFactory factoryList index
  | index < 1 || index > length factoryList = error "Invalid index"
  | index == 1 = tail factoryList
  | index == length factoryList = init factoryList
  | otherwise = head factoryList : deleteFactory (tail factoryList) (index - 1)

testDelete :: [Factory]
testDelete = deleteFactory [factory5, factory4, factory1, factory3] 3

-- Функция, содержащая отсортированные элементы capacity
sortCapacity :: [Int]
sortCapacity = qsort capList
-- Функция, содержащая элементы capacity
capList :: [Int]
capList = [capacity factory1] ++ [capacity factory2] ++ [capacity factory3] ++
          [capacity factory4] ++ [capacity factory5]
-- Функция быстрой сортировки
qsort :: [Int] -> [Int]
qsort []  = []
qsort (x:xs) = qsort (filter (>= x) xs) ++ [x] ++ qsort (filter (< x) xs)

-- Функция, выводящая самое мощное изделие
queryMaximumCapacity :: [Factory] -> [Int] -> [Factory]
queryMaximumCapacity tempList sortedCapacityList
  | length tempList < 1 = []
  | capacity (head tempList) == head sortedCapacityList = [head tempList] ++ queryMaximumCapacity (tail tempList) sortedCapacityList
  | otherwise = queryMaximumCapacity (tail tempList) sortedCapacityList

testQueryMaximumCapacity :: [Factory]
testQueryMaximumCapacity = queryMaximumCapacity [factory1, factory4, factory5, factory2, factory3] sortCapacity

-- Функция, выводящая элементы списка в выбранном диапазоне мощностей
queryCapacityRange :: [Factory] -> Int -> Int -> [Factory]
queryCapacityRange tempList startPoint endPoint
  | startPoint < 0 || endPoint < 0 = error "Invalid value"
  | startPoint > endPoint = error "Start point bigger than end point"
  | length tempList < 1 = []
  | capacity (head tempList) >= startPoint && capacity (head tempList) <= endPoint = [head tempList] ++ queryCapacityRange (tail tempList) startPoint endPoint
  | otherwise = queryCapacityRange (tail tempList) startPoint endPoint

testQueryCapacityRange :: [Factory]
testQueryCapacityRange = queryCapacityRange [factory5, factory2, factory4, factory3] 100 500
