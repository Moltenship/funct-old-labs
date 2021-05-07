{-
  Вариант 2 - Структура данных: предприятие-изготовитель изделия; тип; мощность.
  Создать два запроса, позволяющих найти список предприятий,
  выпускающих изделие максимальной мощности и диапазон мощностей
-}


data Factories a = Factory String String a deriving (Show, Eq, Ord)

instance Functor Factories where
    fmap f (Factory a b c) = Factory a b (f c)

instance Applicative Factories where
    pure a = Factory "" "" a
    Factory t e f <*> Factory b c a  = Factory b c (f a)

factory1 :: Factories Int
factory1 = Factory "Volvo" "Engine" 1000
factory2 :: Factories Int
factory2 = Factory "dsad" "gdsf" 1000
factory3 :: Factories Int
factory3 = Factory "beka" "eqeq" 400
factory4 :: Factories Int
factory4 = Factory "zaza" "xexe" 500
factory5 :: Factories Int
factory5 = Factory "Shooth" "sassss" 100

factoryList :: [Factories Int]
factoryList = [factory1, factory2, factory3, factory4, factory5]

-- Функция добавления элемента в список
addFactory :: [Factories Int] -> Factories Int -> [Factories Int]
addFactory factoryList added = factoryList ++ [added]

testAdd :: [Factories Int]
testAdd = addFactory [factory1, factory2, factory3] factory5

-- Функция замены элемента в списке(модификация)
editFactory :: [Factories Int] -> Int -> Factories Int -> [Factories Int]
editFactory factoryList index newFactory
  | index < 1 || index > length factoryList = error "Invalid index"
  | index == 1 = [newFactory] ++ tail factoryList
  | index == length factoryList = init factoryList ++ [newFactory]
  | otherwise = take (index - 1) factoryList ++ [newFactory] ++ drop (index) factoryList

testEdit :: [Factories Int]
testEdit = editFactory [factory4, factory3, factory2] 2 factory5

-- Функция удаления элемента из списка
deleteFactory :: [Factories Int] -> Int -> [Factories Int]
deleteFactory factoryList index
  | index < 1 || index > length factoryList = error "Invalid index"
  | index == 1 = tail factoryList
  | index == length factoryList = init factoryList
  | otherwise = head factoryList : deleteFactory (tail factoryList) (index - 1)

testDelete :: [Factories Int]
testDelete = deleteFactory [factory5, factory4, factory1, factory3] 3

qSortFromCap :: [Factories Int] -> [Factories Int]
qSortFromCap [] = []
qSortFromCap (x:xs) = qSortFromCap (filter (condL x) xs) ++ [x] ++ qSortFromCap (filter (condB x) xs)
    where condL (Factory _ _ a) (Factory _ _ b) = a <= b
          condB (Factory _ _ a) (Factory _ _ b) = a > b

queryMaximumCapacity :: [Factories Int] -> [Factories Int]
queryMaximumCapacity [] = []
queryMaximumCapacity (x:xs) = [x] ++ otherFact
    where otherFact = filter (condE x) xs
            where condE (Factory _ _ a) (Factory _ _ b) = a == b

test = queryMaximumCapacity $ qSortFromCap factoryList

-- Функция, выводящая элементы списка в выбранном диапазоне мощностей
queryCapacityRange :: [Factories Int] -> Int -> Int -> [Factories Int]
tempList [] = []
queryCapacityRange tempList startPoint endPoint = filter condCap tempList
    where condCap (Factory _ _ a) = (a >= startPoint) && (a <= endPoint)

testCap = queryCapacityRange factoryList 100 500
