fibonacci :: [Int] -> Int -> [Int]
fibonacci [0] num = take num [x | x <- [0,0..]]
fibonacci step num = take num [x | x <- fibonacci' (listOfOnes step') step']
    where step' = head step
          listOfOnes 0 = []
          listOfOnes step = 1 : listOfOnes (step - 1)

fibonacci' :: [Int] -> Int -> [Int]
fibonacci' xs step =  head xs : fibonacci' (tail xs ++ [summ xs step]) step
    where summ xs 0 = 0
          summ xs step = last xs + summ (init xs) (step - 1)
