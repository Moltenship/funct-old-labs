length' :: [a] -> Int
length' xs = sum[1 | _ <- xs]

longer_than :: [a] -> [b] -> Bool
longer_than xs xs1 =
    if (length' xs) > (length' xs1)
    then True
    else False
