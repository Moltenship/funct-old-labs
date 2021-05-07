module SupportFunct
where

toInt :: [Char] -> IO Int
toInt line = do
  let checker = readMaybe line :: Maybe Int
  case checker of
    Nothing -> do
      putStrLn "Incorrect number. Thy again: "
      line <- getLine
      toInt line
    Just n -> return n

checkValue :: IO Int
checkValue = do
  line <- getLine
  n <- toInt line
  if n < 0
    then putStrLn "Wrong value. Try again" >> checkValue
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
