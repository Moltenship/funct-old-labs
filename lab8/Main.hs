{-
Программа принимает от пользователя две строки, осуществляет
поиск количества вхождений второй строки в первую любым известным
методом, кроме прямого (линейного, грубой силы), и выводит на экран
полученное значение.
-}
import Data.List (tails, stripPrefix)
import Data.Maybe (catMaybes)
import Control.Concurrent (forkIO, threadDelay)
import Data.Foldable (for_)
import System.Environment
import OccurrenceFunct

main = do
  (argv) <- getArgs
  if (length argv == 0)
    then do putStrLn "No arguments. The first arg is pattern, the second is text"
    else if (length argv > 2 || length argv < 2)
      then do putStrLn "Expected 2 arguments"
      else do
        let pat = argv !! 0
        let text = argv !! 1
        if (pat == "" || text == "")
            then do putStrLn "Empty string"
            else do
                forkIO (do
                    putStrLn $ "NUMBER OF OCCURRENCES " ++ show (count pat text))
                putStrLn ""
