module Lib (solveDay1) where

solveDay1 :: FilePath -> IO ()
solveDay1 fp = do
  inputLines <- readLines fp
  print inputLines

type Elve = [Int]

readLines :: FilePath -> IO (Int)
readLines fp = maximum' <$> map sum <$> foldr elves [] <$> lines <$> readFile fp

elves :: String -> [Elve] -> [Elve]
elves s [] = [[read s]]
elves "" (x : xs) = ([] : x : xs)
elves n (x : xs) = ((read n) : x) : xs

maximum' :: Ord a => [a] -> a
maximum' = foldr1 (\x y -> if x >= y then x else y)
