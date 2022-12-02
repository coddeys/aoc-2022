module Lib (solveDay1) where

import Data.List (sort)

solveDay1 :: FilePath -> IO ()
solveDay1 fp = do
  inputLines <- readLines fp
  print inputLines

type Elve = [Int]

readLines :: FilePath -> IO (Int)
readLines fp = sum <$> lastN 3 <$> sort <$> map sum <$> foldr elves [] <$> lines <$> readFile fp

elves :: String -> [Elve] -> [Elve]
elves s [] = [[read s]]
elves "" (x : xs) = ([] : x : xs)
elves n (x : xs) = ((read n) : x) : xs

lastN :: Int -> [a] -> [a]
lastN n xs = drop (length xs - n) xs
