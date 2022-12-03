module Lib (solveDay1) where

import Data.List (sort)

solveDay1 :: FilePath -> IO ()
solveDay1 fp = do
  calories <- (elvesMostCalories 3) <$> readCalories fp
  print calories

elvesMostCalories :: Int -> [Int] -> Int
elvesMostCalories n =
  sum . (lastN n) . sort

readCalories :: FilePath -> IO [Int]
readCalories fp = map sum <$> foldr elves [] <$> readLines fp

type Elve = [Int]

elves :: String -> [Elve] -> [Elve]
elves s [] = [[read s]]
elves "" (x : xs) = ([] : x : xs)
elves n (x : xs) = ((read n) : x) : xs

lastN :: Int -> [a] -> [a]
lastN n xs = drop (length xs - n) xs

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile
