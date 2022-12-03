module Day1 (calcMostCalories) where

import Data.List (sort)

calcMostCalories :: Int -> [String] -> Int
calcMostCalories n input =
  (elvesMostCalories n) $ map sum $ foldr elves [] $ input

elvesMostCalories :: Int -> [Int] -> Int
elvesMostCalories n =
  sum . (lastN n) . sort

type Elve = [Int]

elves :: String -> [Elve] -> [Elve]
elves s [] = [[read s]]
elves "" (x : xs) = ([] : x : xs)
elves n (x : xs) = ((read n) : x) : xs

lastN :: Int -> [a] -> [a]
lastN n xs = drop (length xs - n) xs
