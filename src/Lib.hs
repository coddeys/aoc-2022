module Lib (solveDay1, solveDay2) where

import Control.Applicative
import Data.Bifunctor
import Data.Bitraversable
import Data.Maybe (catMaybes)
import Day1 (calcMostCalories)

solveDay1 :: FilePath -> IO ()
solveDay1 fp = do
  calories <- (calcMostCalories 3) <$> readLines fp
  print (calories)

solveDay2 :: FilePath -> IO ()
solveDay2 fp = do
  rounds <- calcTotal <$> readLines fp
  print (rounds)

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

data Result
  = Draw
  | First
  | Second

resultToScore :: Result -> (Int, Int)
resultToScore Draw = (3, 3)
resultToScore First = (6, 0)
resultToScore Second = (0, 6)

data Shape
  = Rock
  | Paper
  | Scissors
  deriving (Show)

toScore :: Shape -> Int
toScore Rock = 1
toScore Paper = 2
toScore Scissors = 3

calcTotal :: [String] -> Int
calcTotal = sum . map calcScores . shapes

calcScores :: (Shape, Shape) -> (Int)
calcScores x =
  combine
    ((resultToScore . play) x)
    (bimap toScore toScore x)
  where
    combine (_, xb) (_, yb) = (xb + yb)

play :: (Shape, Shape) -> Result
play (Rock, Rock) = Draw
play (Rock, Paper) = Second
play (Rock, Scissors) = First
play (Paper, Rock) = First
play (Paper, Paper) = Draw
play (Paper, Scissors) = Second
play (Scissors, Rock) = Second
play (Scissors, Paper) = First
play (Scissors, Scissors) = Draw

shapes :: [String] -> [(Shape, Shape)]
shapes xs = catMaybes $ map shape xs

shape :: String -> Maybe (Shape, Shape)
shape (x : ' ' : y : []) = bisequenceA (fs, secondShape fs y)
  where
    fs = firstShape x
shape _ = Nothing

firstShape :: Char -> Maybe Shape
firstShape 'A' = Just Rock
firstShape 'B' = Just Paper
firstShape 'C' = Just Scissors
firstShape _ = Nothing

secondShape :: Maybe Shape -> Char -> Maybe Shape
secondShape fs c =
  liftA2 toShape fs result
  where
    result = secondColumn c

secondColumn :: Char -> Maybe Result
secondColumn 'X' = Just First
secondColumn 'Y' = Just Draw
secondColumn 'Z' = Just Second
secondColumn _ = Nothing

toShape :: Shape -> Result -> Shape
toShape Rock Draw = Rock
toShape Rock Second = Paper
toShape Rock First = Scissors
toShape Paper First = Rock
toShape Paper Draw = Paper
toShape Paper Second = Scissors
toShape Scissors Second = Rock
toShape Scissors First = Paper
toShape Scissors Draw = Scissors
