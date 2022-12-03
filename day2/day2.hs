module DAyTwo where

import System.Environment
import Data.List
import Data.List.Split

-- Part 1
scores :: [String] -> [Int]
scores [] = []
scores (x:xs) = (getValue mine) + (getWinner (opp, mine)) : scores xs
  where
    (opp, mine) = readMoves x
    readMoves :: String -> (Char, Char)
    readMoves str = (head str, head $ drop 2 str)
    getValue :: Char -> Int
    getValue 'X' = 1 -- Rock
    getValue 'Y' = 2 -- Paper
    getValue 'Z' = 3 -- Scissors
    getWinner :: (Char, Char) -> Int
    getWinner ('A', mine)
      | mine == 'X' = 3 -- Rock Rock
      | mine == 'Y' = 6 -- Rock Paper
      | otherwise   = 0 -- Rock Scissors
    getWinner ('B', mine)
      | mine == 'X' = 0 -- Paper Rock
      | mine == 'Y' = 3 -- Paper Paper
      | otherwise   = 6
    getWinner ('C', mine)
      | mine == 'X' = 6
      | mine == 'Y' = 0
      | otherwise   = 3

main1 :: String -> IO ()
main1 source = do
  i <- readFile source
  let split_file = splitOn "\n" i :: [String]
  putStrLn (show $ sum (scores split_file))

-- Part 2
scores' :: [String] -> [Int]
scores' [] = []
scores' (x:xs) = getMove (opp, mine) : scores' xs
  where
    (opp, mine) = readMoves x
    readMoves :: String -> (Char, Char)
    readMoves str = (head str, head $ drop 2 str)
    getMove :: (Char, Char) -> Int -- (opp, mine)
    getMove (opp, 'X') -- Lose
      | opp == 'A' = 3
      | opp == 'B' = 1
      | otherwise  = 2
    getMove (opp, 'Y') -- Draw
      | opp == 'A' = 3 + 1
      | opp == 'B' = 3 + 2
      | otherwise  = 3 + 3
    getMove (opp, 'Z') -- Win
      | opp == 'A' = 6 + 2
      | opp == 'B' = 6 + 3
      | otherwise  = 6 + 1

main2 :: String -> IO ()
main2 source = do
  i <- readFile source
  let split_file = splitOn "\n" i :: [String]
  putStrLn (show $ sum (scores' split_file))