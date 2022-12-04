module DayThree where

import System.Environment
import Data.List
import Data.List.Split
import Data.Char


-- Part 1
getContainedPairs :: [String] -> Int -> Int
getContainedPairs [] pairs = pairs
getContainedPairs (x:xs) pairs
  | hasContained x = getContainedPairs xs (pairs + 1)
  | otherwise      = getContainedPairs xs pairs

hasContained :: String -> Bool
hasContained pair =
  (pair1first <= pair2first && pair1second >= pair2second) || 
  (pair2first <= pair1first && pair2second >= pair1second)
  where
    (firstPair:secondPair:rest) = splitOn "," pair
    pair1first = read ((splitOn "-" firstPair)!!0) :: Int
    pair1second = read ((splitOn "-" firstPair)!!1) :: Int
    pair2first = read ((splitOn "-" secondPair)!!0) :: Int
    pair2second = read ((splitOn "-" secondPair)!!1) :: Int
    


main1 :: IO ()
main1 = do
  i <- readFile "day4/day4_input.txt"
  let split_file = splitOn "\n" i :: [String]
  putStrLn (show $ getContainedPairs split_file 0)

-- Part 2
getOverlappedPairs :: [String] -> Int -> Int
getOverlappedPairs [] pairs = pairs
getOverlappedPairs (x:xs) pairs
  | hasOverlap x = getOverlappedPairs xs (pairs + 1)
  | otherwise      = getOverlappedPairs xs pairs

hasOverlap :: String -> Bool
hasOverlap pair =
  (pair1first <= pair2first && pair1second >= pair2first) || 
  (pair2first <= pair1first && pair2second >= pair1first)
  where
    (firstPair:secondPair:rest) = splitOn "," pair
    pair1first = read ((splitOn "-" firstPair)!!0) :: Int
    pair1second = read ((splitOn "-" firstPair)!!1) :: Int
    pair2first = read ((splitOn "-" secondPair)!!0) :: Int
    pair2second = read ((splitOn "-" secondPair)!!1) :: Int
    


main2 :: IO ()
main2 = do
  i <- readFile "day4/day4_input.txt"
  let split_file = splitOn "\n" i :: [String]
  putStrLn (show $ getOverlappedPairs split_file 0)