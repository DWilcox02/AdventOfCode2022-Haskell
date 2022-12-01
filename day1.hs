module DayOne where

import System.Environment
import Data.List
import Data.List.Split

--- Part 1
getMostCalories :: [String] -> Int -> Int
getMostCalories [] cb = cb
getMostCalories (x:xs) cb
  | cur > cb = getMostCalories xs cur
  | otherwise = getMostCalories xs cb
  where
    cur = groupToCalories (splitOn "\n" x) 0
    groupToCalories :: [String] -> Int -> Int
    groupToCalories [] accum = accum
    groupToCalories (s:ss) accum
      = groupToCalories ss (accum + (read s))


main1 :: String -> IO ()
main1 source = do
  i <- readFile source
  let split_file = splitOn "\n\n" i :: [String]
  putStrLn (show $ getMostCalories split_file 0)

--- Part 2

getMostCalories' :: [String] -> [Int] -> [Int]
getMostCalories' [] top3 = top3
getMostCalories' (x:xs) top3 
  = getMostCalories' xs (tail $ insert cur top3)
  where
    cur = groupToCalories (splitOn "\n" x) 0
    groupToCalories :: [String] -> Int -> Int
    groupToCalories [] accum = accum
    groupToCalories (s:ss) accum
      = groupToCalories ss (accum + (read s))

main2 :: String -> IO ()
main2 source = do
  i <- readFile source
  let split_file = splitOn "\n\n" i :: [String]
  putStrLn (show $ getMostCalories' split_file [0, 0, 0])