module DaySix where

import System.Environment
import Data.List
import Data.List.Split
import Data.Char

-- Part 1
fourDifferent :: String -> Int -> Int
fourDifferent (line:lines) index
  | length (nub firstFour) == 4 = index + 4
  | otherwise                   = fourDifferent lines (index + 1)
  where
    firstFour = take 4 (line:lines)

main1 :: IO ()
main1 = do
  i <- readFile "day6/day6_input.txt"
  putStrLn (show $ fourDifferent i 0)

-- Part 2
fourteenDifferent :: String -> Int -> Int
fourteenDifferent (line:lines) index
  | length (nub firstFourteen) == 14 = index + 14
  | otherwise                   = fourteenDifferent lines (index + 1)
  where
    firstFourteen = take 14 (line:lines)

main2 :: IO ()
main2 = do
  i <- readFile "day6/day6_input.txt"
  putStrLn (show $ fourteenDifferent i 0)