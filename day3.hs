module DayThree where

import System.Environment
import Data.List
import Data.List.Split
import Data.Char

-- Part 1
parseBags :: [String] -> [Int]
parseBags [] = []
parseBags (x:xs) = getPriority priority : parseBags xs
  where
    firstBag = take ((length x) `div` 2) x
    secondBag = drop ((length x) `div` 2) x
    priority = checkRepeatingElement firstBag secondBag
checkRepeatingElement :: String -> String -> Char
checkRepeatingElement (y:ys) str
    | y `elem` str = y
    | otherwise    = checkRepeatingElement ys str 
getPriority :: Char -> Int
getPriority z
    | ord z >= 97 = (ord z) - 96
    | otherwise   = (ord z) - 64 + 26

main1 :: IO ()
main1 = do
  i <- readFile "day3_input.txt"
  let split_file = splitOn "\n" i :: [String]
  putStrLn (show $ sum $ parseBags split_file)

-- part 2

parseBags' :: [String] -> [Int]
parseBags' [] = []
parseBags' bags
  = getPriority item : parseBags' (drop 3 bags)
  where
    group = take 3 bags
    item = checkRepeatingElement' group
checkRepeatingElement' :: [String] -> Char
checkRepeatingElement' ((x:xs):b:c:rest)
  | x `elem` b && x `elem` c = x
  | otherwise                = checkRepeatingElement' [xs,b,c]


main2 :: IO ()
main2 = do
  i <- readFile "day3_input.txt"
  let split_file = splitOn "\n" i :: [String]
  putStrLn (show $ sum $ parseBags' split_file)