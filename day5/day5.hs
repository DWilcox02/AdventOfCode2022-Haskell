module DayFive where

import System.Environment
import Data.List
import Data.List.Split
import Data.Char
import Data.Function (on)

-- Part 1
type Stack = (Int, String)
type Index = Int
type Location = Int

getStacks :: [String] -> [(Index, Location)] -> [Stack]
getStacks _ [] = []
getStacks crates (x:xs) = getDataForStack crates x "" : getStacks crates xs
  where
    getDataForStack :: [String] -> (Index, Location) -> String -> Stack
    getDataForStack [] (i, l) current = (i, dropWhile isSpace current)
    getDataForStack (crate:crates) (i, l) current
      = getDataForStack crates (i, l) (current ++ [nextCrate])
      where
        nextCrate = crate!!l

parseInstructions :: [Stack] -> [String] -> [Stack]
parseInstructions stacks [] = stacks
parseInstructions stacks (s:ss)
  = parseInstructions (move stacks number) ss
  where
    instructions = words s
    number = read (instructions!!1) :: Int
    from = read (instructions!!3) :: Int
    to = read (instructions!!5) :: Int
    move :: [Stack] -> Int -> [Stack]
    move initialStack 0 = initialStack
    move initialStack n
      = move newStack (n - 1)
      where
        (_, movingCrate:newFrom) = head $ filter (\(a, b) -> a == from) initialStack
        (_, movingTo) = head $ filter (\(a, b) -> a == to) initialStack
        newTo = movingCrate : movingTo
        newStack = (filter (\(a, b) -> a /= from && a /= to) initialStack) ++ [(to, newTo), (from, newFrom)]
        
results xs = sortOn fst xs

main1 :: String -> IO ()
main1 source = do
  i <- readFile source
  let splitFile = splitOn "\n\n" i :: [String]
  let currentData = splitOn "\n" $ splitFile!!0
  let inst = splitOn "\n" $ splitFile!!1
  let listingRow = last currentData
  let indices = map read $ words listingRow :: [Int]
  let indexLocations = [(x, y) | x <- indices, y <- [0..((length listingRow) - 1)], listingRow!!y == chr (x + 48)]
  let stackData = Data.List.init currentData
  let startingStacks = getStacks stackData indexLocations
  putStrLn(show $ results (parseInstructions startingStacks inst))


-- Part 2

parseInstructions' :: [Stack] -> [String] -> [Stack]
parseInstructions' stacks [] = stacks
parseInstructions' stacks (s:ss)
  = parseInstructions' (newStack) ss
  where
    instructions = words s
    number = read (instructions!!1) :: Int
    from = read (instructions!!3) :: Int
    to = read (instructions!!5) :: Int
    (_, fromStack) = head $ filter (\(a, b) -> a == from) stacks
    (_, toStack) = head $ filter (\(a, b) -> a == to) stacks
    newFrom = drop number fromStack
    newTo = (take number fromStack) ++ toStack
    newStack = (filter (\(a, b) -> a /= from && a /= to) stacks) ++ [(to, newTo), (from, newFrom)]

main2 :: String -> IO ()
main2 source = do
  i <- readFile source
  let splitFile = splitOn "\n\n" i :: [String]
  let currentData = splitOn "\n" $ splitFile!!0
  let inst = splitOn "\n" $ splitFile!!1
  let listingRow = last currentData
  let indices = map read $ words listingRow :: [Int]
  let indexLocations = [(x, y) | x <- indices, y <- [0..((length listingRow) - 1)], listingRow!!y == chr (x + 48)]
  let stackData = Data.List.init currentData
  let startingStacks = getStacks stackData indexLocations
  putStrLn(show $ results (parseInstructions' startingStacks inst))
