module Day2(run) where

import Text.Read
import Data.List

parse :: String -> IO [[String]]
parse = fmap (map words . lines) . readFile

part1 :: [String] -> Int
part1 ["A", "X"] = 1 + 3
part1 ["A", "Y"] = 2 + 6
part1 ["A", "Z"] = 3 + 0
part1 ["B", "X"] = 1 + 0
part1 ["B", "Y"] = 2 + 3
part1 ["B", "Z"] = 3 + 6
part1 ["C", "X"] = 1 + 6
part1 ["C", "Y"] = 2 + 0
part1 ["C", "Z"] = 3 + 3
part1 _ = 0

part2 :: [String] -> Int
part2 ["A", "X"] = 3 + 0 
part2 ["A", "Y"] = 1 + 3 
part2 ["A", "Z"] = 2 + 6 
part2 ["B", "X"] = 1 + 0 
part2 ["B", "Y"] = 2 + 3 
part2 ["B", "Z"] = 3 + 6 
part2 ["C", "X"] = 2 + 0 
part2 ["C", "Y"] = 3 + 3
part2 ["C", "Z"] = 1 + 6
part2 _ = 0

run :: String -> IO ()
run input = do
    xs <- parse input
    print $ sum $ map part1 xs
    print $ sum $ map part2 xs