module Day1(run) where

import Text.Read
import Data.List

parse :: String -> IO [Maybe Int]
parse = fmap (map readMaybe . lines) . readFile

-- Partition a list, breaking into sub lists where an element is Nothing
part :: [Maybe Int] -> [[Int]]
part xs = inner [] [] xs where 
    inner res acc [] = acc : res
    inner res acc (Just x : xs) = inner res (x : acc) xs
    inner res acc (Nothing : xs) = inner (acc : res) [] xs

run :: String -> IO ()
run input = do
  input <- map sum . part <$> parse input
  print $ maximum input
  print $ sum $ take 3 (reverse . sort $ input)
