module Main where

import qualified Day1 (run)
import qualified Day2 (run)

main :: IO ()
main = do
  putStrLn "Advent of Code 2022"
  -- Day1.run "data/day1.txt"
  Day2.run "data/day2.txt"
