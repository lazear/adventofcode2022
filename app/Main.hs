module Main where

import qualified Day1 (run)
import qualified Day2 (run)
import qualified Day3 (run)
import qualified Day4 (run)

main :: IO ()
main = do
  putStrLn "Advent of Code 2022"
  -- Day1.run "data/day1.txt"
  -- Day2.run "data/day2.txt"
  -- Day3.run "data/day3.txt"
  Day4.run "data/day4.txt"
