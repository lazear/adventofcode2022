module Main where

import qualified Day1 (run)

main :: IO ()
main = do
  putStrLn "Advent of Code 2022"
  Day1.run "data/day1.txt"
