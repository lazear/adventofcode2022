module Day4(run) where

import Text.Read
import Data.List
import qualified Data.Char as Char


parse :: String -> IO [String]
parse = fmap lines . readFile

split :: (Char -> Bool) -> String -> [String]
split f s =
    case dropWhile f s 
      of "" -> []
         s' -> w : split f s''
            where (w, s'') = break f s'

toSections :: String -> [Int]
toSections = map read . split (not . Char.isDigit)

overlap :: (Bool -> Bool -> Bool) -> [Int] -> Bool
overlap op [a, b, c, d]
    | (a >= c && a <= d) `op` (b >= c && b <= d) = True
    | (c >= a && c <= b) `op` (d >= a && d <= b) = True
    | otherwise = False
overlap _ _ = False

part1 :: [String] -> Int
part1 = length . filter id . map (overlap (&&) . toSections)

part2 :: [String] -> Int
part2 = length . filter id . map (overlap (||) . toSections)

run :: String -> IO ()
run input = do
    lines <- parse input
    print $ part1 lines
    print $ part2 lines
