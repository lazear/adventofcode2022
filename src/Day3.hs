module Day3(run) where

import Text.Read
import Data.List
import qualified Data.Set as Set
import Data.Char (ord, isLower)


parse :: String -> IO [String]
parse = fmap lines . readFile

mapTup :: (a -> b) -> (a, a) -> (b, b)
mapTup f (a, a') = (f a, f a')

priority :: Char -> Int
priority c
    | isLower c = ord c - ord 'a' + 1
    | otherwise = ord c - ord 'A' + 27

half :: String -> ([Char], [Char])
half xs = splitAt (length xs `div` 2) xs

part1 :: String -> Int
part1 s =
    let (s1, s2) = mapTup Set.fromList (half s)
    in let intersect = Set.intersection s1 s2
    in priority . head $ Set.elems intersect

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n a = x : chunks n xs
    where
        (x, xs) = splitAt n a

-- Return set of characters found in all strings
inter :: [String] -> Set.Set Char
inter [] = Set.empty
inter (x:xs) = foldr (Set.intersection . Set.fromList) (Set.fromList x) xs

part2 :: [String] -> Int
part2 xs = sum $ map (priority . head . Set.elems . inter) (chunks 3 xs)

run :: String -> IO ()
run input = do
    input <- parse input
    print $ sum $ map part1 input
    print $ part2 input