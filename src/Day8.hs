module Day8 (run) where
import qualified Data.Char as Char
import qualified Data.List as List
import Control.Monad (join)

-- Check a single row or col for visibility
visible :: [Int] -> [Bool]
visible xs = zipWith (||) (inner xs) (reverse . inner . reverse $ xs)
    where
        inner = snd . List.mapAccumL (\acc x -> (max x acc, x > acc)) (-1)

distance :: [Int] -> [Int]
distance xs = zipWith (*) (inner xs) (reverse . inner . reverse $ xs)
    where
        inner = snd . List.mapAccumL (\s a -> (a : s, f $ span (< a) s)) []
        f (prefix, rest) = if (not . null) rest then 1 + length prefix else length prefix

-- Take a function to apply to each row & column, a function to zip together rows & cols
-- and then the input grid
combine :: ([Int] -> [a]) -> (a -> a -> a) -> [[Int]] -> [[a]]
combine f op xs = zipWith (zipWith op) rows cols
    where
        rows = map f xs
        cols = (List.transpose . map f . List.transpose) xs

run :: String -> IO ()
run file = do
    grid <- map (map Char.digitToInt) . lines <$> readFile file
    let p1grid = combine visible (||) grid
    let part1 = sum $ map (length . filter id) p1grid

    let p2grid = combine distance (*) grid
    let part2 = foldr max 0 $ join p2grid

    print part1
    print part2