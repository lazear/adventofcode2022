module Day6 (run) where

-- import Text.Read (read)
import Data.List
import qualified Data.Set as Set
import Data.Foldable (asum)

windows :: Int -> [a] -> [[a]]
windows n xs 
    | n > length xs = []
    | otherwise = take n xs : windows n (tail xs)

numUnique :: String -> Int
numUnique = Set.size . Set.fromList

startMarker :: Int -> String -> Maybe Int
startMarker sz = asum . zipWith inner [0..] . windows sz
    where
        inner idx x
            | numUnique x == sz = Just $ idx + sz 
            | otherwise = Nothing

part1 :: String -> Maybe Int
part1 = startMarker 4

part2 :: String -> Maybe Int
part2 = startMarker 14

run :: String -> IO ()
run file = do
    input <- lines <$> readFile file

    print $ map part1 input
    print $ map part2 input