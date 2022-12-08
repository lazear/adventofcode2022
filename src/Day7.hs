module Day7 (run) where

import Data.List
import Text.Read (readMaybe)
import Data.Maybe (catMaybes, mapMaybe)
import qualified Data.Char as Char
import qualified Data.Map as Map

data Line
    = Up
    | Down String
    | Entry String Int
    deriving (Show, Eq)

fileSize :: String -> Maybe Line
fileSize s =
    case words s
      of [l, r] -> readMaybe l >>= Just . Entry r
         _ -> Nothing

parse :: String -> Maybe Line
parse s
    | s == "$ cd .." = Just Up
    | "$ cd " `isPrefixOf` s = stripPrefix "$ cd " s >>= Just . Down
    | Char.isDigit (head s) = fileSize s
    | otherwise = Nothing

mkTree :: (Map.Map [String] Int, [String]) -> Line -> (Map.Map [String] Int, [String])
mkTree (map', pwd) Up = (map', tail pwd)
mkTree (map', pwd) (Down dir) = (map', dir : pwd)
mkTree (map', pwd) (Entry file sz) = (Map.insert (file : pwd) sz map', pwd)

mkTree' :: Map.Map [String] Int -> Map.Map String Int
mkTree' = Map.foldrWithKey (\k a out -> foldr (Map.alter (Just . maybe a (+ a))) out $ dirPath [] (tail k)) Map.empty

construct :: [Line] -> Map.Map String Int
construct = mkTree' . fst . foldl mkTree (Map.empty, [])

dirPath :: [String] -> [String] -> [String]
dirPath acc [] = acc
dirPath acc xs = dirPath (unwords xs : acc) $ tail xs

part1 :: Map.Map String Int -> Int
part1 = Map.foldr (+) 0 . Map.filter (<= 100000) 

part2 :: Map.Map String Int -> Int
part2 tree = Map.foldr min root $ Map.filter (>= goal) tree
    where
        root = Map.foldr max 0 tree
        goal = root - 40000000 

run :: String -> IO ()
run input = do
    input <- lines <$> readFile input
    let tree = construct $ mapMaybe parse input

    print $ part1 tree
    print $ part2 tree
