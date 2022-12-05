module Day5 (run) where

-- import Text.Read (read)
import Data.List
import Data.Maybe (catMaybes, listToMaybe, mapMaybe)
import qualified Data.Char as Char

-- Move 1 from 2 to 1
type Move = (Int, Int, Int)
type Stack = String

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n a = x : chunks n xs
    where
        (x, xs) = splitAt n a

split :: (Char -> Bool) -> String -> [String]
split f s =
    case dropWhile f s
      of "" -> []
         s' -> w : split f s''
            where (w, s'') = break f s'

toSections :: String -> [Int]
toSections = map read . split (not . Char.isDigit)

-- Parse the stacks:
-- From the header lines, split each line into chunks of 4 characters
-- For each chunk, filter down to only alphanumeric characters
-- Transpose the chunks so that they turn into logical stacks
-- Concat
initStacks :: [String] -> [Stack]
initStacks = map catMaybes . transpose . map (map (find Char.isAlpha) . chunks 4)

parseMove :: String -> Maybe Move
parseMove = doParse . toSections
    where
        doParse [quant, src, dst] = Just (quant, src - 1, dst - 1)
        doParse _ = Nothing

-- Process a single move, transforming the crates with `f`
runMove :: (Stack -> Stack) -> [Stack] -> Move -> [Stack]
runMove f stack (quant, src, dst)  = stack'
    where
        (prefix, src') = splitAt quant (stack !! src)
        dst' = f prefix ++ (stack !! dst)
        replace i v
          | i == src = src'
          | i == dst = dst'
          | otherwise = v
        stack' = zipWith replace [0..] stack

part1 :: [Stack] -> [Move] -> String
part1 s = mapMaybe listToMaybe . foldl (runMove reverse) s

part2 :: [Stack] -> [Move] -> String
part2 s = mapMaybe listToMaybe . foldl (runMove id) s

run :: String -> IO ()
run input = do
    input <- lines <$> readFile input
    let (init, rest) = break (isPrefixOf "move") input
    let moves = mapMaybe parseMove rest
    let stacks = initStacks init

    print $ part1 stacks moves
    print $ part2 stacks moves
