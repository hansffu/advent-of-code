module Solutions.Day2 (solution, test) where

import Text.Megaparsec.Char (char, string)

import Data.List (nub)
import Data.List.Split (chunksOf)
import Lib.Parser (Parser, intP)
import Lib.Solution
import Text.Megaparsec (sepBy)

solution :: Solution Input String String
solution = Solution 2 parser part1 part2

part1 :: Input -> IO String
part1 input =
  return $
    show $
      sum $
        filter isRepeating $
          input >>= toRange
 where
  isRepeating n =
    let s = show n
        (f, l) = splitAt (length s `div` 2) s
     in f == l

part2 :: Input -> IO String
part2 input = return $ show $ sum $ filter repeating $ input >>= toRange

toRange :: (Int, Int) -> [Int]
toRange (a, b) = [a .. b]

repeating :: Int -> Bool
repeating n = or invalidRanges
 where
  s = show n
  maxL = length s `div` 2
  divisors = [x | x <- [1 .. maxL], length s `mod` x == 0]
  chunks = (`chunksOf` s) <$> divisors
  invalidRanges = (\x -> length (nub x) == 1) <$> chunks

type Input = [(Int, Int)]
parser :: Parser Input
parser = range `sepBy` string ","
 where
  range = do
    a <- intP <* char '-'
    b <- intP
    return (a, b)

test :: IO (String, String)
test = testSolution solution
