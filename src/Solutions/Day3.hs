module Solutions.Day3 (solution, test) where

import Text.Megaparsec.Char (digitChar, eol)

import Data.Char (digitToInt)
import Lib.Parser (Parser)
import Lib.Solution
import Text.Megaparsec (many)

solution :: Solution Input String String
solution = Solution 3 parser part1 part2

part1 :: Input -> IO String
part1 input = return $ show $ sum $ biggest <$> input
 where
  biggest :: [Int] -> Int
  biggest [] = 0
  biggest [x] = 0
  biggest (x : xs) = max (x * 10 + b' xs) (biggest xs)

  b' :: [Int] -> Int
  b' [] = 0
  b' (x' : xs') = max x' $ b' xs'

part2 :: Input -> IO String
part2 = todo

type Input = [[Int]]
parser :: Parser Input
parser = many lineP
 where
  lineP = do
    a <- many digitChar <* eol
    return $ digitToInt <$> a

test :: IO (String, String)
test = testSolution solution
