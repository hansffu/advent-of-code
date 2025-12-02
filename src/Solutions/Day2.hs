module Solutions.Day2 (solution, test) where

import Text.Megaparsec.Char (string)

import Lib.Parser (Parser, intP)
import Lib.Solution
import Text.Megaparsec (sepBy)
import Text.Megaparsec.Char (char)

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
  toRange (a, b) = [a .. b]
  isRepeating n =
    let s = show n
        (f, l) = splitAt (length s `div` 2) s
     in f == l

part2 :: Input -> IO String
part2 = todo

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
