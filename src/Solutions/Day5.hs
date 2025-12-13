module Solutions.Day5 (solution, test) where

import Text.Megaparsec.Char (eol, newline, string)

import Lib.Parser (Parser, intP)
import Lib.Solution
import Text.Megaparsec (many)

solution :: Solution Input Int String
solution = Solution 5 parser part1 part2

part1 :: Input -> IO Int
part1 (freshRanges, b) = return $ length $ filter isFresh b
 where
  isFresh vid = any (\(from, to) -> vid >= from && vid <= to) freshRanges

part2 :: Input -> IO String
part2 = todo

type Input = ([(Int, Int)], [Int])
parser :: Parser Input
parser = do
  ranges <- many rangeP <* newline
  ids <- many (intP <* eol)
  return (ranges, ids)
 where
  rangeP = do
    from <- intP <* string "-"
    to <- intP <* eol
    return (from, to)

test :: IO (Int, String)
test = testSolution solution
