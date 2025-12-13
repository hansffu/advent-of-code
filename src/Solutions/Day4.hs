module Solutions.Day4 (solution, test) where

import Text.Megaparsec.Char (char, eol, string)

import Control.Applicative ((<|>))
import Data.Maybe (catMaybes, mapMaybe)
import Lib.Array2d (indexes2d, safe2dLookup, toArray2d, unsafe2dLookup)
import Lib.Parser (Parser)
import Lib.Solution
import Lib.Utils (prettyPrint)
import Text.Megaparsec (many)

solution :: Solution Input String String
solution = Solution 4 parser part1 part2

part1 :: Input -> IO String
part1 input = return $ show $ length $ filter isSafe $ indexes2d arr
 where
  arr = toArray2d input
  isRoll = unsafe2dLookup arr
  isSafe pos@(row, col) =
    let neighbours = [(y, x) | y <- [row - 1, row, row + 1], x <- [col - 1, col, col + 1], (y, x) /= (row, col)]
        adj = length $ filter id $ fst <$> mapMaybe (safe2dLookup arr) neighbours
     in isRoll pos && adj < 4

part2 :: Input -> IO String
part2 = todo

type Input = [[Bool]]
parser :: Parser Input
parser = many lineP
 where
  lineP = many charP <* eol
  charP = rollP <|> emptyP
  rollP = True <$ char '@'
  emptyP = False <$ char '.'

test :: IO (String, String)
test = testSolution solution
