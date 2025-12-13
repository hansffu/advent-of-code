module Solutions.Day4 (solution, test) where

import Text.Megaparsec.Char (char, eol)

import Control.Applicative ((<|>))
import Control.Monad.State (State, evalState, get, put)
import Data.Maybe (mapMaybe)
import Lib.Array2d (Array2d, indexes2d, replaceArr2D, safe2dLookup, toArray2d, unsafe2dLookup)
import Lib.Parser (Parser)
import Lib.Solution
import Text.Megaparsec (many)

solution :: Solution Input String String
solution = Solution 4 parser part1 part2

part1 :: Input -> IO String
part1 input = return $ show $ length $ filter isSafe' $ indexes2d arr
 where
  arr = toArray2d input
  isRoll = unsafe2dLookup arr
  isSafe' pos@(row, col) =
    let neighbours = [(y, x) | y <- [row - 1, row, row + 1], x <- [col - 1, col, col + 1], (y, x) /= (row, col)]
        adj = length $ filter id $ fst <$> mapMaybe (safe2dLookup arr) neighbours
     in isRoll pos && adj < 4

part2 :: Input -> IO String
part2 = return . show . evalState doPass . toArray2d

doPass :: State (Array2d Bool) Int
doPass = do
  arr <- get
  let safe = filter (isSafe arr) (indexes2d arr)
  let newArr = foldr (\(y, x) acc -> replaceArr2D y x False acc) arr safe
  put newArr
  if null safe
    then return 0
    else do
      rest <- doPass
      return $ length safe + rest

-- doPass
isSafe :: Array2d Bool -> (Int, Int) -> Bool
isSafe arr pos@(row, col) =
  let neighbours = [(y, x) | y <- [row - 1, row, row + 1], x <- [col - 1, col, col + 1], (y, x) /= (row, col)]
      adj = length $ filter id $ fst <$> mapMaybe (safe2dLookup arr) neighbours
   in unsafe2dLookup arr pos && adj < 4

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
