module Solutions.Day1 (solution) where

import Lib.Parser (Parser)
import Lib.Solution
import Text.Megaparsec.Char (string)

solution :: Solution Input String String
solution = Solution 1 parser part1 part2

part1 :: Input -> IO String
part1 = todo

part2 :: Input -> IO String
part2 = todo

-- data Rot = R Int | L Int deriving (Show)

type Input = String
parser :: Parser Input
parser = string "todo"

test :: IO (String, String)
test = testSolution solution
