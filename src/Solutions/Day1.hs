{-# LANGUAGE BlockArguments #-}

module Solutions.Day1 (solution) where

import Debug.Trace (traceShow)
import Lib.Parser (Parser, intP)
import Lib.Solution
import Lib.Utils (debug)
import Text.Megaparsec (anySingle, many)
import Text.Megaparsec.Char (eol)

solution :: Solution Input String String
solution = Solution 1 parser part1 part2

part1 :: Input -> IO String
part1 = pure . show . length . filter (== 0) . scanl rot 50
 where
  rot :: Int -> Rot -> Int
  rot from (R n) = (from + n) `mod` 100
  rot from (L n) = (from - n) `mod` 100

part2 :: Input -> IO String
part2 input = pure $ show $ length $ filter (== 0) $ scanl rot 50 steps
 where
  steps = input >>= toSteps
  toSteps :: Rot -> [Int]
  toSteps (R n) = replicate n 1
  toSteps (L n) = replicate n (negate 1)
  rot :: Int -> Int -> Int
  rot acc n = (acc + n) `mod` 100

data Rot = R Int | L Int deriving (Show)

type Input = [Rot]

parser :: Parser Input
parser = many rowP
 where
  rowP :: Parser Rot
  rowP = do
    dir <- anySingle
    count <- intP <* eol
    return $ if dir == 'L' then L count else R count

test :: IO (String, String)
test = testSolution solution
