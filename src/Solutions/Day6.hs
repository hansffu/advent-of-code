{-# LANGUAGE LambdaCase #-}

module Solutions.Day6 (solution, test) where

import Text.Megaparsec.Char (asciiChar)

import Data.Char (isSpace)
import Data.List (transpose)
import Data.List.Split (splitWhen)
import Lib.Parser (Parser)
import Lib.Solution
import Lib.Utils (readInt)
import Text.Megaparsec (many)

solution :: Solution Input Int Int
solution = Solution 6 parser part1 part2

part1 :: Input -> IO Int
part1 input' = do
  return $ sum results
 where
  input = words <$> lines input'
  op :: [Int -> Int -> Int]
  op =
    ( \case
        "+" -> (+)
        "-" -> (-)
        "*" -> (*)
        -- "/" -> (/)
        _ -> error "wrong symbol"
    )
      <$> last input
  nums' = init input
  nums = (readInt <$>) <$> transpose nums'
  results = zipWith foldr1 op nums

part2 :: Input -> IO Int
part2 raw = do
  return $ sum results
 where
  input = transpose $ init $ lines raw
  op :: [Int -> Int -> Int]
  op =
    ( \case
        "+" -> (+)
        "-" -> (-)
        "*" -> (*)
        -- "/" -> (/)
        _ -> error "wrong symbol"
    )
      <$> last (words <$> lines raw)

  nums' = splitWhen (all isSpace) input
  nums = (readInt <$>) <$> nums'
  results = zipWith foldr1 op nums

type Input = String
parser :: Parser Input
parser =
  many asciiChar

test :: IO (Int, Int)
test = testSolution solution
