{-# OPTIONS_GHC -Wno-x-partial #-}

module Solutions.Day7 (solution, test) where

import Text.Megaparsec.Char (eol, printChar)

import Data.List (elemIndex, nub, partition)
import Data.List.Extra (sort)
import Data.Maybe (fromJust)
import Lib.Parser (Parser)
import Lib.Solution
import Text.Megaparsec (many, sepBy)

solution :: Solution Input Int Int
solution = Solution 7 parser part1 part2

part1 :: Input -> IO Int
part1 input = return $ sum $ fst <$> beams
 where
  start = fromJust $ elemIndex 'S' $ head input
  splitters = (fst <$>) . filter (\(_, c) -> c == '^') . zip [0 ..] <$> tail input
  beams = (0, [start]) : zipWith splitBeams beams splitters

  splitBeams :: (Int, [Int]) -> [Int] -> (Int, [Int])
  splitBeams beams' splitters' =
    let (toSplit, kept) = partition (`elem` splitters') $ snd beams'
        split = toSplit >>= (\x -> [x - 1, x + 1])
        newBeams = nub $ sort $ split <> kept
     in (length toSplit, newBeams)

part2 :: Input -> IO Int
part2 = todo'

type Input = [String]
parser :: Parser Input
parser = many printChar `sepBy` eol

test :: IO (Int, Int)
test = testSolution solution
