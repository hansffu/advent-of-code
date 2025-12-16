module Solutions.Day5 (solution, test) where

import Text.Megaparsec.Char (eol, newline, string)

import Data.List (sort)
import Data.Tuple (swap)
import Lib.Parser (Parser, intP)
import Lib.Solution
import Text.Megaparsec (many)

solution :: Solution Input Int Int
solution = Solution 5 parser part1 part2

part1 :: Input -> IO Int
part1 (freshRanges, b) = return $ length $ filter isFresh b
 where
  isFresh vid = any (\(from, to) -> vid >= from && vid <= to) freshRanges

part2 :: Input -> IO Int
part2 = return . sum . ((+) 1 . uncurry (-) . swap <$>) . combineRanges . fst

combineRanges :: [Range] -> [Range]
combineRanges = foldr place []

type Range = (Int, Int)
combine :: Range -> Range -> [Range]
combine a@(s1, e1) b@(s2, e2)
  | isOverlapping = [(min s1 s2, max e1 e2)]
  | otherwise = sort [a, b]
 where
  isOverlapping = not (e1 + 1 < s2 || e2 + 1 < s1)

place :: Range -> [Range] -> [Range]
place b [] = pure b
place b [a] = combine a b
place b (a : as) = case combine a b of
  [c] -> place c as
  [c, d] -> c : place d as
  _ -> error "unreachable"

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

test :: IO (Int, Int)
test = testSolution solution
