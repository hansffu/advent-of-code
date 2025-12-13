module Lib.Array2d (
  getNeighbours,
  toArray2d,
  Array2d,
  replaceArr2D,
  Node,
  safe2dLookup,
  unsafe2dLookup,
  findIndexes2d,
  indexes2d,
) where

import Data.Array (Array, listArray, (!), (//))
import Data.Array.Base (bounds, (!?))
import Data.Maybe (catMaybes)

type Array2d a = Array Int (Array Int a)
type Node a = (a, (Int, Int))

toArray2d :: [[a]] -> Array2d a
toArray2d rows = listArray (0, length rows - 1) $ listArray (0, length (head rows) - 1) <$> rows

replaceArr2D :: Int -> Int -> a -> Array2d a -> Array2d a
replaceArr2D row col newVal arr2d =
  arr2d // [(row, newRow)]
 where
  currentRow = arr2d ! row
  newRow = currentRow // [(col, newVal)]

getNeighbours :: (a -> a -> Bool) -> Array2d a -> Node a -> [Node a]
getNeighbours predicate arr (item, (y, x)) =
  filter (predicate item . fst) $
    catMaybes
      [ safe2dLookup arr (y + 1, x)
      , safe2dLookup arr (y - 1, x)
      , safe2dLookup arr (y, x + 1)
      , safe2dLookup arr (y, x - 1)
      ]

safe2dLookup :: Array2d a -> (Int, Int) -> Maybe (a, (Int, Int))
safe2dLookup arr (y, x) = do
  row <- arr !? y
  item <- row !? x
  return (item, (y, x))

unsafe2dLookup :: Array2d a -> (Int, Int) -> a
unsafe2dLookup arr (y, x) =
  let row = arr ! y
      item = row ! x
   in item

findIndexes2d :: (a -> Bool) -> [[a]] -> [(a, (Int, Int))]
findIndexes2d p rows = do
  (y, row) <- zip [0 ..] rows
  (x, a) <- zip [0 ..] row
  if p a then return (a, (y, x)) else []

indexes2d :: Array2d a -> [(Int, Int)]
indexes2d arr = do
  let (rowStart, rowEnd) = bounds arr
  rowIdx <- [rowStart .. rowEnd]
  let row = arr ! rowIdx
      (colStart, colEnd) = bounds row
  colIdx <- [colStart .. colEnd]
  return (rowIdx, colIdx)
