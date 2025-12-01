module Main where

import Lib.Solution (Solution (part1Solution, part2Solution))
import Solutions.Day1 (solution)
import Test.Tasty
import Test.Tasty.HUnit
import Utils (testSolution, testSolution')

main :: IO ()
main = defaultMain day4Tests

day1Tests :: TestTree
day1Tests =
  testGroup
    "Day 1"
    [ testCase "part 1" $
        do
          result <- testSolution' (part1Solution solution) "1.1"
          assertEqual "" 142 result
    , testCase "part 2" $
        do
          result <- testSolution (part2Solution solution) 1
          assertEqual "" 281 result
    ]
