module Day3 where

import Inputs

import Data.List (foldl')

part1 = fst . foldl' foo (0,0) . map cycle . lines $ day3
  where 
    foo (n, p) line = (n + fromEnum ('#' == line !! p), p+3)

part2 = product . sequence slopes $ dat
  where
    dat = map cycle . lines $ day3
    slopes = [slopeCount 1 1, slopeCount 3 1, slopeCount 5 1, slopeCount 7 1, slopeCount 1 2]
    slopeCount k l = fst . foldl' (foo k) (0,k) . map snd .
      filter (toEnum . fst) . zip (0 : cycle (take (l-1) [0,0..] <> [1])) 
    foo k (n, p) line = (n + fromEnum ('#' == line !! p), p+k)