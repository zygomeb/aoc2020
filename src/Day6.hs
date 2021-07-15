{-# LANGUAGE LambdaCase, TypeApplications #-}

module Day6 where

import Inputs

import Data.Bits
import Data.Char
import Data.Monoid 

part1 = foldMap (Sum . count1) . blocks $ day6
  where
    count1 = popCount @Int . foldr ((.|.) . (2^) . subtract 97 . ord) 0 . filter isLower
    blocks = lines . concat . map (\case "" -> "\n"; x -> ' ':x) . lines

part2 = foldMap (Sum . count1) . blocks $ day6
  where
    count1 = popCount @Int . foldr (.&.) (2^32-1)
      . map (foldr ((.|.) . (2^) . subtract 97 . ord) 0) . words
    blocks = lines . concat . map (\case "" -> "\n"; x -> ' ':x) . lines