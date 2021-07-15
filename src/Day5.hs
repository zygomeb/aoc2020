module Day5 where

import Inputs

import Control.Arrow
import Data.Tuple 
import Data.Bits (xor)

part1 = maximum . map decode . lines $ day5
  where
    decode = uncurry (+) . fmap (*8) . (bin 'R' *** bin 'B') . swap . splitAt 7
    bin one = snd . foldr (\a (b,s) -> if a == one then (b+1,s+2^b) else (b+1, s)) (0,0) 

part2 = foldr xor 0 $ [1..minimum ids-1] <> ids <> [maximum ids+1..1023] 
  where
    decode :: String -> Int
    decode = uncurry (+) . fmap (*8) . (bin 'R' *** bin 'B') . swap . splitAt 7
    bin one = snd . foldr (\a (b,s) -> if a == one then (b+1,s+2^b) else (b+1, s)) (0,0) 
    ids = map decode . lines $ day5