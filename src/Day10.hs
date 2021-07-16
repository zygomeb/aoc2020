{-# LANGUAGE TupleSections, TypeApplications, QuasiQuotes #-}

module Day10 where

import Inputs

import Data.List
import Control.Arrow

part1 = uncurry (*) . snd . foldl' foo (0,(0,1))
  . sort . map (read @Int) . lines $ day10
  where
    foo (p,c) x = case x-p of 
      1 -> (x, first succ c)
      3 -> (x, second succ c)
      _ -> (x, c)

part2 = foldl' foo [1,0,0] . map (take 4)
  . tails . (0:) . sort . map read . lines $ day10
  where
    step [y,z] = [y,z,0]
    step [z]   = [z,0,0]
    foo arrs (x:[]) = arrs
    foo (tothis:arrs) (x:xs) =
      zipWith (+) delta $ step arrs 
      where outs = map (fromEnum . (<= 3) . subtract x) xs
            delta = map (*tothis) outs
    foo arrs [] = arrs