{-# LANGUAGE ViewPatterns #-}

module Day9 where

import Inputs

import Data.Semigroup
import Data.Foldable 
import Data.List
import Data.Bool (bool)
import Data.Maybe (isJust)
import Data.Function 
import Control.Monad (join)

part1 = fold . map (fmap Last . foo . take 26) . tails 
  . reverse . map read . lines $ day9
  where
    foo :: [Int] -> Maybe Int
    foo l@(length -> 26) =
      let (x:xs) = l
      in bool (Just x) Nothing . (>0) . length . filter (==x) 
        $ [a+b | ps <- tails xs, a <- take 1 ps, b <- tail ps]
        -- tails has an empty one at the end, can't pmatch
    foo x = Nothing
  
part2 = asum . map (fmap movingSums . sequence)
  . groupBy ((&&) `on` isJust) 
  . map (bool Nothing . Just <*> (< num)) 
  . map read . lines $ day9 
  where 
    (Just (Last num)) = part1
    movingSums = filter (/= 0) . concat 
      . (map.map) foo . map inits . tails
    foo xs = if sum xs == num then minimum xs + maximum xs
      else 0 