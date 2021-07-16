{-# LANGUAGE LambdaCase, TupleSections, DeriveFunctor #-}

module Day12 where

import Inputs

import Data.Function
import Control.Arrow
import Control.Monad
import Data.List

data Act = N | E | S | W | L | R | F 
  deriving (Show, Read, Enum)

type Action = (Act, Int)

manDistance (x,y) (z,w) = abs (x-z) + abs (y-w) 

parse :: String -> Action
parse (x:xs) = (read [x], read xs)

act :: (Action, (Int, Int)) -> Action -> (Action, (Int, Int))
act (dir@(direct, magn), xy) = \case
  (N, y) -> (dir,) $ second (+y) xy
  (E, x) -> (dir,) $ first  (+x) xy
  (S, y) -> (dir,) $ second (subtract y) xy
  (W, x) -> (dir,) $ first  (subtract x) xy
  (L, d) -> (,xy) . (,magn) .
    toEnum $ (fromEnum direct - ((d `rem` 360) `div` 90)) `mod` 4
  (R, d) -> (,xy) . (,magn) . 
    toEnum $ (fromEnum direct + ((d `rem` 360) `div` 90)) `rem` 4
  (F, u) -> act (dir,xy) (fmap (const u) dir) 

part1 = manDistance (0,0) . snd . foldl' act ((E,0),(0,0)) 
  . map parse . lines $ day12

instance (Num a, Num b) => Num (a, b) where
  (a,b) + (c,d) = (a+c, b+d)

act2 :: ((Int, Int), (Int, Int)) -> Action -> ((Int, Int), (Int, Int))
act2 (way, xy) = \case
  (N, y) -> (,xy) $ second (+y) way
  (E, x) -> (,xy) $ first  (+x) way
  (S, y) -> (,xy) $ second (subtract y) way
  (W, x) -> (,xy) $ first  (subtract x) way
  (L, d) -> (primRotate (negate d) way, xy)
  (R, d) -> (primRotate d way, xy)
  (F, u) -> (way, join (***) (u*) way + xy)

primRotate alpha (x, y) = case alpha `mod` 360 of
  0 -> (x,y)
  90 -> (y,-x)
  180 -> (-x,-y)
  270 -> (-y, x)

part2 = manDistance (0,0) . snd . foldl' act2 ((10,1),(0,0)) 
  . map parse . lines $ day12