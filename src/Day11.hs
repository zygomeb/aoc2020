{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable
    , GADTs #-}

module Day11 where

import Inputs

import Data.Array
import Control.Comonad
import Data.Bool
import Data.Monoid

data Cell = Ground | Free | Occupied 
  deriving Eq

showCell Ground = '.'
showCell Free   = 'L'
showCell Occupied = '#'

parseCell '.' = Ground
parseCell 'L' = Free
parseCell '#' = Occupied

-- no dep. types so we can't prove that this \in bounds of e
data Grid e a where 
  Grid :: (e,e) -> Array e (Array e a) -> Grid e a
  deriving (Show, Functor, Foldable, Traversable)

instance (Ix e, Eq a) => Eq (Grid e a) where
  (Grid _ gr) == (Grid _ gr') = gr == gr'

(!.!) p (x,y) = p ! x ! y

instance (Ix e, Num e, Enum e) => Comonad (Grid e) where
  extract (Grid xy gr) = gr !.! xy
  duplicate (Grid xy gr) = 
    let limX = snd $ bounds gr 
        limY = snd $ bounds (gr ! 0)
    in Grid xy $ listArray (0, limX) [listArray (0, limY) 
          [(Grid (x,y) gr) | y <- [0..limY]] | x <- [0..limX]]

neighb :: (Ix e, Eq e, Num e, Ord e, Enum e) => Grid e a -> [a]
neighb (Grid (x,y) gr) = map (gr !.!) around
  where 
    inBounds (x,y) = x `between` bounds gr && y `between` bounds (gr ! 0)
    between c (a,b) = a <= c && c <= b
    around = filter inBounds [(a,b) | a <- [x-1..x+1], b <- [y-1..y+1]
      , (a,b) /= (x,y)]

getBounds :: [[a]] -> (Int, Int)
getBounds a@(xs:_) = (length a, length xs) 

update :: Grid Int Cell -> Cell
update gr = 
  let occupien = length . filter (==Occupied) . neighb $ gr
  in case extract gr of
    Occupied -> if occupien >= 4 then Free else Occupied
    Free     -> if occupien == 0 then Occupied else Free
    Ground   -> Ground

draw :: Grid Int Cell -> String
draw (Grid xy gr) = foldMap (<>"\n") 
  . (fmap.foldMap) (pure.showCell) $ gr

fixedPoint :: (Eq (w a), Comonad w) => (w a -> a) -> w a -> w a
fixedPoint f w = go w (w =>> f) 
  where
    go w w' = if w == w' then w else go w' (w' =>> f)

part1 = countOcc . fixedPoint update . fmap parseCell 
  . Grid (0,0) . arrayify . lines $ day11
  where
    countOcc :: Grid Int Cell -> Sum Int
    countOcc = foldMap (bool 0 1 . (==Occupied))
    arrayify xs = 
      let (x,y) = getBounds xs
      in listArray (0, x-1) $ map (listArray (0, y-1)) xs

neighb2 :: Grid Int Cell -> [Cell]
neighb2 (Grid (x,y) gr) = map (gr !.!) around
  where 
    between c (a,b) = a <= c && c <= b
    limX = snd $ bounds gr 
    limY = snd $ bounds (gr ! 0) 
    ray = take 1 . dropWhile ((==Ground) . (gr !.!)) 
    around = -- bounds checking by construction
      ray (zip [x+1..limX] [y+1..limY]) <>
      ray (zip [x+1..limX] [y-1, y-2..0]) <>
      ray (zip [x-1, x-2..0] [y+1..limY]) <>
      ray (zip [x-1, x-2..0] [y-1, y-2..0]) <>
      ray (zip [x+1..limX] (repeat y)) <>
      ray (zip [x-1, x-2..0] (repeat y)) <>
      ray (zip (repeat x) [y+1, y+2..limY]) <>
      ray (zip (repeat x) [y-1, y-2..0])

update2 :: Grid Int Cell -> Cell
update2 gr = 
  let occupien = length . filter (==Occupied) . neighb2 $ gr
  in case extract gr of
    Occupied -> if occupien >= 5 then Free else Occupied
    Free     -> if occupien == 0 then Occupied else Free
    Ground   -> Ground

part2 = countOcc . fixedPoint update2 . fmap parseCell 
  . Grid (0,0) . arrayify . lines $ day11
  where
    countOcc :: Grid Int Cell -> Sum Int
    countOcc = foldMap (bool 0 1 . (==Occupied))
    arrayify xs = 
      let (x,y) = getBounds xs
      in listArray (0, x-1) $ map (listArray (0, y-1)) xs