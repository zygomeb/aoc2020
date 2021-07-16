{-# LANGUAGE TupleSections #-}

module Day13 where

import Inputs

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Control.Arrow
import Data.Function
import Data.Void
import Data.List
import Data.Maybe

type Parser = Parsec Void String

parseL2 :: Parser [Int]
parseL2 = some $ do
  x <- decimal
  optional (char ',')
  optional (some $ string "x,")
  pure x

part1 = uncurry (*) . minimumBy (compare `on` snd) . uncurry (map . ((,) <*>) . mod)
  . parseMods . break (=='\n') $ day13
  where 
    parseMods :: (String, String) -> (Int, [Int])
    parseMods = negate . read *** either undefined id . parse parseL2 "" . tail

parse2L2 :: Parser [Maybe Int]
parse2L2 = some $ do
  x <- choice [Just <$> decimal, pure Nothing <* char 'x']
  optional (char ',')
  pure x

part2 = chineese . convertNormalizeMods
  . either undefined id . parse parse2L2 "" . head . tail . lines $ day13
  where
    convertNormalizeMods = fmap ((,) . uncurry mod <*> snd)
      . (fmap.fmap) fromJust . filter (maybe False (const True) . snd) . zip [0, -1..]

-- expects normalized congruences as input (i.e. that xi `mod` ni == xi)
chineese :: [(Int,Int)] -> Int
chineese ((x1,n1):xs) = go (x1 `mod` n1) n1 xs
  where
    go res _ [] = res
    go res iter ((xi,ni):xs) 
      | res `mod` ni == xi = go res (iter*ni) xs
    go res iter xs = go (res+iter) iter xs 

-- (t, t+1, t+2) = (7k, 59l, 31m) <-> t mod 7 = 0, t mod 59 = -1, t mod 31 = -2