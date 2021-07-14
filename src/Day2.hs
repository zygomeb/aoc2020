module Day2 where

import Inputs

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

type Parser = Parsec Void String

parser :: Parser (Int,Int,Char,String)
parser = do
  x <- decimal 
  char '-' 
  y <- decimal 
  char ' '
  a <- lowerChar 
  string ": "
  pass <- some lowerChar 
  pure (x,y,a,pass)

part1 = foo . traverse (parse parser "") . lines $ day2
  where
    foo (Left _)  = undefined
    foo (Right x) = length . filter valid $ x  
    valid (a,b,x,str) = ((&&) . (>= a) <*> (<= b)) . length . filter (==x) $ str

part2 =  foo . traverse (parse parser "") . lines $ day2
  where
    foo (Left _)  = undefined
    foo (Right x) = length . filter valid $ x  
    valid (a,b,x,str) = ((str !! (a-1)) == x) `xor` ((str !! (b-1)) == x) 
    xor a b = (a || b) && not (a && b)  