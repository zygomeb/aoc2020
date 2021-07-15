{-# LANGUAGE GADTs, TupleSections, ViewPatterns #-}

module Day8 where

import Inputs

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Control.Arrow

data Instr = Acc Int | Jmp Int | Nop Int deriving Show 

data Tape a = Tape [a] a [a] deriving Show

instance Functor Tape where
  fmap f (Tape ls c rs) = Tape (fmap f ls) (f c) (fmap f rs) 

moveL 0 x = x
moveL n (Tape (l:ls) c rs) = moveL (n-1) $ Tape ls l (c:rs)

moveR 0 x = x
moveR n (Tape ls c (r:rs)) = moveR (n-1) $ Tape (c:ls) r rs

move x = if x < 0 then moveL (abs x) else moveR x

lastT (Tape ls x []) = True
lastT _ = False 

this (Tape _ x _) = x
fThis f (Tape ls x rs) = Tape ls (f x) rs 

rewind (Tape ls c rs) = Tape [] x xs
  where (x:xs) = reverse ls <> (c:rs)

newTapeWMemory (x:xs) = Tape [] (x,False) $ map (,False) xs 

type Parser = Parsec Void String

instrParser :: Parser Instr
instrParser = choice [
    string "acc " *> decSignP Acc,
    string "jmp " *> decSignP Jmp,
    string "nop " *> decSignP Nop
  ]
  where
    decSignP x = choice [
      char '-' *> (x . negate <$> decimal),
      char '+' *> (x <$> decimal)
      ]

part1 = fmap (loopNet 0 . newTapeWMemory) 
  . traverse (parse instrParser "") . lines $ day8
  where
    visit (Tape ls (c,_) rs) = Tape ls (c,True) rs 
    loopNet acc t@(this -> (_, True))  = acc
    loopNet acc t@(this -> (Acc x, _)) = loopNet (acc+x) . moveR 1 $ visit t
    loopNet acc t@(this -> (Jmp x, _)) = loopNet acc . move x $ visit t 
    loopNet acc t@(this -> (Nop _, _)) = loopNet acc . moveR 1 $ visit t

part2 = fmap (findBroken . rewind . loopNetT . newTapeWMemory) 
  . traverse (parse instrParser "") . lines $ day8
  where
    visit (Tape ls (c,_) rs) = Tape ls (c,True) rs 
    visited (Tape _ (_,b) _) = b
    loopNetT t@(this -> (_, True))  = t
    loopNetT t@(this -> (Acc x, _)) = loopNetT . moveR 1 $ visit t
    loopNetT t@(this -> (Jmp x, _)) = loopNetT . move x $ visit t 
    loopNetT t@(this -> (Nop _, _)) = loopNetT . moveR 1 $ visit t
    findBroken t@(this -> (Nop x, True)) = 
      if visited (move x t) then
        findBroken $ moveR 1 t
      else case interpret 0 . rewind 
        . fmap (fmap $ const False) . fThis (first (\(Nop x) -> Jmp x)) $ t of
          Just acc -> acc
          Nothing  -> findBroken $ moveR 1 t
    findBroken t@(this -> (Jmp x, True)) = 
      if visited (moveR 1 t) then
        findBroken $ moveR 1 t
      else case interpret 0 . rewind 
        . fmap (fmap $ const False) . fThis (first (\(Jmp x) -> Nop x)) $ t of
          Just acc -> acc
          Nothing  -> findBroken $ moveR 1 t
    findBroken t = findBroken $ moveR 1 t
    interpret acc t@(this -> (_, True))  = Nothing
    interpret acc t@(this -> (Acc x, _)) = if lastT t 
      then Just (acc+x)
      else interpret (acc+x) . moveR 1 $ visit t
    interpret acc t@(this -> (Jmp x, _)) = if lastT t && x == 1
      then Just acc
      else interpret acc . move x $ visit t 
    interpret acc t@(this -> (Nop _, _)) = if lastT t 
      then Just acc
      else interpret acc . moveR 1 $ visit t