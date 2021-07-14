module Day1 where

import Inputs
import Control.Recursion

part1 = part1' 2020 . map read . lines $ day1
part1' goal = para foo
  where
    foo Nil = []
    foo (Cons x (past, xs)) = case filter ((==goal).(+x)) past of
      (y:_) -> x*y : xs
      _     -> xs
      
part2 =
  let li = map read . lines $ day1
  in concat $ foldr (((:) .). map . (*) <*> flip part1' li . (2020-)) [] li
