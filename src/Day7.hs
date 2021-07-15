module Day7 where

import Inputs

import Data.Graph
import Data.List
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Data.Maybe

type Parser = Parsec Void String 

ruleParser :: Parser ([(String, Int)], String, [String])
ruleParser = do
  this1 <- some lowerChar
  char ' '
  this2 <- some lowerChar
  string " bags contain"
  conns <- try (string " no other bags." >> pure []) <|> some arrParser
  pure (conns, this1 <> " " <> this2, map fst conns)
  where 
    arrParser = do
      char ' '
      n <- decimal
      char ' '
      to1 <- some lowerChar
      char ' '
      to2 <- some lowerChar
      string " bag"
      optional (char 's')
      char '.' <|> char ','
      pure (to1 <> " " <> to2, n)

part1 = case traverse (parse ruleParser "") . lines $ day7 of
  Left err -> error "unreachable"
  Right ok -> 
    let (graph, toNode, index) = graphFromEdges ok
        Just v = index "shiny gold" 
    in subtract 1 . length $ reachable (transposeG graph) v

part2 = case traverse (parse ruleParser "") . lines $ day7 of
  Left err -> error "unreachable"
  Right ok -> 
    let (graph, toNode, index) = graphFromEdges ok
        Just v = index "shiny gold" 
        countBags v = 
          let (desc,_,_) = toNode v
          in sum [x + x*countBags (fromJust (index s)) | (s, x) <- desc ]
    in countBags v
