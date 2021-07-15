{-# LANGUAGE ViewPatterns, LambdaCase #-}

module Day4 where

import Inputs

import Data.List (sort, nub, delete)

-- I absolutely HATE this day.
part1 = length . filter ((== proper) . delete "cid" . map (take 3)) 
  . map (sort . words) . blocks $ day4
  where
    blocks = lines . concat . map (\case "" -> "\n"; x -> ' ':x) . lines
    proper = ["byr","ecl","eyr","hcl","hgt","iyr","pid"]

part2 = length . filter parseValidate . (map.map) (drop 4)
  . map (filter (\case 'c':'i':'d':_ -> False; _ -> True))
  . filter ((== proper) . delete "cid" . map (take 3)) 
  . map (sort . words) . blocks $ day4
  where
    parseValidate [(read -> byr),ecl,(read -> eyr),hcl,(reverse -> hgtR),(read -> iyr),pid] = 
      1920 <= byr && byr <= 2002
      && (ecl `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"])
      && 2020 <= eyr && eyr <= 2030
      && hcl !! 0 == '#' && all (`elem` ['0'..'9']<>['a'..'f']) (tail hcl) && length (tail hcl) == 6
      && (case hgtR of 
            'n':'i':(read.reverse -> x) -> 59 <= x && x <= 76
            'm':'c':(read.reverse -> x) -> 150 <= x && x <= 193
            x -> False
          )
      && 2010 <= iyr && iyr <= 2020
      && length pid == 9
    blocks = lines . concat . map (\case "" -> "\n"; x -> ' ':x) . lines
    proper = ["byr","ecl","eyr","hcl","hgt","iyr","pid"]