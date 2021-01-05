module Util where

import Data.Char (isSpace)

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace
   
countMatches :: [a] -> (a -> Bool) -> Int
countMatches [] _ = 0
countMatches (x:xs) p =
    let c = countMatches xs p in
    if p x then 1 + c else c

splitAtFirst :: [a] -> (a -> Bool) -> ([a], [a])
splitAtFirst [] _ = ([], [])
splitAtFirst l@(x:xs) p =
   let (prefix, suffix) = splitAtFirst xs p in
   if p x
      then ([], l)
      else (x:prefix, suffix)

xor :: [Bool] -> Bool
xor [] = False
xor (True:xs) = notElem True xs
xor (False:xs) = xor xs
