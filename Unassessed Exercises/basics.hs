module Basics where

import Prelude hiding (lcm)
import Data.Char (ord, chr, isSpace)
import Data.List ((\\))

precedes :: String -> String -> Bool
precedes (x : xs) (y : ys)
  = if x == y
    then precedes xs ys
    else x < y
precedes (x : xs) _
  = False
precedes _ _
  = True

pos :: Int -> [Int] -> Int
pos n (x : xs)
  = if n == x
    then 0
    else 1 + pos n xs

twoSame :: [Int] -> Bool
twoSame (x : xs)
  = elem x xs || twoSame xs
twoSame []
  = False

rev :: [a] -> [a]
rev (x : xs)
  = rev xs ++ [x]
rev []
  = []

rev' :: [a] -> [a]
rev' xs
  = rev'' xs []
  where
    rev'' :: [a] -> [a] -> [a]
    rev'' (x : xs) ys
      = rev'' xs (x : ys)
    rev'' [] ys
      = ys

subString :: String -> String -> Bool
subString xs ys
  = foldl1 (||) [subString' xs zs | zs <- [drop n ys | n <- [0..length ys]]]
  where
    subString' (x:xs) (y:ys)
      = x == y && subString' xs ys
    subString' [] _
      = True
    subString' _ _
      = False

transpose :: String -> String -> String -> String
transpose xs ys zs
  = [xs !! n |n <- [pos (ord z) (map ord ys)| z <- zs]]

removeWhitespace :: String -> String
removeWhitespace xs
  = filter (\x -> not (isSpace x)) xs

nextWord :: String -> (String, String)
-- Pre: First character not a whitespace
nextWord xs
  = nextWord' xs (removeWhitespace xs) []
  where
    nextWord' str@(x : xs) (y : ys) zs
      = if x == y
        then nextWord' xs ys (zs ++ (x : []))
        else (zs, str)
    nextWord' xs _ zs
      = (zs, xs)

splitUp :: String -> [String]
splitUp xs
  = splitUp' xs (removeWhitespace xs)
  where
    splitUp' xss@(x : xs) yss@(y : ys)
      = if x == y
        then [nw] ++ splitUp rem
        else splitUp' xs yss
      where 
          (nw, rem) = nextWord xss
    splitUp' _ []
      = []

primefactors :: Int -> [Int]
primefactors n
 = primefactors' n 2
 where
   primefactors' n i
    = if i > n
      then []
      else 
        if mod n i == 0
        then [i] ++ primefactors'(div n i) i
        else primefactors' n (i + 1)

hcf :: Int -> Int -> Int
hcf x y
  = foldl1 (*) (xs \\ (xs \\ ys))
  where
    xs = primefactors x
    ys = primefactors y

lcm :: Int -> Int -> Int
lcm x y
  = foldl1 (*) (xs ++ (ys \\ xs))
  where
    xs = primefactors x
    ys = primefactors y