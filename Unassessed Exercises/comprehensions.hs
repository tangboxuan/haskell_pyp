module Comprehensions where

import Data.List ((\\))

findAll n t = [y | (x, y) <- t, x == n]

remove :: Eq a => a -> [(a, b)] -> [(a, b)]
remove n xs
    = [(x, y) | (x, y) <- xs, x /= n]

remove' n xs
    = filter (\(a,b) -> a /= n) xs

quicksort :: [Int] -> [Int]
quicksort (h : xs)
    = quicksort [x | x <- xs, x < h] ++ h : quicksort [x | x <- xs, x > h] 
quicksort []
    = []

allSplits :: [a] -> [([a],[a])]
allSplits xs
    = [splitAt n xs | n <- [1..(length xs - 1)]]

prefixes :: [t] -> [[t]]
prefixes xs
    = [x | (x, y) <- allSplits xs] ++ (xs : [])

substrings :: String -> [String]
substrings xs
    = prefixes xs ++ concat [prefixes y | (x, y) <- allSplits xs]

perms :: Eq a => [a] -> [[a]]
perms []
    = [[]]
perms xs
    = [ x : y | x <- xs, y <- perms (xs \\ [x])]

routes :: Int -> Int -> [(Int, Int)] -> [[Int]]
routes x y paths
    = if x == y
      then [[y]]
      else map (\xs -> x : xs) (concat [routes b y [pair | pair@(c, d) <- paths, d /= b] | (a, b) <- paths, a == x])
