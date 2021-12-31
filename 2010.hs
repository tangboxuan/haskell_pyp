data SuffixTree = Leaf Int | Node [(String, SuffixTree)] 
                deriving (Eq, Show)

------------------------------------------------------

isPrefix :: String -> String -> Bool
isPrefix [] _
  = True
isPrefix (x:xs) (y:ys)
  = x==y && isPrefix xs ys
isPrefix _ _
  = False

--Pre: s is a prefix of s'
removePrefix :: String -> String -> String
removePrefix s s'
  = drop (length s) s'

suffixes :: [a] -> [[a]]
suffixes []
  = []
suffixes s
  = s : suffixes (tail s)

isSubstring :: String -> String -> Bool
isSubstring xs ys
  = elem xs (suffixes ys)

findSubstrings :: String -> String -> [Int]
findSubstrings xs ys
  = [length ys - length y | y <- suffixes ys, isPrefix xs y]

------------------------------------------------------

getIndices :: SuffixTree -> [Int]
getIndices (Leaf n)
  = [n]
getIndices (Node xs)
  = concat [getIndices x | (_, x) <- xs]

common (x:xs) (y:ys)
  | x == y    = x : common xs ys
  | otherwise = []
common _ _
  = []

partition :: Eq a => [a] -> [a] -> ([a], [a], [a])
partition xs ys
  = (cs, remove xs, remove ys)
  where
    cs = common xs ys
    remove = drop (length cs)

findSubstrings' :: String -> SuffixTree -> [Int]
findSubstrings' "" (Leaf i)
  = [i]
findSubstrings' _ (Leaf i)
  = []
findSubstrings' xs (Node ys)
  = concat [findSubstrings'' xs y | y <- ys]
  where
    findSubstrings'' xs (y, t)
      | a == "" = [] -- nothing in common
      | b == "" = getIndices t -- a /= ""
      | c == "" = findSubstrings' b t
      where
        (a, b, c) = partition xs y

------------------------------------------------------

insert :: (String, Int) -> SuffixTree -> SuffixTree
insert i@(s, n) (Node xs)
  | xs == xs' = Node ((s, Leaf n):xs)
  | otherwise = Node xs'
  where
    insert' :: (String, Int) -> (String, SuffixTree) -> (String, SuffixTree)
    xs' = [insert' i x | x <- xs]
    insert' (s, n) x@(a, t)
      | p == ""   = x
      | otherwise = (p, Node [(f, Leaf n), (g, t)])
      where
        (p, f, g) = partition s a

    

-- This function is given
buildTree :: String -> SuffixTree 
buildTree s
  = foldl (flip insert) (Node []) (zip (suffixes s) [0..])

------------------------------------------------------
-- Part IV

longestRepeatedSubstring :: SuffixTree -> String
longestRepeatedSubstring 
  = undefined

------------------------------------------------------
-- Example strings and suffix trees...

s1 :: String
s1 
  = "banana"

s2 :: String
s2 
  = "mississippi"

t1 :: SuffixTree
t1 
  = Node [("banana", Leaf 0), 
          ("a", Node [("na", Node [("na", Leaf 1), 
                                   ("", Leaf 3)]), 
                     ("", Leaf 5)]), 
          ("na", Node [("na", Leaf 2), 
                       ("", Leaf 4)])]

t2 :: SuffixTree
t2 
  = Node [("mississippi", Leaf 0), 
          ("i", Node [("ssi", Node [("ssippi", Leaf 1), 
                                    ("ppi", Leaf 4)]), 
                      ("ppi", Leaf 7), 
                      ("", Leaf 10)]), 
          ("s", Node [("si", Node [("ssippi", Leaf 2), 
                                   ("ppi", Leaf 5)]), 
                      ("i", Node [("ssippi", Leaf 3), 
                                  ("ppi", Leaf 6)])]), 
          ("p", Node [("pi", Leaf 8), 
                      ("i", Leaf 9)])]