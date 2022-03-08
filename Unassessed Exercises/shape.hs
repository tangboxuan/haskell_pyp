data Shape = Triangle Float Float Float | Circle Float | Square Float | Polygon [(Float, Float)]
area :: Shape -> Float
area (Triangle a b c)
    = sqrt (s * (s - a) * (s - b) * (s - c))
    where
        s =  (a + b + c) / 2
area (Circle r)
    = pi * r ^ 2
area (Square l)
    = l ^ 2
area (Polygon (x : y : z : xs))
    = area (Triangle a b c) + area (Polygon (y : z : xs))
    where
        l (a, c) (b, d)= sqrt ((a - b) ^ 2 + (c - d) ^ 2)
        a = l x y
        b = l x z
        c = l y z
area (Polygon _)
    = 0

type Date = (Int, Int, Int)
age :: Date -> Date -> Int
age (d, m, y) (d', m', y')
    | (m', d') <= (m, d) = y' - y - 1
    | otherwise = y' - y

data Tree = Leaf | Node Tree Tree
    deriving (Eq, Show)
makeTrees :: Int -> [Tree]
makeTrees 0
    = [Leaf]
makeTrees n
    = [ Node l r | x <- [0..(n - 1)], l <- (makeTrees x), r <- (makeTrees (n - x - 1))]

data BTree a = BLeaf a | BNode (BTree a) (BTree a)
    deriving (Show)
build :: [a] -> BTree a
build [x]
    = BLeaf x
build xs
    = BNode (build as) (build bs)
    where
        n        = div (length xs) 2
        (as, bs) = splitAt n xs
ends :: BTree a -> [a]
ends (BNode a b)
    = ends a ++ ends b
ends (BLeaf x)
    = [x]
swap :: BTree a -> BTree a
swap (BNode a b)
    = BNode (swap b) (swap a)
swap leaf
    = leaf

data Tree' a b = Empty' | Leaf' b | Node' a (Tree' a b) (Tree' a b)
mapT :: (b -> c) -> (a -> d) -> Tree' a b -> Tree' d c
mapT f g (Leaf' x)
    = Leaf' (f x)
mapT f g (Node' x l r)
    = Node' (g x) (mapT f g l) (mapT f g r)
foldT :: (b -> c) -> (a -> c -> c -> c) -> c -> Tree' a b -> c
foldT f g b Empty'
    = b
foldT f g b (Leaf' x)
    = f x
foldT f g b (Node' x l r)
    = g x (foldT f g b l) (foldT f g b r)
count
    = foldT (const 1) (\x l r -> l + r) 0
sum'
    = foldT id (\x l r -> x + l + r) 0
flatten
    = foldT (\x -> [x]) (\x l r -> l ++ (x : r)) []
flatten'
    = foldT (\x -> [x]) (\x l r -> r ++ (x : l)) []
eval
    = foldT id (\x l r -> x l r) 0