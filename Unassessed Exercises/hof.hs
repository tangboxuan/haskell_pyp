import Data.Char

depunctuate :: String -> String
depunctuate xs
    = filter (\x -> not (elem x ".,:")) xs

makeString :: [Int] -> String
makeString xs
    = map (\x -> chr x) xs

enpower :: [Int] -> Int 
enpower [n]
    = n
enpower (n : ns)
    = enpower ns ^ n

enpower' :: [Int] -> Int
enpower' xs
    = foldr1 (\x y -> y ^ x) xs

revAll' :: [[a]] -> [a]
revAll' xs
    = concatMap (\x -> reverse x) xs

revAll :: [[a]] -> [a] 
revAll []
    = []
revAll (x : xs)
    = reverse x ++ revAll xs

rev :: [a] -> [a]
rev xs
    = foldl (\x y -> y : x) [] xs

dezip :: [(a,b)] -> ([a], [b])
dezip xs
    = foldr (\(x, y) (xs, ys) -> (x : xs, y : ys)) ([], []) xs

allSame :: [Int] -> Bool
allSame xs
    = and (zipWith (==) xs (tail xs))

factorials
    = scanl (*) 1 [2..]

e
    = sum (take 10 (map (1/) (scanl (*) 1 [1..])))

squash :: (a -> a -> b) -> [a] -> [b]
squash f []
    = []
squash f (x : y : xs)
    = (f x y) : (squash f (y : xs))
squash f (x : [])
    = []
squash' f xs
    = zipWith f xs (tail xs)

converge :: (a -> a -> Bool) -> [a] -> a
converge f (x : y : xs)
    | f x y     = x
    | otherwise = converge f (y : xs)
converge f [x]
    = x

e'
    = converge (\x y -> abs (x - y) < 0.00001) (scanl (+) 0 (scanl (/) 1 [1..]))

limit :: (a -> a -> Bool) -> [a] -> [a]
limit f (x : y : xs)
    | f x y     = [x, y]
    | otherwise = x : limit f (y : xs)
limit f x
    = x

repeatUntil :: (a -> Bool) -> (a -> a) -> a -> a
repeatUntil g f
    = head . filter g . iterate f

any', all' :: (a -> Bool) -> [a] -> Bool
any' p = or . map p
all' p = and . map p

isElem :: Eq a => a -> [a] -> Bool
isElem
    = any' . (==)

-- infixl 9 <.>
-- (<.>) :: (a -> b) -> (c -> d -> a) -> (c -> d -> b)
-- (<.>)
--     = (.) . (.)

-- pipeline :: [a -> a] -> [a] -> [a]
-- pipeline fs
--     = map . foldr id . fs