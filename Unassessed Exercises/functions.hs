addDigit :: Int -> Int -> Int
addDigit x y
  = 10 * x + y

ctof :: Double -> Double
ctof c
  = c * 9 / 5 + 32

type Vertex = (Float, Float)
distance :: Vertex -> Vertex -> Float
distance (ax, ay) (bx, by)
  = sqrt ((ax - bx) ^ 2 + (ay - by) ^ 2)

triangleArea :: Vertex -> Vertex -> Vertex -> Float
triangleArea x y z
  = sqrt (s * (s-a) * (s-b) * (s-c))
  where
    a = distance x y
    b = distance x z
    c = distance y z
    s = (a + b + c) / 2

isPrime :: Int -> Bool
-- Pre: x > 1
isPrime x
  = isPrime' (x - 1)
  where
    isPrime' :: Int -> Bool
    isPrime' a
      | a == 1    = True
      | otherwise = mod x a /= 0 && isPrime' (a - 1)

fact :: Int -> Int
-- Pre: x >= 0
fact x 
  | x == 0    = 1
  | otherwise = x * fact (x-1)

perm :: Int -> Int -> Int
-- Pre: n >= 0, r >= 0, n >= r
perm n r
  = perm' n
  where
    perm' :: Int -> Int
    perm' a
      | a == n - r = 1
      | otherwise  = a * perm' (a - 1)

choose :: Int -> Int -> Int
-- Pre: n >= 0, r >= 0, n >= r
choose n r
  | r == 0 = 1
  | r == n = 1
  | otherwise = choose (n - 1) r + choose (n - 1) (r - 1)

remainder :: Int -> Int -> Int
-- Pre: q >= 0, d > 0
remainder q d
  | q >= d    = remainder (q - d) d
  | otherwise = q

quotient :: Int -> Int -> Int
-- Pre:: q >= 0, d > 0
quotient q d
  | q >= d    = 1 + quotient (q - d) d
  | otherwise = 0

binary :: Int -> Int
-- Pre: x >= 0
binary x
  | x < 2     = x
  | otherwise = (binary (quotient x 2)) * 10 + remainder x 2

add :: Int -> Int -> Int
-- Pre: x, y >= 0
add x y
  | x == 0 = y
  | otherwise = add (pred x) (succ y)

larger :: Int -> Int -> String
-- Pre: x, y >= 0
larger x y
  | y == 0 = "First"
  | x == 0 = "Second"
  | otherwise = larger (pred x) (pred y)

chop :: Int -> (Int, Int)
chop n
  = chop' (0, n)
  where
    chop' :: (Int, Int) -> (Int, Int)
    chop' (a, b)
      | b < 10    = (a, b)
      | otherwise = chop' (a + 1, b - 10)

fib :: Int -> Int
-- Pre: n >= 0
fib n
  | n == 0    = 0
  | n == 1    = 1
  | otherwise = fib (n - 1) + fib (n - 2)

fib' :: Int -> Int
-- Pre: n >= 0
fib' n
  = fib'' 0 0 1
  where
    fib'' :: Int -> Int -> Int -> Int
    fib'' i a b
      | i == n    = a
      | otherwise = fib'' (i + 1) (b) (a + b)

goldenRatio :: Float -> Float
goldenRatio e
  = goldenRatio' 3 1.0 2.0
  where
    goldenRatio' :: Int -> Float -> Float -> Float
    goldenRatio' n a b
      | abs (a - b) < e = b
      | otherwise       = goldenRatio' (n + 1) b (fromIntegral (fib' (n + 1)) / fromIntegral (fib' n))