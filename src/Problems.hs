module Problems where

import Data.List (sortOn)
import Data.Ord (Down (Down))
import Flow

type Answer = (Integer, String)

antara :: Integer -> Integer -> [Integer]
antara awal akhir = [awal .. akhir]

antara' :: Integer -> Integer -> [Integer]
antara' akhir awal = [akhir, akhir - 1 .. awal]

p1 :: String
p1 =
  antara 1 999
    |> filter isMulOf3And5
    |> sum
    |> show

a1 :: Answer
a1 = (1, p1)

(%%) :: (Integral a) => a -> a -> Bool
a %% b = 0 == a `rem` b

isMulOf3And5 :: (Integral a) => a -> Bool
isMulOf3And5 n = n %% 3 || n %% 5

fib :: (Num a) => a -> a -> [a]
fib a b = a : fib b (a + b)

p2 :: String
p2 =
  fibs
    |> filter even
    |> sum
    |> show
 where
  fibs :: [Integer]
  fibs = takeWhile (< 4_000_000) $ fib 1 2
a2 :: Answer
a2 = (2, p2)

pFactors :: Integer -> [Integer]
pFactors num = aux num 2
 where
  aux 1 _ = []
  aux n f
    | f * f > n = [n]
    | n %% f = f : aux (n // f) f
    | otherwise = aux n (f + 1)

(//) :: Integer -> Integer -> Integer
(//) = div

p3 :: String
p3 =
  -- 13195
  600851475143
    |> pFactors
    |> foldl max 0
    |> show

a3 :: Answer
a3 = (3, p3)

isPalindrome :: String -> Bool
isPalindrome str =
  reverse str
    |> zip str
    |> map (uncurry (==))
    |> and

zipSelf :: [a] -> [(a, a)]
zipSelf xs = zip xs xs

cek :: [(Integer, Bool)] -> Integer
cek [] = 0
cek ((i, b) : xs) = if b then i else cek xs

p4 :: String
p4 =
  let angka = antara' 999 1
      angkas =
        [ a * b
        | a <- angka
        , b <- angka
        , a <= b
        ]
   in angkas
        |> sortOn Down
        |> map show
        |> filter isPalindrome
        |> head

-- \|> map isPalindrome
-- \|> zip angkas
-- \|> cek
-- \|> show

a4 :: Answer
a4 = (4, p4)

kpk :: Integer -> Integer -> Integer
kpk a b = a * b // fpb a b

fpb :: Integer -> Integer -> Integer
fpb a b
  | a == b = a
  | otherwise = fpb (max a b - min a b) (min a b)

p5 :: String
p5 = foldl kpk 1 [1 .. 20] |> show

a5 :: Answer
a5 = (5, p5)

sumSquareDiff :: [Integer] -> Integer
sumSquareDiff xs =
  let xsSqSm = xs |> map square |> sum
      xsSmSq = xs |> sum |> square
   in xsSqSm - xsSmSq |> abs

sumSquareDiff' :: [Integer] -> Integer
sumSquareDiff' =
  id
    .> selfPair
    .> mapFst (map square .> sum)
    .> mapSnd (sum .> square)
    .> uncurry (-)
    .> abs

selfPair :: a -> (a, a)
selfPair a = (a, a)

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst fn (f, s) = (fn f, s)

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd fn (f, s) = (f, fn s)

mapTuple :: (a -> a') -> (b -> b') -> (a, b) -> (a', b')
mapTuple fFst fSnd = mapFst fFst . mapSnd fSnd

square :: (Num a) => a -> a
square x = x * x

p6 :: String
p6 = antara 1 100 |> sumSquareDiff |> show

a6 :: Answer
a6 = (6, p6)

primes :: [Integer]
primes = sieve [2 ..]
 where
  sieve [] = []
  sieve (p : xs) = p : sieve [x | x <- xs, x `mod` p /= 0]

p7 :: String
p7 = primes !! 10_000 |> show
a7 :: Answer
a7 = (7, p7)

num8 :: Integer
num8 =
  7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450

p8 :: String
p8 =
  num8
    |> show
    |> map charToNum
    |> slidingBy 13
    |> filter (not <. elem 0)
    |> map product
    |> sortOn Down
    |> head
    |> show

charToNum :: Char -> Integer
charToNum n = case n of
  '0' -> 0
  '1' -> 1
  '2' -> 2
  '3' -> 3
  '4' -> 4
  '5' -> 5
  '6' -> 6
  '7' -> 7
  '8' -> 8
  '9' -> 9
  _ -> 0

slidingBy :: Int -> [a] -> [[a]]
slidingBy n xs
  | length xs < n = []
  | otherwise = take n xs : slidingBy n (tail xs)

a8 :: Answer
a8 = (8, p8)
