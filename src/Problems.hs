{- |
module for the problems code and the answer in the format of
("Problem number", "Problem answer").
-}
module Problems where

import Control.Monad (filterM, forM_)
import Control.Monad.ST (runST)
import Control.Monad.ST.Strict (ST)
import Data.Array.ST hiding (range)
import Data.List (sortOn)
import Data.Ord (Down (Down))
import Flow
import GHC.Base (when)

-- | typing for convenient, [fst] is problem number, and [snd] is the answer
type Answer = (Int, String)

-- | antara membuat [List Int] dari awal hingga akhir secara menaik
antara :: Int -> Int -> [Int]
antara awal akhir = [awal .. akhir]

-- | antara' membuat [List Int] dari akhir hingga awal secara menurun
antara' :: Int -> Int -> [Int]
antara' akhir awal = [akhir, akhir - 1 .. awal]

p1 :: String
p1 =
  antara 1 999
    |> filter isMulOf3And5
    |> sum
    |> show

a1 :: Answer
a1 = (1, p1)

-- | operator [%%] atau [is evenly divisible by]
(%%) :: (Integral a) => a -> a -> Bool
a %% b = 0 == a `rem` b

-- | isMulOf3And5 untuk mengecek apakah sebuah bilangan bisa dibagi rata oleh [3] dan/atau [5]
isMulOf3And5 :: (Integral a) => a -> Bool
isMulOf3And5 n = n %% 3 || n %% 5

-- | fib menghasilkan [fibbonanci series] secara infinit
fib :: (Num a) => a -> a -> [a]
fib a b = a : fib b (a + b)

p2 :: String
p2 =
  fibs
    |> filter even
    |> sum
    |> show
 where
  fibs :: [Int]
  fibs = takeWhile (< 4_000_000) $ fib 1 2
a2 :: Answer
a2 = (2, p2)

-- | pFactors mencari faktor prima dari sebuah [Int], hasilnya tidak berupa [Set]
pFactors :: Int -> [Int]
pFactors num = aux num 2
 where
  aux 1 _ = []
  aux n f
    | f * f > n = [n]
    | n %% f = f : aux (n // f) f
    | otherwise = aux n (f + 1)

-- | operator [//] untuk pembagian bilangan bulat
(//) :: Int -> Int -> Int
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

{- |
isPalindrome mengecek apakah sebuah string [palindrome], yaitu sama jika
dibaca dari depan dan belakang
-}
isPalindrome :: String -> Bool
isPalindrome str =
  reverse str
    |> zip str
    |> map (uncurry (==))
    |> and

-- | isPalindrome' adalah bentuk [point-free] dari [isPalindrome]
isPalindrome' :: String -> Bool
isPalindrome' = reverse .> zipSelf .> map (uncurry (==)) .> and

-- | zipSelf men[zip] list dengan dirinya sendiri
zipSelf :: [a] -> [(a, a)]
zipSelf xs = zip xs xs

cek :: [(Int, Bool)] -> Int
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

-- | kpk mencari [Kelipatan Persekutuan Terkecil] dari dua bilangan bulat
kpk :: Int -> Int -> Int
kpk a b = a * b // fpb a b

-- | fpb mencari [Faktor Persekutuan Terbesar] dari dua bilangan bulat
fpb :: Int -> Int -> Int
fpb a b
  | a == b = a
  | otherwise = fpb (max a b - min a b) (min a b)

p5 :: String
p5 = foldl kpk 1 [1 .. 20] |> show

a5 :: Answer
a5 = (5, p5)

sumSquareDiff :: [Int] -> Int
sumSquareDiff xs =
  let xsSqSm = xs |> map square |> sum
      xsSmSq = xs |> sum |> square
   in xsSqSm - xsSmSq |> abs

sumSquareDiff' :: [Int] -> Int
sumSquareDiff' =
  id
    .> selfPair
    .> mapTuple (sum . map square) (square . sum)
    .> uncurry (-)
    .> abs

sumSquareDiff'' :: [Int] -> Int
sumSquareDiff'' =
  abs
    . uncurry (-)
    . mapTuple (sum . map square) (square . sum)
    . selfPair

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

primes :: [Int]
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

charToNum :: Char -> Int
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

isTriplePitagoras :: (Int, Int, Int) -> Bool
isTriplePitagoras (a, b, c) = a * a + b * b == c * c

p9 :: String
p9 =
  let range = antara 1 1000
   in [ a * b * c
      | a <- range
      , b <- range
      , a <= b
      , c <- range
      , b <= c
      , a <= c
      , a + b + c == 1000
      , isTriplePitagoras (a, b, c)
      ]
        |> head
        |> show

a9 :: Answer
a9 = (9, p9)

-- menggunakan optimasi dari ChatGPT
primesHingga :: Int -> [Int]
primesHingga limit = runST $ do
  sieve <- newArray (2, limit) True :: ST s (STUArray s Int Bool)
  forM_ [2 .. truncate (sqrt (fromIntegral limit :: Double))] $ \p -> do
    isPrime <- readArray sieve p
    when isPrime $
      forM_ [p * p, p * p + p .. limit] $
        \i -> writeArray sieve i False
  filterM (readArray sieve) [2 .. limit]

p10 :: String
p10 = primesHingga 2_000_000 |> sum |> show

a10 :: Answer
a10 = (10, p10)
