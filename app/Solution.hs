{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE MultilineStrings #-}

module Solution where

import Data.Function       ((&))
import Data.List           (find)
import Data.Maybe          (fromMaybe)
import Data.Numbers.Primes (primes)

data Sol = Sol
    { number :: Int
    , title  :: String
    , desc   :: String
    , hash   :: String
    , answer :: String
    } deriving Show

sol :: Show a => Int -> String -> String -> String -> a -> Sol
sol n t d h = Sol n t d h . show

solutions :: [Sol]
solutions =
    [ answer1
    , answer2
    , answer3
    , answer4
    , answer5
    , answer6
    , answer7
    , answer8
    , answer9
    , answer10
    ]

----------------------------------------

answer1 :: Sol
answer1 = sol 1
    "Multiples of 3 and 5"
    "Find the sum of all the multiples of 3 or 5 below 1000."
    "e1edf9d1967ca96767dcc2b2d6df69f4"
    problem1

problem1 :: Int
problem1 = sum
    [ n
    | n <- [0 .. 1000 - 1]
    , n %% 3 || n %% 5
    ]

(%%) :: Integral a => a -> a -> Bool
a %% b = a `mod` b == 0

(/%) :: Integral a => a -> a -> Bool
a /% b = a `mod` b /= 0

----------------------------------------

answer2 :: Sol
answer2 = sol 2
    "Even Fibonacci numbers"
    "By considering the terms in the Fibonacci sequence whose values do not exceed four million, find the sum of the even-valued terms."
    "4194eb91842c8e7e6df099ca73c38f28"
    problem2

problem2 :: Int
problem2 = sum
    [ n
    | n <- takeWhile (< 4_000_000) fibs
    , even n
    ]

fibs :: [Int]
fibs = 1 : 2 : zipWith (+) fibs (drop 1 fibs)

----------------------------------------

answer3 :: Sol
answer3 = sol 3
    "Largest prime factor"
    "What is the largest prime factor of the number 600851475143?"
    "94c4dd41f9dddce696557d3717d98d82"
    problem3

problem3 :: Int
problem3 = 600851475143 &primeFactors &maximum

primeFactors :: Int -> [Int]
primeFactors = go 0
  where
    go ix@((primes !!) -> p) k
        | p * p > k = [k]
        | k %% p    = p : go ix (k `div` p)
        | otherwise = go (ix + 1) k

-- primes :: [Int]
-- primes = go [2..]
--   where
--     go [] = []
--     go (p : xs) = p : go [x | x <- xs, x /% p]

----------------------------------------

answer4 :: Sol
answer4 = sol 4
    "Largest palindrome product"
    "Find the largest palindrome made from the product of two 3-digit numbers"
    "d4cfc27d16ea72a96b83d9bdef6ce2ec"
    problem4

problem4 :: Int
problem4 = maximum
    [ x * y
    | x <- [100..999]
    , y <- [100..999]
    , x <= y
    , x * y &show &isPalindrome
    ]

isPalindrome :: Eq a => [a] -> Bool
isPalindrome s = s == reverse s

----------------------------------------

answer5 :: Sol
answer5 = sol 5
    "Smallest multiple"
    "What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20"
    "bc0d0a22a7a46212135ed0ba77d22f3a"
    problem5'

-- time: ~7s
problem5 :: Int
problem5 = find go [small .. big] &fromMaybe 0
  where
    go x  = all (x %%) [1..20]
    small = product (takeWhile (<= 20) primes)
    big   = product [1..20]

problem5' :: Int
problem5' = foldl kpk 1 [1..20]

kpk :: Int -> Int -> Int
kpk a b = a * b `div` fpb a b

fpb :: Int -> Int -> Int
fpb a b
    | a == b    = a
    | otherwise = fpb (a - b & abs) (min a b)

----------------------------------------

answer6 :: Sol
answer6 = sol 6
    "Sum square difference"
    "Find the difference between the sum of the squares of the first one hundred natural numbers ([1..100]) and the square of the sum."
    "867380888952c39a131fe1d832246ecc"
    problem6

problem6 :: Int
problem6 = sumSquare - squareSum &abs
  where
    sumSquare = [1..100] &map square &sum
    squareSum = [1..100] &sum &square
    square x = x * x

----------------------------------------

answer7 :: Sol
answer7 = sol 7
    "10001st prime"
    "What is the 10,001st prime number?"
    "8c32ab09ec0210af60d392e9b2009560"
    problem7

problem7 :: Int
problem7 = primes !! 10000

----------------------------------------

answer8 :: Sol
answer8 = sol 8
    "Largest product in a series"
    """Find the thirteen adjacent digits in the 1000-digit number that have the greatest product. What is the value of this product?

    73167176531330624919225119674426574742355349194934
    96983520312774506326239578318016984801869478851843
    85861560789112949495459501737958331952853208805511
    12540698747158523863050715693290963295227443043557
    66896648950445244523161731856403098711121722383113
    62229893423380308135336276614282806444486645238749
    30358907296290491560440772390713810515859307960866
    70172427121883998797908792274921901699720888093776
    65727333001053367881220235421809751254540594752243
    52584907711670556013604839586446706324415722155397
    53697817977846174064955149290862569321978468622482
    83972241375657056057490261407972968652414535100474
    82166370484403199890008895243450658541227588666881
    16427171479924442928230863465674813919123162824586
    17866458359124566529476545682848912883142607690042
    24219022671055626321111109370544217506941658960408
    07198403850962455444362981230987879927244284909188
    84580156166097919133875499200524063689912560717606
    05886116467109405077541002256983155200055935729725
    71636269561882670428252483600823257530420752963450
    """
    "0f53ea7949d32ef24f9186207600403c"
    problem8

problem8 :: Int
problem8 = theNumber &slidingBy 13 &map (product . map (read . (:[]))) &maximum

theNumber :: String
theNumber = "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450"

slidingBy :: Int -> [a] -> [[a]]
slidingBy n (length -> l) | l < n = []
slidingBy n xs = take n xs : slidingBy n (drop 1 xs)

----------------------------------------

answer9 :: Sol
answer9 = sol 9
    "Special Pythagorean triplet"
    "There exists exactly one Pythagorean triplet for which a + b + c = 1000. Find the product abc."
    "24eaa9820350012ff678de47cb85b639"
    problem9

problem9 :: Int
problem9 =
  let rng = [1..1000] in
    [ a * b * c
    | a <- rng
    , b <- rng
    , a <= b
    , c <- [1000 -a -b]
    , a*a + b*b == c*c
    ] & \case [x] -> x ; _ -> 0

----------------------------------------

answer10 :: Sol
answer10 = sol 10
    "Summation of primes"
    "Find the sum of all the primes below two million."
    "d915b2a9ac8749a6b837404815f1ae25"
    problem10

problem10 :: Int
problem10 = primes &takeWhile (< 2_000_000) &sum

theGrid :: String
theGrid =
    """
    08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08
    49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00
    81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65
    52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91
    22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80
    24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50
    32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70
    67 26 20 68 02 62 12 20 95 63 94 39 63 08 40 91 66 49 94 21
    24 55 58 05 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72
    21 36 23 09 75 00 76 44 20 45 35 14 00 61 33 97 34 31 33 95
    78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92
    16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57
    86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58
    19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40
    04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66
    88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69
    04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36
    20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16
    20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54
    01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48
    """
