{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}

module Solution where

import Data.Function ((&))
import Data.List (find)
import Data.Maybe (fromMaybe)

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
problem3 = 600851475143 & primeFactors & maximum

primeFactors :: Int -> [Int]
primeFactors = go 0
  where
    go ix@((primes !!) -> p) k
        | p * p > k = [k]
        | k %% p    = p : go ix (k `div` p)
        | otherwise = go (ix + 1) k

primes :: [Int]
primes = go [2..]
  where
    go [] = []
    go (p : xs) = p : go [x | x <- xs, x /% p]

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
