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

----------------------------------------

answer11 :: Sol
answer11 = sol 11
    ""
    ""
    ""
    problem11

problem11 :: Int
problem11 = 0

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

----------------------------------------

answer12 :: Sol
answer12 = sol 12
    ""
    ""
    ""
    problem12

problem12 :: Int
problem12 = 0

----------------------------------------

answer13 :: Sol
answer13 = sol 13
    ""
    ""
    ""
    problem13

problem13 :: Int
problem13 = 0

----------------------------------------

answer14 :: Sol
answer14 = sol 14
    ""
    ""
    ""
    problem14

problem14 :: Int
problem14 = 0

----------------------------------------

answer15 :: Sol
answer15 = sol 15
    ""
    ""
    ""
    problem15

problem15 :: Int
problem15 = 0

----------------------------------------

answer16 :: Sol
answer16 = sol 16
    ""
    ""
    ""
    problem16

problem16 :: Int
problem16 = 0

----------------------------------------

answer17 :: Sol
answer17 = sol 17
    ""
    ""
    ""
    problem17

problem17 :: Int
problem17 = 0

----------------------------------------

answer18 :: Sol
answer18 = sol 18
    ""
    ""
    ""
    problem18

problem18 :: Int
problem18 = 0

----------------------------------------

answer19 :: Sol
answer19 = sol 19
    ""
    ""
    ""
    problem19

problem19 :: Int
problem19 = 0

----------------------------------------

answer20 :: Sol
answer20 = sol 20
    ""
    ""
    ""
    problem20

problem20 :: Int
problem20 = 0

----------------------------------------

answer21 :: Sol
answer21 = sol 21
    ""
    ""
    ""
    problem21

problem21 :: Int
problem21 = 0

----------------------------------------

answer22 :: Sol
answer22 = sol 22
    ""
    ""
    ""
    problem22

problem22 :: Int
problem22 = 0

----------------------------------------

answer23 :: Sol
answer23 = sol 23
    ""
    ""
    ""
    problem23

problem23 :: Int
problem23 = 0

----------------------------------------

answer24 :: Sol
answer24 = sol 24
    ""
    ""
    ""
    problem24

problem24 :: Int
problem24 = 0

----------------------------------------

answer25 :: Sol
answer25 = sol 25
    ""
    ""
    ""
    problem25

problem25 :: Int
problem25 = 0

----------------------------------------

answer26 :: Sol
answer26 = sol 26
    ""
    ""
    ""
    problem26

problem26 :: Int
problem26 = 0

----------------------------------------

answer27 :: Sol
answer27 = sol 27
    ""
    ""
    ""
    problem27

problem27 :: Int
problem27 = 0

----------------------------------------

answer28 :: Sol
answer28 = sol 28
    ""
    ""
    ""
    problem28

problem28 :: Int
problem28 = 0

----------------------------------------

answer29 :: Sol
answer29 = sol 29
    ""
    ""
    ""
    problem29

problem29 :: Int
problem29 = 0

----------------------------------------

answer30 :: Sol
answer30 = sol 30
    ""
    ""
    ""
    problem30

problem30 :: Int
problem30 = 0

----------------------------------------

answer31 :: Sol
answer31 = sol 31
    ""
    ""
    ""
    problem31

problem31 :: Int
problem31 = 0

----------------------------------------

answer32 :: Sol
answer32 = sol 32
    ""
    ""
    ""
    problem32

problem32 :: Int
problem32 = 0

----------------------------------------

answer33 :: Sol
answer33 = sol 33
    ""
    ""
    ""
    problem33

problem33 :: Int
problem33 = 0

----------------------------------------

answer34 :: Sol
answer34 = sol 34
    ""
    ""
    ""
    problem34

problem34 :: Int
problem34 = 0

----------------------------------------

answer35 :: Sol
answer35 = sol 35
    ""
    ""
    ""
    problem35

problem35 :: Int
problem35 = 0

----------------------------------------

answer36 :: Sol
answer36 = sol 36
    ""
    ""
    ""
    problem36

problem36 :: Int
problem36 = 0

----------------------------------------

answer37 :: Sol
answer37 = sol 37
    ""
    ""
    ""
    problem37

problem37 :: Int
problem37 = 0

----------------------------------------

answer38 :: Sol
answer38 = sol 38
    ""
    ""
    ""
    problem38

problem38 :: Int
problem38 = 0

----------------------------------------

answer39 :: Sol
answer39 = sol 39
    ""
    ""
    ""
    problem39

problem39 :: Int
problem39 = 0

----------------------------------------

answer40 :: Sol
answer40 = sol 40
    ""
    ""
    ""
    problem40

problem40 :: Int
problem40 = 0

----------------------------------------

answer41 :: Sol
answer41 = sol 41
    ""
    ""
    ""
    problem41

problem41 :: Int
problem41 = 0

----------------------------------------

answer42 :: Sol
answer42 = sol 42
    ""
    ""
    ""
    problem42

problem42 :: Int
problem42 = 0

----------------------------------------

answer43 :: Sol
answer43 = sol 43
    ""
    ""
    ""
    problem43

problem43 :: Int
problem43 = 0

----------------------------------------

answer44 :: Sol
answer44 = sol 44
    ""
    ""
    ""
    problem44

problem44 :: Int
problem44 = 0

----------------------------------------

answer45 :: Sol
answer45 = sol 45
    ""
    ""
    ""
    problem45

problem45 :: Int
problem45 = 0

----------------------------------------

answer46 :: Sol
answer46 = sol 46
    ""
    ""
    ""
    problem46

problem46 :: Int
problem46 = 0

----------------------------------------

answer47 :: Sol
answer47 = sol 47
    ""
    ""
    ""
    problem47

problem47 :: Int
problem47 = 0

----------------------------------------

answer48 :: Sol
answer48 = sol 48
    ""
    ""
    ""
    problem48

problem48 :: Int
problem48 = 0

----------------------------------------

answer49 :: Sol
answer49 = sol 49
    ""
    ""
    ""
    problem49

problem49 :: Int
problem49 = 0

----------------------------------------

answer50 :: Sol
answer50 = sol 50
    ""
    ""
    ""
    problem50

problem50 :: Int
problem50 = 0

----------------------------------------

answer51 :: Sol
answer51 = sol 51
    ""
    ""
    ""
    problem51

problem51 :: Int
problem51 = 0

----------------------------------------

answer52 :: Sol
answer52 = sol 52
    ""
    ""
    ""
    problem52

problem52 :: Int
problem52 = 0

----------------------------------------

answer53 :: Sol
answer53 = sol 53
    ""
    ""
    ""
    problem53

problem53 :: Int
problem53 = 0

----------------------------------------

answer54 :: Sol
answer54 = sol 54
    ""
    ""
    ""
    problem54

problem54 :: Int
problem54 = 0

----------------------------------------

answer55 :: Sol
answer55 = sol 55
    ""
    ""
    ""
    problem55

problem55 :: Int
problem55 = 0

----------------------------------------

answer56 :: Sol
answer56 = sol 56
    ""
    ""
    ""
    problem56

problem56 :: Int
problem56 = 0

----------------------------------------

answer57 :: Sol
answer57 = sol 57
    ""
    ""
    ""
    problem57

problem57 :: Int
problem57 = 0

----------------------------------------

answer58 :: Sol
answer58 = sol 58
    ""
    ""
    ""
    problem58

problem58 :: Int
problem58 = 0

----------------------------------------

answer59 :: Sol
answer59 = sol 59
    ""
    ""
    ""
    problem59

problem59 :: Int
problem59 = 0

----------------------------------------

answer60 :: Sol
answer60 = sol 60
    ""
    ""
    ""
    problem60

problem60 :: Int
problem60 = 0

----------------------------------------

answer61 :: Sol
answer61 = sol 61
    ""
    ""
    ""
    problem61

problem61 :: Int
problem61 = 0

----------------------------------------

answer62 :: Sol
answer62 = sol 62
    ""
    ""
    ""
    problem62

problem62 :: Int
problem62 = 0

----------------------------------------

answer63 :: Sol
answer63 = sol 63
    ""
    ""
    ""
    problem63

problem63 :: Int
problem63 = 0

----------------------------------------

answer64 :: Sol
answer64 = sol 64
    ""
    ""
    ""
    problem64

problem64 :: Int
problem64 = 0

----------------------------------------

answer65 :: Sol
answer65 = sol 65
    ""
    ""
    ""
    problem65

problem65 :: Int
problem65 = 0

----------------------------------------

answer66 :: Sol
answer66 = sol 66
    ""
    ""
    ""
    problem66

problem66 :: Int
problem66 = 0

----------------------------------------

answer67 :: Sol
answer67 = sol 67
    ""
    ""
    ""
    problem67

problem67 :: Int
problem67 = 0

----------------------------------------

answer68 :: Sol
answer68 = sol 68
    ""
    ""
    ""
    problem68

problem68 :: Int
problem68 = 0

----------------------------------------

answer69 :: Sol
answer69 = sol 69
    ""
    ""
    ""
    problem69

problem69 :: Int
problem69 = 0

----------------------------------------

answer70 :: Sol
answer70 = sol 70
    ""
    ""
    ""
    problem70

problem70 :: Int
problem70 = 0

----------------------------------------

answer71 :: Sol
answer71 = sol 71
    ""
    ""
    ""
    problem71

problem71 :: Int
problem71 = 0

----------------------------------------

answer72 :: Sol
answer72 = sol 72
    ""
    ""
    ""
    problem72

problem72 :: Int
problem72 = 0

----------------------------------------

answer73 :: Sol
answer73 = sol 73
    ""
    ""
    ""
    problem73

problem73 :: Int
problem73 = 0

----------------------------------------

answer74 :: Sol
answer74 = sol 74
    ""
    ""
    ""
    problem74

problem74 :: Int
problem74 = 0

----------------------------------------

answer75 :: Sol
answer75 = sol 75
    ""
    ""
    ""
    problem75

problem75 :: Int
problem75 = 0

----------------------------------------

answer76 :: Sol
answer76 = sol 76
    ""
    ""
    ""
    problem76

problem76 :: Int
problem76 = 0

----------------------------------------

answer77 :: Sol
answer77 = sol 77
    ""
    ""
    ""
    problem77

problem77 :: Int
problem77 = 0

----------------------------------------

answer78 :: Sol
answer78 = sol 78
    ""
    ""
    ""
    problem78

problem78 :: Int
problem78 = 0

----------------------------------------

answer79 :: Sol
answer79 = sol 79
    ""
    ""
    ""
    problem79

problem79 :: Int
problem79 = 0

----------------------------------------

answer80 :: Sol
answer80 = sol 80
    ""
    ""
    ""
    problem80

problem80 :: Int
problem80 = 0

----------------------------------------

answer81 :: Sol
answer81 = sol 81
    ""
    ""
    ""
    problem81

problem81 :: Int
problem81 = 0

----------------------------------------

answer82 :: Sol
answer82 = sol 82
    ""
    ""
    ""
    problem82

problem82 :: Int
problem82 = 0

----------------------------------------

answer83 :: Sol
answer83 = sol 83
    ""
    ""
    ""
    problem83

problem83 :: Int
problem83 = 0

----------------------------------------

answer84 :: Sol
answer84 = sol 84
    ""
    ""
    ""
    problem84

problem84 :: Int
problem84 = 0

----------------------------------------

answer85 :: Sol
answer85 = sol 85
    ""
    ""
    ""
    problem85

problem85 :: Int
problem85 = 0

----------------------------------------

answer86 :: Sol
answer86 = sol 86
    ""
    ""
    ""
    problem86

problem86 :: Int
problem86 = 0

----------------------------------------

answer87 :: Sol
answer87 = sol 87
    ""
    ""
    ""
    problem87

problem87 :: Int
problem87 = 0

----------------------------------------

answer88 :: Sol
answer88 = sol 88
    ""
    ""
    ""
    problem88

problem88 :: Int
problem88 = 0

----------------------------------------

answer89 :: Sol
answer89 = sol 89
    ""
    ""
    ""
    problem89

problem89 :: Int
problem89 = 0

----------------------------------------

answer90 :: Sol
answer90 = sol 90
    ""
    ""
    ""
    problem90

problem90 :: Int
problem90 = 0

----------------------------------------

answer91 :: Sol
answer91 = sol 91
    ""
    ""
    ""
    problem91

problem91 :: Int
problem91 = 0

----------------------------------------

answer92 :: Sol
answer92 = sol 92
    ""
    ""
    ""
    problem92

problem92 :: Int
problem92 = 0

----------------------------------------

answer93 :: Sol
answer93 = sol 93
    ""
    ""
    ""
    problem93

problem93 :: Int
problem93 = 0

----------------------------------------

answer94 :: Sol
answer94 = sol 94
    ""
    ""
    ""
    problem94

problem94 :: Int
problem94 = 0

----------------------------------------

answer95 :: Sol
answer95 = sol 95
    ""
    ""
    ""
    problem95

problem95 :: Int
problem95 = 0

----------------------------------------

answer96 :: Sol
answer96 = sol 96
    ""
    ""
    ""
    problem96

problem96 :: Int
problem96 = 0

----------------------------------------

answer97 :: Sol
answer97 = sol 97
    ""
    ""
    ""
    problem97

problem97 :: Int
problem97 = 0

----------------------------------------

answer98 :: Sol
answer98 = sol 98
    ""
    ""
    ""
    problem98

problem98 :: Int
problem98 = 0

----------------------------------------

answer99 :: Sol
answer99 = sol 99
    ""
    ""
    ""
    problem99

problem99 :: Int
problem99 = 0

----------------------------------------

answer100 :: Sol
answer100 = sol 100
    ""
    ""
    ""
    problem100

problem100 :: Int
problem100 = 0

----------------------------------------

answer101 :: Sol
answer101 = sol 101
    ""
    ""
    ""
    problem101

problem101 :: Int
problem101 = 0

----------------------------------------

answer102 :: Sol
answer102 = sol 102
    ""
    ""
    ""
    problem102

problem102 :: Int
problem102 = 0

----------------------------------------

answer103 :: Sol
answer103 = sol 103
    ""
    ""
    ""
    problem103

problem103 :: Int
problem103 = 0

----------------------------------------

answer104 :: Sol
answer104 = sol 104
    ""
    ""
    ""
    problem104

problem104 :: Int
problem104 = 0

----------------------------------------

answer105 :: Sol
answer105 = sol 105
    ""
    ""
    ""
    problem105

problem105 :: Int
problem105 = 0

----------------------------------------

answer106 :: Sol
answer106 = sol 106
    ""
    ""
    ""
    problem106

problem106 :: Int
problem106 = 0

----------------------------------------

answer107 :: Sol
answer107 = sol 107
    ""
    ""
    ""
    problem107

problem107 :: Int
problem107 = 0

----------------------------------------

answer108 :: Sol
answer108 = sol 108
    ""
    ""
    ""
    problem108

problem108 :: Int
problem108 = 0

----------------------------------------

answer109 :: Sol
answer109 = sol 109
    ""
    ""
    ""
    problem109

problem109 :: Int
problem109 = 0

----------------------------------------

answer110 :: Sol
answer110 = sol 110
    ""
    ""
    ""
    problem110

problem110 :: Int
problem110 = 0

----------------------------------------

answer111 :: Sol
answer111 = sol 111
    ""
    ""
    ""
    problem111

problem111 :: Int
problem111 = 0

----------------------------------------

answer112 :: Sol
answer112 = sol 112
    ""
    ""
    ""
    problem112

problem112 :: Int
problem112 = 0

----------------------------------------

answer113 :: Sol
answer113 = sol 113
    ""
    ""
    ""
    problem113

problem113 :: Int
problem113 = 0

----------------------------------------

answer114 :: Sol
answer114 = sol 114
    ""
    ""
    ""
    problem114

problem114 :: Int
problem114 = 0

----------------------------------------

answer115 :: Sol
answer115 = sol 115
    ""
    ""
    ""
    problem115

problem115 :: Int
problem115 = 0

----------------------------------------

answer116 :: Sol
answer116 = sol 116
    ""
    ""
    ""
    problem116

problem116 :: Int
problem116 = 0

----------------------------------------

answer117 :: Sol
answer117 = sol 117
    ""
    ""
    ""
    problem117

problem117 :: Int
problem117 = 0

----------------------------------------

answer118 :: Sol
answer118 = sol 118
    ""
    ""
    ""
    problem118

problem118 :: Int
problem118 = 0

----------------------------------------

answer119 :: Sol
answer119 = sol 119
    ""
    ""
    ""
    problem119

problem119 :: Int
problem119 = 0

----------------------------------------

answer120 :: Sol
answer120 = sol 120
    ""
    ""
    ""
    problem120

problem120 :: Int
problem120 = 0

----------------------------------------

answer121 :: Sol
answer121 = sol 121
    ""
    ""
    ""
    problem121

problem121 :: Int
problem121 = 0

----------------------------------------

answer122 :: Sol
answer122 = sol 122
    ""
    ""
    ""
    problem122

problem122 :: Int
problem122 = 0

----------------------------------------

answer123 :: Sol
answer123 = sol 123
    ""
    ""
    ""
    problem123

problem123 :: Int
problem123 = 0

----------------------------------------

answer124 :: Sol
answer124 = sol 124
    ""
    ""
    ""
    problem124

problem124 :: Int
problem124 = 0

----------------------------------------

answer125 :: Sol
answer125 = sol 125
    ""
    ""
    ""
    problem125

problem125 :: Int
problem125 = 0

----------------------------------------

answer126 :: Sol
answer126 = sol 126
    ""
    ""
    ""
    problem126

problem126 :: Int
problem126 = 0

----------------------------------------

answer127 :: Sol
answer127 = sol 127
    ""
    ""
    ""
    problem127

problem127 :: Int
problem127 = 0

----------------------------------------

answer128 :: Sol
answer128 = sol 128
    ""
    ""
    ""
    problem128

problem128 :: Int
problem128 = 0

----------------------------------------

answer129 :: Sol
answer129 = sol 129
    ""
    ""
    ""
    problem129

problem129 :: Int
problem129 = 0

----------------------------------------

answer130 :: Sol
answer130 = sol 130
    ""
    ""
    ""
    problem130

problem130 :: Int
problem130 = 0

----------------------------------------

answer131 :: Sol
answer131 = sol 131
    ""
    ""
    ""
    problem131

problem131 :: Int
problem131 = 0

----------------------------------------

answer132 :: Sol
answer132 = sol 132
    ""
    ""
    ""
    problem132

problem132 :: Int
problem132 = 0

----------------------------------------

answer133 :: Sol
answer133 = sol 133
    ""
    ""
    ""
    problem133

problem133 :: Int
problem133 = 0

----------------------------------------

answer134 :: Sol
answer134 = sol 134
    ""
    ""
    ""
    problem134

problem134 :: Int
problem134 = 0

----------------------------------------

answer135 :: Sol
answer135 = sol 135
    ""
    ""
    ""
    problem135

problem135 :: Int
problem135 = 0

----------------------------------------

answer136 :: Sol
answer136 = sol 136
    ""
    ""
    ""
    problem136

problem136 :: Int
problem136 = 0

----------------------------------------

answer137 :: Sol
answer137 = sol 137
    ""
    ""
    ""
    problem137

problem137 :: Int
problem137 = 0

----------------------------------------

answer138 :: Sol
answer138 = sol 138
    ""
    ""
    ""
    problem138

problem138 :: Int
problem138 = 0

----------------------------------------

answer139 :: Sol
answer139 = sol 139
    ""
    ""
    ""
    problem139

problem139 :: Int
problem139 = 0

----------------------------------------

answer140 :: Sol
answer140 = sol 140
    ""
    ""
    ""
    problem140

problem140 :: Int
problem140 = 0

----------------------------------------

answer141 :: Sol
answer141 = sol 141
    ""
    ""
    ""
    problem141

problem141 :: Int
problem141 = 0

----------------------------------------

answer142 :: Sol
answer142 = sol 142
    ""
    ""
    ""
    problem142

problem142 :: Int
problem142 = 0

----------------------------------------

answer143 :: Sol
answer143 = sol 143
    ""
    ""
    ""
    problem143

problem143 :: Int
problem143 = 0

----------------------------------------

answer144 :: Sol
answer144 = sol 144
    ""
    ""
    ""
    problem144

problem144 :: Int
problem144 = 0

----------------------------------------

answer145 :: Sol
answer145 = sol 145
    ""
    ""
    ""
    problem145

problem145 :: Int
problem145 = 0

----------------------------------------

answer146 :: Sol
answer146 = sol 146
    ""
    ""
    ""
    problem146

problem146 :: Int
problem146 = 0

----------------------------------------

answer147 :: Sol
answer147 = sol 147
    ""
    ""
    ""
    problem147

problem147 :: Int
problem147 = 0

----------------------------------------

answer148 :: Sol
answer148 = sol 148
    ""
    ""
    ""
    problem148

problem148 :: Int
problem148 = 0

----------------------------------------

answer149 :: Sol
answer149 = sol 149
    ""
    ""
    ""
    problem149

problem149 :: Int
problem149 = 0

----------------------------------------

answer150 :: Sol
answer150 = sol 150
    ""
    ""
    ""
    problem150

problem150 :: Int
problem150 = 0

----------------------------------------

answer151 :: Sol
answer151 = sol 151
    ""
    ""
    ""
    problem151

problem151 :: Int
problem151 = 0

----------------------------------------

answer152 :: Sol
answer152 = sol 152
    ""
    ""
    ""
    problem152

problem152 :: Int
problem152 = 0

----------------------------------------

answer153 :: Sol
answer153 = sol 153
    ""
    ""
    ""
    problem153

problem153 :: Int
problem153 = 0

----------------------------------------

answer154 :: Sol
answer154 = sol 154
    ""
    ""
    ""
    problem154

problem154 :: Int
problem154 = 0

----------------------------------------

answer155 :: Sol
answer155 = sol 155
    ""
    ""
    ""
    problem155

problem155 :: Int
problem155 = 0

----------------------------------------

answer156 :: Sol
answer156 = sol 156
    ""
    ""
    ""
    problem156

problem156 :: Int
problem156 = 0

----------------------------------------

answer157 :: Sol
answer157 = sol 157
    ""
    ""
    ""
    problem157

problem157 :: Int
problem157 = 0

----------------------------------------

answer158 :: Sol
answer158 = sol 158
    ""
    ""
    ""
    problem158

problem158 :: Int
problem158 = 0

----------------------------------------

answer159 :: Sol
answer159 = sol 159
    ""
    ""
    ""
    problem159

problem159 :: Int
problem159 = 0

----------------------------------------

answer160 :: Sol
answer160 = sol 160
    ""
    ""
    ""
    problem160

problem160 :: Int
problem160 = 0

----------------------------------------

answer161 :: Sol
answer161 = sol 161
    ""
    ""
    ""
    problem161

problem161 :: Int
problem161 = 0

----------------------------------------

answer162 :: Sol
answer162 = sol 162
    ""
    ""
    ""
    problem162

problem162 :: Int
problem162 = 0

----------------------------------------

answer163 :: Sol
answer163 = sol 163
    ""
    ""
    ""
    problem163

problem163 :: Int
problem163 = 0

----------------------------------------

answer164 :: Sol
answer164 = sol 164
    ""
    ""
    ""
    problem164

problem164 :: Int
problem164 = 0

----------------------------------------

answer165 :: Sol
answer165 = sol 165
    ""
    ""
    ""
    problem165

problem165 :: Int
problem165 = 0

----------------------------------------

answer166 :: Sol
answer166 = sol 166
    ""
    ""
    ""
    problem166

problem166 :: Int
problem166 = 0

----------------------------------------

answer167 :: Sol
answer167 = sol 167
    ""
    ""
    ""
    problem167

problem167 :: Int
problem167 = 0

----------------------------------------

answer168 :: Sol
answer168 = sol 168
    ""
    ""
    ""
    problem168

problem168 :: Int
problem168 = 0

----------------------------------------

answer169 :: Sol
answer169 = sol 169
    ""
    ""
    ""
    problem169

problem169 :: Int
problem169 = 0

----------------------------------------

answer170 :: Sol
answer170 = sol 170
    ""
    ""
    ""
    problem170

problem170 :: Int
problem170 = 0

----------------------------------------

answer171 :: Sol
answer171 = sol 171
    ""
    ""
    ""
    problem171

problem171 :: Int
problem171 = 0

----------------------------------------

answer172 :: Sol
answer172 = sol 172
    ""
    ""
    ""
    problem172

problem172 :: Int
problem172 = 0

----------------------------------------

answer173 :: Sol
answer173 = sol 173
    ""
    ""
    ""
    problem173

problem173 :: Int
problem173 = 0

----------------------------------------

answer174 :: Sol
answer174 = sol 174
    ""
    ""
    ""
    problem174

problem174 :: Int
problem174 = 0

----------------------------------------

answer175 :: Sol
answer175 = sol 175
    ""
    ""
    ""
    problem175

problem175 :: Int
problem175 = 0

----------------------------------------

answer176 :: Sol
answer176 = sol 176
    ""
    ""
    ""
    problem176

problem176 :: Int
problem176 = 0

----------------------------------------

answer177 :: Sol
answer177 = sol 177
    ""
    ""
    ""
    problem177

problem177 :: Int
problem177 = 0

----------------------------------------

answer178 :: Sol
answer178 = sol 178
    ""
    ""
    ""
    problem178

problem178 :: Int
problem178 = 0

----------------------------------------

answer179 :: Sol
answer179 = sol 179
    ""
    ""
    ""
    problem179

problem179 :: Int
problem179 = 0

----------------------------------------

answer180 :: Sol
answer180 = sol 180
    ""
    ""
    ""
    problem180

problem180 :: Int
problem180 = 0

----------------------------------------

answer181 :: Sol
answer181 = sol 181
    ""
    ""
    ""
    problem181

problem181 :: Int
problem181 = 0

----------------------------------------

answer182 :: Sol
answer182 = sol 182
    ""
    ""
    ""
    problem182

problem182 :: Int
problem182 = 0

----------------------------------------

answer183 :: Sol
answer183 = sol 183
    ""
    ""
    ""
    problem183

problem183 :: Int
problem183 = 0

----------------------------------------

answer184 :: Sol
answer184 = sol 184
    ""
    ""
    ""
    problem184

problem184 :: Int
problem184 = 0

----------------------------------------

answer185 :: Sol
answer185 = sol 185
    ""
    ""
    ""
    problem185

problem185 :: Int
problem185 = 0

----------------------------------------

answer186 :: Sol
answer186 = sol 186
    ""
    ""
    ""
    problem186

problem186 :: Int
problem186 = 0

----------------------------------------

answer187 :: Sol
answer187 = sol 187
    ""
    ""
    ""
    problem187

problem187 :: Int
problem187 = 0

----------------------------------------

answer188 :: Sol
answer188 = sol 188
    ""
    ""
    ""
    problem188

problem188 :: Int
problem188 = 0

----------------------------------------

answer189 :: Sol
answer189 = sol 189
    ""
    ""
    ""
    problem189

problem189 :: Int
problem189 = 0

----------------------------------------

answer190 :: Sol
answer190 = sol 190
    ""
    ""
    ""
    problem190

problem190 :: Int
problem190 = 0

----------------------------------------

answer191 :: Sol
answer191 = sol 191
    ""
    ""
    ""
    problem191

problem191 :: Int
problem191 = 0

----------------------------------------

answer192 :: Sol
answer192 = sol 192
    ""
    ""
    ""
    problem192

problem192 :: Int
problem192 = 0

----------------------------------------

answer193 :: Sol
answer193 = sol 193
    ""
    ""
    ""
    problem193

problem193 :: Int
problem193 = 0

----------------------------------------

answer194 :: Sol
answer194 = sol 194
    ""
    ""
    ""
    problem194

problem194 :: Int
problem194 = 0

----------------------------------------

answer195 :: Sol
answer195 = sol 195
    ""
    ""
    ""
    problem195

problem195 :: Int
problem195 = 0

----------------------------------------

answer196 :: Sol
answer196 = sol 196
    ""
    ""
    ""
    problem196

problem196 :: Int
problem196 = 0

----------------------------------------

answer197 :: Sol
answer197 = sol 197
    ""
    ""
    ""
    problem197

problem197 :: Int
problem197 = 0

----------------------------------------

answer198 :: Sol
answer198 = sol 198
    ""
    ""
    ""
    problem198

problem198 :: Int
problem198 = 0

----------------------------------------

answer199 :: Sol
answer199 = sol 199
    ""
    ""
    ""
    problem199

problem199 :: Int
problem199 = 0

----------------------------------------

answer200 :: Sol
answer200 = sol 200
    ""
    ""
    ""
    problem200

problem200 :: Int
problem200 = 0

----------------------------------------

answer201 :: Sol
answer201 = sol 201
    ""
    ""
    ""
    problem201

problem201 :: Int
problem201 = 0

----------------------------------------

answer202 :: Sol
answer202 = sol 202
    ""
    ""
    ""
    problem202

problem202 :: Int
problem202 = 0

----------------------------------------

answer203 :: Sol
answer203 = sol 203
    ""
    ""
    ""
    problem203

problem203 :: Int
problem203 = 0

----------------------------------------

answer204 :: Sol
answer204 = sol 204
    ""
    ""
    ""
    problem204

problem204 :: Int
problem204 = 0

----------------------------------------

answer205 :: Sol
answer205 = sol 205
    ""
    ""
    ""
    problem205

problem205 :: Int
problem205 = 0

----------------------------------------

answer206 :: Sol
answer206 = sol 206
    ""
    ""
    ""
    problem206

problem206 :: Int
problem206 = 0

----------------------------------------

answer207 :: Sol
answer207 = sol 207
    ""
    ""
    ""
    problem207

problem207 :: Int
problem207 = 0

----------------------------------------

answer208 :: Sol
answer208 = sol 208
    ""
    ""
    ""
    problem208

problem208 :: Int
problem208 = 0

----------------------------------------

answer209 :: Sol
answer209 = sol 209
    ""
    ""
    ""
    problem209

problem209 :: Int
problem209 = 0

----------------------------------------

answer210 :: Sol
answer210 = sol 210
    ""
    ""
    ""
    problem210

problem210 :: Int
problem210 = 0

----------------------------------------

answer211 :: Sol
answer211 = sol 211
    ""
    ""
    ""
    problem211

problem211 :: Int
problem211 = 0

----------------------------------------

answer212 :: Sol
answer212 = sol 212
    ""
    ""
    ""
    problem212

problem212 :: Int
problem212 = 0

----------------------------------------

answer213 :: Sol
answer213 = sol 213
    ""
    ""
    ""
    problem213

problem213 :: Int
problem213 = 0

----------------------------------------

answer214 :: Sol
answer214 = sol 214
    ""
    ""
    ""
    problem214

problem214 :: Int
problem214 = 0

----------------------------------------

answer215 :: Sol
answer215 = sol 215
    ""
    ""
    ""
    problem215

problem215 :: Int
problem215 = 0

----------------------------------------

answer216 :: Sol
answer216 = sol 216
    ""
    ""
    ""
    problem216

problem216 :: Int
problem216 = 0

----------------------------------------

answer217 :: Sol
answer217 = sol 217
    ""
    ""
    ""
    problem217

problem217 :: Int
problem217 = 0

----------------------------------------

answer218 :: Sol
answer218 = sol 218
    ""
    ""
    ""
    problem218

problem218 :: Int
problem218 = 0

----------------------------------------

answer219 :: Sol
answer219 = sol 219
    ""
    ""
    ""
    problem219

problem219 :: Int
problem219 = 0

----------------------------------------

answer220 :: Sol
answer220 = sol 220
    ""
    ""
    ""
    problem220

problem220 :: Int
problem220 = 0

----------------------------------------

answer221 :: Sol
answer221 = sol 221
    ""
    ""
    ""
    problem221

problem221 :: Int
problem221 = 0

----------------------------------------

answer222 :: Sol
answer222 = sol 222
    ""
    ""
    ""
    problem222

problem222 :: Int
problem222 = 0

----------------------------------------

answer223 :: Sol
answer223 = sol 223
    ""
    ""
    ""
    problem223

problem223 :: Int
problem223 = 0

----------------------------------------

answer224 :: Sol
answer224 = sol 224
    ""
    ""
    ""
    problem224

problem224 :: Int
problem224 = 0

----------------------------------------

answer225 :: Sol
answer225 = sol 225
    ""
    ""
    ""
    problem225

problem225 :: Int
problem225 = 0

----------------------------------------

answer226 :: Sol
answer226 = sol 226
    ""
    ""
    ""
    problem226

problem226 :: Int
problem226 = 0

----------------------------------------

answer227 :: Sol
answer227 = sol 227
    ""
    ""
    ""
    problem227

problem227 :: Int
problem227 = 0

----------------------------------------

answer228 :: Sol
answer228 = sol 228
    ""
    ""
    ""
    problem228

problem228 :: Int
problem228 = 0

----------------------------------------

answer229 :: Sol
answer229 = sol 229
    ""
    ""
    ""
    problem229

problem229 :: Int
problem229 = 0

----------------------------------------

answer230 :: Sol
answer230 = sol 230
    ""
    ""
    ""
    problem230

problem230 :: Int
problem230 = 0

----------------------------------------

answer231 :: Sol
answer231 = sol 231
    ""
    ""
    ""
    problem231

problem231 :: Int
problem231 = 0

----------------------------------------

answer232 :: Sol
answer232 = sol 232
    ""
    ""
    ""
    problem232

problem232 :: Int
problem232 = 0

----------------------------------------

answer233 :: Sol
answer233 = sol 233
    ""
    ""
    ""
    problem233

problem233 :: Int
problem233 = 0

----------------------------------------

answer234 :: Sol
answer234 = sol 234
    ""
    ""
    ""
    problem234

problem234 :: Int
problem234 = 0

----------------------------------------

answer235 :: Sol
answer235 = sol 235
    ""
    ""
    ""
    problem235

problem235 :: Int
problem235 = 0

----------------------------------------

answer236 :: Sol
answer236 = sol 236
    ""
    ""
    ""
    problem236

problem236 :: Int
problem236 = 0

----------------------------------------

answer237 :: Sol
answer237 = sol 237
    ""
    ""
    ""
    problem237

problem237 :: Int
problem237 = 0

----------------------------------------

answer238 :: Sol
answer238 = sol 238
    ""
    ""
    ""
    problem238

problem238 :: Int
problem238 = 0

----------------------------------------

answer239 :: Sol
answer239 = sol 239
    ""
    ""
    ""
    problem239

problem239 :: Int
problem239 = 0

----------------------------------------

answer240 :: Sol
answer240 = sol 240
    ""
    ""
    ""
    problem240

problem240 :: Int
problem240 = 0

----------------------------------------

answer241 :: Sol
answer241 = sol 241
    ""
    ""
    ""
    problem241

problem241 :: Int
problem241 = 0

----------------------------------------

answer242 :: Sol
answer242 = sol 242
    ""
    ""
    ""
    problem242

problem242 :: Int
problem242 = 0

----------------------------------------

answer243 :: Sol
answer243 = sol 243
    ""
    ""
    ""
    problem243

problem243 :: Int
problem243 = 0

----------------------------------------

answer244 :: Sol
answer244 = sol 244
    ""
    ""
    ""
    problem244

problem244 :: Int
problem244 = 0

----------------------------------------

answer245 :: Sol
answer245 = sol 245
    ""
    ""
    ""
    problem245

problem245 :: Int
problem245 = 0

----------------------------------------

answer246 :: Sol
answer246 = sol 246
    ""
    ""
    ""
    problem246

problem246 :: Int
problem246 = 0

----------------------------------------

answer247 :: Sol
answer247 = sol 247
    ""
    ""
    ""
    problem247

problem247 :: Int
problem247 = 0

----------------------------------------

answer248 :: Sol
answer248 = sol 248
    ""
    ""
    ""
    problem248

problem248 :: Int
problem248 = 0

----------------------------------------

answer249 :: Sol
answer249 = sol 249
    ""
    ""
    ""
    problem249

problem249 :: Int
problem249 = 0

----------------------------------------

answer250 :: Sol
answer250 = sol 250
    ""
    ""
    ""
    problem250

problem250 :: Int
problem250 = 0

----------------------------------------

answer251 :: Sol
answer251 = sol 251
    ""
    ""
    ""
    problem251

problem251 :: Int
problem251 = 0

----------------------------------------

answer252 :: Sol
answer252 = sol 252
    ""
    ""
    ""
    problem252

problem252 :: Int
problem252 = 0

----------------------------------------

answer253 :: Sol
answer253 = sol 253
    ""
    ""
    ""
    problem253

problem253 :: Int
problem253 = 0

----------------------------------------

answer254 :: Sol
answer254 = sol 254
    ""
    ""
    ""
    problem254

problem254 :: Int
problem254 = 0

----------------------------------------

answer255 :: Sol
answer255 = sol 255
    ""
    ""
    ""
    problem255

problem255 :: Int
problem255 = 0

----------------------------------------

answer256 :: Sol
answer256 = sol 256
    ""
    ""
    ""
    problem256

problem256 :: Int
problem256 = 0

----------------------------------------

answer257 :: Sol
answer257 = sol 257
    ""
    ""
    ""
    problem257

problem257 :: Int
problem257 = 0

----------------------------------------

answer258 :: Sol
answer258 = sol 258
    ""
    ""
    ""
    problem258

problem258 :: Int
problem258 = 0

----------------------------------------

answer259 :: Sol
answer259 = sol 259
    ""
    ""
    ""
    problem259

problem259 :: Int
problem259 = 0

----------------------------------------

answer260 :: Sol
answer260 = sol 260
    ""
    ""
    ""
    problem260

problem260 :: Int
problem260 = 0

----------------------------------------

answer261 :: Sol
answer261 = sol 261
    ""
    ""
    ""
    problem261

problem261 :: Int
problem261 = 0

----------------------------------------

answer262 :: Sol
answer262 = sol 262
    ""
    ""
    ""
    problem262

problem262 :: Int
problem262 = 0

----------------------------------------

answer263 :: Sol
answer263 = sol 263
    ""
    ""
    ""
    problem263

problem263 :: Int
problem263 = 0

----------------------------------------

answer264 :: Sol
answer264 = sol 264
    ""
    ""
    ""
    problem264

problem264 :: Int
problem264 = 0

----------------------------------------

answer265 :: Sol
answer265 = sol 265
    ""
    ""
    ""
    problem265

problem265 :: Int
problem265 = 0

----------------------------------------

answer266 :: Sol
answer266 = sol 266
    ""
    ""
    ""
    problem266

problem266 :: Int
problem266 = 0

----------------------------------------

answer267 :: Sol
answer267 = sol 267
    ""
    ""
    ""
    problem267

problem267 :: Int
problem267 = 0

----------------------------------------

answer268 :: Sol
answer268 = sol 268
    ""
    ""
    ""
    problem268

problem268 :: Int
problem268 = 0

----------------------------------------

answer269 :: Sol
answer269 = sol 269
    ""
    ""
    ""
    problem269

problem269 :: Int
problem269 = 0

----------------------------------------

answer270 :: Sol
answer270 = sol 270
    ""
    ""
    ""
    problem270

problem270 :: Int
problem270 = 0

----------------------------------------

answer271 :: Sol
answer271 = sol 271
    ""
    ""
    ""
    problem271

problem271 :: Int
problem271 = 0

----------------------------------------

answer272 :: Sol
answer272 = sol 272
    ""
    ""
    ""
    problem272

problem272 :: Int
problem272 = 0

----------------------------------------

answer273 :: Sol
answer273 = sol 273
    ""
    ""
    ""
    problem273

problem273 :: Int
problem273 = 0

----------------------------------------

answer274 :: Sol
answer274 = sol 274
    ""
    ""
    ""
    problem274

problem274 :: Int
problem274 = 0

----------------------------------------

answer275 :: Sol
answer275 = sol 275
    ""
    ""
    ""
    problem275

problem275 :: Int
problem275 = 0

----------------------------------------

answer276 :: Sol
answer276 = sol 276
    ""
    ""
    ""
    problem276

problem276 :: Int
problem276 = 0

----------------------------------------

answer277 :: Sol
answer277 = sol 277
    ""
    ""
    ""
    problem277

problem277 :: Int
problem277 = 0

----------------------------------------

answer278 :: Sol
answer278 = sol 278
    ""
    ""
    ""
    problem278

problem278 :: Int
problem278 = 0

----------------------------------------

answer279 :: Sol
answer279 = sol 279
    ""
    ""
    ""
    problem279

problem279 :: Int
problem279 = 0

----------------------------------------

answer280 :: Sol
answer280 = sol 280
    ""
    ""
    ""
    problem280

problem280 :: Int
problem280 = 0

----------------------------------------

answer281 :: Sol
answer281 = sol 281
    ""
    ""
    ""
    problem281

problem281 :: Int
problem281 = 0

----------------------------------------

answer282 :: Sol
answer282 = sol 282
    ""
    ""
    ""
    problem282

problem282 :: Int
problem282 = 0

----------------------------------------

answer283 :: Sol
answer283 = sol 283
    ""
    ""
    ""
    problem283

problem283 :: Int
problem283 = 0

----------------------------------------

answer284 :: Sol
answer284 = sol 284
    ""
    ""
    ""
    problem284

problem284 :: Int
problem284 = 0

----------------------------------------

answer285 :: Sol
answer285 = sol 285
    ""
    ""
    ""
    problem285

problem285 :: Int
problem285 = 0

----------------------------------------

answer286 :: Sol
answer286 = sol 286
    ""
    ""
    ""
    problem286

problem286 :: Int
problem286 = 0

----------------------------------------

answer287 :: Sol
answer287 = sol 287
    ""
    ""
    ""
    problem287

problem287 :: Int
problem287 = 0

----------------------------------------

answer288 :: Sol
answer288 = sol 288
    ""
    ""
    ""
    problem288

problem288 :: Int
problem288 = 0

----------------------------------------

answer289 :: Sol
answer289 = sol 289
    ""
    ""
    ""
    problem289

problem289 :: Int
problem289 = 0

----------------------------------------

answer290 :: Sol
answer290 = sol 290
    ""
    ""
    ""
    problem290

problem290 :: Int
problem290 = 0

----------------------------------------

answer291 :: Sol
answer291 = sol 291
    ""
    ""
    ""
    problem291

problem291 :: Int
problem291 = 0

----------------------------------------

answer292 :: Sol
answer292 = sol 292
    ""
    ""
    ""
    problem292

problem292 :: Int
problem292 = 0

----------------------------------------

answer293 :: Sol
answer293 = sol 293
    ""
    ""
    ""
    problem293

problem293 :: Int
problem293 = 0

----------------------------------------

answer294 :: Sol
answer294 = sol 294
    ""
    ""
    ""
    problem294

problem294 :: Int
problem294 = 0

----------------------------------------

answer295 :: Sol
answer295 = sol 295
    ""
    ""
    ""
    problem295

problem295 :: Int
problem295 = 0

----------------------------------------

answer296 :: Sol
answer296 = sol 296
    ""
    ""
    ""
    problem296

problem296 :: Int
problem296 = 0

----------------------------------------

answer297 :: Sol
answer297 = sol 297
    ""
    ""
    ""
    problem297

problem297 :: Int
problem297 = 0

----------------------------------------

answer298 :: Sol
answer298 = sol 298
    ""
    ""
    ""
    problem298

problem298 :: Int
problem298 = 0

----------------------------------------

answer299 :: Sol
answer299 = sol 299
    ""
    ""
    ""
    problem299

problem299 :: Int
problem299 = 0

----------------------------------------

answer300 :: Sol
answer300 = sol 300
    ""
    ""
    ""
    problem300

problem300 :: Int
problem300 = 0

----------------------------------------

answer301 :: Sol
answer301 = sol 301
    ""
    ""
    ""
    problem301

problem301 :: Int
problem301 = 0

----------------------------------------

answer302 :: Sol
answer302 = sol 302
    ""
    ""
    ""
    problem302

problem302 :: Int
problem302 = 0

----------------------------------------

answer303 :: Sol
answer303 = sol 303
    ""
    ""
    ""
    problem303

problem303 :: Int
problem303 = 0

----------------------------------------

answer304 :: Sol
answer304 = sol 304
    ""
    ""
    ""
    problem304

problem304 :: Int
problem304 = 0

----------------------------------------

answer305 :: Sol
answer305 = sol 305
    ""
    ""
    ""
    problem305

problem305 :: Int
problem305 = 0

----------------------------------------

answer306 :: Sol
answer306 = sol 306
    ""
    ""
    ""
    problem306

problem306 :: Int
problem306 = 0

----------------------------------------

answer307 :: Sol
answer307 = sol 307
    ""
    ""
    ""
    problem307

problem307 :: Int
problem307 = 0

----------------------------------------

answer308 :: Sol
answer308 = sol 308
    ""
    ""
    ""
    problem308

problem308 :: Int
problem308 = 0

----------------------------------------

answer309 :: Sol
answer309 = sol 309
    ""
    ""
    ""
    problem309

problem309 :: Int
problem309 = 0

----------------------------------------

answer310 :: Sol
answer310 = sol 310
    ""
    ""
    ""
    problem310

problem310 :: Int
problem310 = 0

----------------------------------------

answer311 :: Sol
answer311 = sol 311
    ""
    ""
    ""
    problem311

problem311 :: Int
problem311 = 0

----------------------------------------

answer312 :: Sol
answer312 = sol 312
    ""
    ""
    ""
    problem312

problem312 :: Int
problem312 = 0

----------------------------------------

answer313 :: Sol
answer313 = sol 313
    ""
    ""
    ""
    problem313

problem313 :: Int
problem313 = 0

----------------------------------------

answer314 :: Sol
answer314 = sol 314
    ""
    ""
    ""
    problem314

problem314 :: Int
problem314 = 0

----------------------------------------

answer315 :: Sol
answer315 = sol 315
    ""
    ""
    ""
    problem315

problem315 :: Int
problem315 = 0

----------------------------------------

answer316 :: Sol
answer316 = sol 316
    ""
    ""
    ""
    problem316

problem316 :: Int
problem316 = 0

----------------------------------------

answer317 :: Sol
answer317 = sol 317
    ""
    ""
    ""
    problem317

problem317 :: Int
problem317 = 0

----------------------------------------

answer318 :: Sol
answer318 = sol 318
    ""
    ""
    ""
    problem318

problem318 :: Int
problem318 = 0

----------------------------------------

answer319 :: Sol
answer319 = sol 319
    ""
    ""
    ""
    problem319

problem319 :: Int
problem319 = 0

----------------------------------------

answer320 :: Sol
answer320 = sol 320
    ""
    ""
    ""
    problem320

problem320 :: Int
problem320 = 0

----------------------------------------

answer321 :: Sol
answer321 = sol 321
    ""
    ""
    ""
    problem321

problem321 :: Int
problem321 = 0

----------------------------------------

answer322 :: Sol
answer322 = sol 322
    ""
    ""
    ""
    problem322

problem322 :: Int
problem322 = 0

----------------------------------------

answer323 :: Sol
answer323 = sol 323
    ""
    ""
    ""
    problem323

problem323 :: Int
problem323 = 0

----------------------------------------

answer324 :: Sol
answer324 = sol 324
    ""
    ""
    ""
    problem324

problem324 :: Int
problem324 = 0

----------------------------------------

answer325 :: Sol
answer325 = sol 325
    ""
    ""
    ""
    problem325

problem325 :: Int
problem325 = 0

----------------------------------------

answer326 :: Sol
answer326 = sol 326
    ""
    ""
    ""
    problem326

problem326 :: Int
problem326 = 0

----------------------------------------

answer327 :: Sol
answer327 = sol 327
    ""
    ""
    ""
    problem327

problem327 :: Int
problem327 = 0

----------------------------------------

answer328 :: Sol
answer328 = sol 328
    ""
    ""
    ""
    problem328

problem328 :: Int
problem328 = 0

----------------------------------------

answer329 :: Sol
answer329 = sol 329
    ""
    ""
    ""
    problem329

problem329 :: Int
problem329 = 0

----------------------------------------

answer330 :: Sol
answer330 = sol 330
    ""
    ""
    ""
    problem330

problem330 :: Int
problem330 = 0

----------------------------------------

answer331 :: Sol
answer331 = sol 331
    ""
    ""
    ""
    problem331

problem331 :: Int
problem331 = 0

----------------------------------------

answer332 :: Sol
answer332 = sol 332
    ""
    ""
    ""
    problem332

problem332 :: Int
problem332 = 0

----------------------------------------

answer333 :: Sol
answer333 = sol 333
    ""
    ""
    ""
    problem333

problem333 :: Int
problem333 = 0

----------------------------------------

answer334 :: Sol
answer334 = sol 334
    ""
    ""
    ""
    problem334

problem334 :: Int
problem334 = 0

----------------------------------------

answer335 :: Sol
answer335 = sol 335
    ""
    ""
    ""
    problem335

problem335 :: Int
problem335 = 0

----------------------------------------

answer336 :: Sol
answer336 = sol 336
    ""
    ""
    ""
    problem336

problem336 :: Int
problem336 = 0

----------------------------------------

answer337 :: Sol
answer337 = sol 337
    ""
    ""
    ""
    problem337

problem337 :: Int
problem337 = 0

----------------------------------------

answer338 :: Sol
answer338 = sol 338
    ""
    ""
    ""
    problem338

problem338 :: Int
problem338 = 0

----------------------------------------

answer339 :: Sol
answer339 = sol 339
    ""
    ""
    ""
    problem339

problem339 :: Int
problem339 = 0

----------------------------------------

answer340 :: Sol
answer340 = sol 340
    ""
    ""
    ""
    problem340

problem340 :: Int
problem340 = 0

----------------------------------------

answer341 :: Sol
answer341 = sol 341
    ""
    ""
    ""
    problem341

problem341 :: Int
problem341 = 0

----------------------------------------

answer342 :: Sol
answer342 = sol 342
    ""
    ""
    ""
    problem342

problem342 :: Int
problem342 = 0

----------------------------------------

answer343 :: Sol
answer343 = sol 343
    ""
    ""
    ""
    problem343

problem343 :: Int
problem343 = 0

----------------------------------------

answer344 :: Sol
answer344 = sol 344
    ""
    ""
    ""
    problem344

problem344 :: Int
problem344 = 0

----------------------------------------

answer345 :: Sol
answer345 = sol 345
    ""
    ""
    ""
    problem345

problem345 :: Int
problem345 = 0

----------------------------------------

answer346 :: Sol
answer346 = sol 346
    ""
    ""
    ""
    problem346

problem346 :: Int
problem346 = 0

----------------------------------------

answer347 :: Sol
answer347 = sol 347
    ""
    ""
    ""
    problem347

problem347 :: Int
problem347 = 0

----------------------------------------

answer348 :: Sol
answer348 = sol 348
    ""
    ""
    ""
    problem348

problem348 :: Int
problem348 = 0

----------------------------------------

answer349 :: Sol
answer349 = sol 349
    ""
    ""
    ""
    problem349

problem349 :: Int
problem349 = 0

----------------------------------------

answer350 :: Sol
answer350 = sol 350
    ""
    ""
    ""
    problem350

problem350 :: Int
problem350 = 0

----------------------------------------

answer351 :: Sol
answer351 = sol 351
    ""
    ""
    ""
    problem351

problem351 :: Int
problem351 = 0

----------------------------------------

answer352 :: Sol
answer352 = sol 352
    ""
    ""
    ""
    problem352

problem352 :: Int
problem352 = 0

----------------------------------------

answer353 :: Sol
answer353 = sol 353
    ""
    ""
    ""
    problem353

problem353 :: Int
problem353 = 0

----------------------------------------

answer354 :: Sol
answer354 = sol 354
    ""
    ""
    ""
    problem354

problem354 :: Int
problem354 = 0

----------------------------------------

answer355 :: Sol
answer355 = sol 355
    ""
    ""
    ""
    problem355

problem355 :: Int
problem355 = 0

----------------------------------------

answer356 :: Sol
answer356 = sol 356
    ""
    ""
    ""
    problem356

problem356 :: Int
problem356 = 0

----------------------------------------

answer357 :: Sol
answer357 = sol 357
    ""
    ""
    ""
    problem357

problem357 :: Int
problem357 = 0

----------------------------------------

answer358 :: Sol
answer358 = sol 358
    ""
    ""
    ""
    problem358

problem358 :: Int
problem358 = 0

----------------------------------------

answer359 :: Sol
answer359 = sol 359
    ""
    ""
    ""
    problem359

problem359 :: Int
problem359 = 0

----------------------------------------

answer360 :: Sol
answer360 = sol 360
    ""
    ""
    ""
    problem360

problem360 :: Int
problem360 = 0

----------------------------------------

answer361 :: Sol
answer361 = sol 361
    ""
    ""
    ""
    problem361

problem361 :: Int
problem361 = 0

----------------------------------------

answer362 :: Sol
answer362 = sol 362
    ""
    ""
    ""
    problem362

problem362 :: Int
problem362 = 0

----------------------------------------

answer363 :: Sol
answer363 = sol 363
    ""
    ""
    ""
    problem363

problem363 :: Int
problem363 = 0

----------------------------------------

answer364 :: Sol
answer364 = sol 364
    ""
    ""
    ""
    problem364

problem364 :: Int
problem364 = 0

----------------------------------------

answer365 :: Sol
answer365 = sol 365
    ""
    ""
    ""
    problem365

problem365 :: Int
problem365 = 0

----------------------------------------

answer366 :: Sol
answer366 = sol 366
    ""
    ""
    ""
    problem366

problem366 :: Int
problem366 = 0

----------------------------------------

answer367 :: Sol
answer367 = sol 367
    ""
    ""
    ""
    problem367

problem367 :: Int
problem367 = 0

----------------------------------------

answer368 :: Sol
answer368 = sol 368
    ""
    ""
    ""
    problem368

problem368 :: Int
problem368 = 0

----------------------------------------

answer369 :: Sol
answer369 = sol 369
    ""
    ""
    ""
    problem369

problem369 :: Int
problem369 = 0

----------------------------------------

answer370 :: Sol
answer370 = sol 370
    ""
    ""
    ""
    problem370

problem370 :: Int
problem370 = 0

----------------------------------------

answer371 :: Sol
answer371 = sol 371
    ""
    ""
    ""
    problem371

problem371 :: Int
problem371 = 0

----------------------------------------

answer372 :: Sol
answer372 = sol 372
    ""
    ""
    ""
    problem372

problem372 :: Int
problem372 = 0

----------------------------------------

answer373 :: Sol
answer373 = sol 373
    ""
    ""
    ""
    problem373

problem373 :: Int
problem373 = 0

----------------------------------------

answer374 :: Sol
answer374 = sol 374
    ""
    ""
    ""
    problem374

problem374 :: Int
problem374 = 0

----------------------------------------

answer375 :: Sol
answer375 = sol 375
    ""
    ""
    ""
    problem375

problem375 :: Int
problem375 = 0

----------------------------------------

answer376 :: Sol
answer376 = sol 376
    ""
    ""
    ""
    problem376

problem376 :: Int
problem376 = 0

----------------------------------------

answer377 :: Sol
answer377 = sol 377
    ""
    ""
    ""
    problem377

problem377 :: Int
problem377 = 0

----------------------------------------

answer378 :: Sol
answer378 = sol 378
    ""
    ""
    ""
    problem378

problem378 :: Int
problem378 = 0

----------------------------------------

answer379 :: Sol
answer379 = sol 379
    ""
    ""
    ""
    problem379

problem379 :: Int
problem379 = 0

----------------------------------------

answer380 :: Sol
answer380 = sol 380
    ""
    ""
    ""
    problem380

problem380 :: Int
problem380 = 0

----------------------------------------

answer381 :: Sol
answer381 = sol 381
    ""
    ""
    ""
    problem381

problem381 :: Int
problem381 = 0

----------------------------------------

answer382 :: Sol
answer382 = sol 382
    ""
    ""
    ""
    problem382

problem382 :: Int
problem382 = 0

----------------------------------------

answer383 :: Sol
answer383 = sol 383
    ""
    ""
    ""
    problem383

problem383 :: Int
problem383 = 0

----------------------------------------

answer384 :: Sol
answer384 = sol 384
    ""
    ""
    ""
    problem384

problem384 :: Int
problem384 = 0

----------------------------------------

answer385 :: Sol
answer385 = sol 385
    ""
    ""
    ""
    problem385

problem385 :: Int
problem385 = 0

----------------------------------------

answer386 :: Sol
answer386 = sol 386
    ""
    ""
    ""
    problem386

problem386 :: Int
problem386 = 0

----------------------------------------

answer387 :: Sol
answer387 = sol 387
    ""
    ""
    ""
    problem387

problem387 :: Int
problem387 = 0

----------------------------------------

answer388 :: Sol
answer388 = sol 388
    ""
    ""
    ""
    problem388

problem388 :: Int
problem388 = 0

----------------------------------------

answer389 :: Sol
answer389 = sol 389
    ""
    ""
    ""
    problem389

problem389 :: Int
problem389 = 0

----------------------------------------

answer390 :: Sol
answer390 = sol 390
    ""
    ""
    ""
    problem390

problem390 :: Int
problem390 = 0

----------------------------------------

answer391 :: Sol
answer391 = sol 391
    ""
    ""
    ""
    problem391

problem391 :: Int
problem391 = 0

----------------------------------------

answer392 :: Sol
answer392 = sol 392
    ""
    ""
    ""
    problem392

problem392 :: Int
problem392 = 0

----------------------------------------

answer393 :: Sol
answer393 = sol 393
    ""
    ""
    ""
    problem393

problem393 :: Int
problem393 = 0

----------------------------------------

answer394 :: Sol
answer394 = sol 394
    ""
    ""
    ""
    problem394

problem394 :: Int
problem394 = 0

----------------------------------------

answer395 :: Sol
answer395 = sol 395
    ""
    ""
    ""
    problem395

problem395 :: Int
problem395 = 0

----------------------------------------

answer396 :: Sol
answer396 = sol 396
    ""
    ""
    ""
    problem396

problem396 :: Int
problem396 = 0

----------------------------------------

answer397 :: Sol
answer397 = sol 397
    ""
    ""
    ""
    problem397

problem397 :: Int
problem397 = 0

----------------------------------------

answer398 :: Sol
answer398 = sol 398
    ""
    ""
    ""
    problem398

problem398 :: Int
problem398 = 0

----------------------------------------

answer399 :: Sol
answer399 = sol 399
    ""
    ""
    ""
    problem399

problem399 :: Int
problem399 = 0

----------------------------------------

answer400 :: Sol
answer400 = sol 400
    ""
    ""
    ""
    problem400

problem400 :: Int
problem400 = 0

----------------------------------------

answer401 :: Sol
answer401 = sol 401
    ""
    ""
    ""
    problem401

problem401 :: Int
problem401 = 0

----------------------------------------

answer402 :: Sol
answer402 = sol 402
    ""
    ""
    ""
    problem402

problem402 :: Int
problem402 = 0

----------------------------------------

answer403 :: Sol
answer403 = sol 403
    ""
    ""
    ""
    problem403

problem403 :: Int
problem403 = 0

----------------------------------------

answer404 :: Sol
answer404 = sol 404
    ""
    ""
    ""
    problem404

problem404 :: Int
problem404 = 0

----------------------------------------

answer405 :: Sol
answer405 = sol 405
    ""
    ""
    ""
    problem405

problem405 :: Int
problem405 = 0

----------------------------------------

answer406 :: Sol
answer406 = sol 406
    ""
    ""
    ""
    problem406

problem406 :: Int
problem406 = 0

----------------------------------------

answer407 :: Sol
answer407 = sol 407
    ""
    ""
    ""
    problem407

problem407 :: Int
problem407 = 0

----------------------------------------

answer408 :: Sol
answer408 = sol 408
    ""
    ""
    ""
    problem408

problem408 :: Int
problem408 = 0

----------------------------------------

answer409 :: Sol
answer409 = sol 409
    ""
    ""
    ""
    problem409

problem409 :: Int
problem409 = 0

----------------------------------------

answer410 :: Sol
answer410 = sol 410
    ""
    ""
    ""
    problem410

problem410 :: Int
problem410 = 0

----------------------------------------

answer411 :: Sol
answer411 = sol 411
    ""
    ""
    ""
    problem411

problem411 :: Int
problem411 = 0

----------------------------------------

answer412 :: Sol
answer412 = sol 412
    ""
    ""
    ""
    problem412

problem412 :: Int
problem412 = 0

----------------------------------------

answer413 :: Sol
answer413 = sol 413
    ""
    ""
    ""
    problem413

problem413 :: Int
problem413 = 0

----------------------------------------

answer414 :: Sol
answer414 = sol 414
    ""
    ""
    ""
    problem414

problem414 :: Int
problem414 = 0

----------------------------------------

answer415 :: Sol
answer415 = sol 415
    ""
    ""
    ""
    problem415

problem415 :: Int
problem415 = 0

----------------------------------------

answer416 :: Sol
answer416 = sol 416
    ""
    ""
    ""
    problem416

problem416 :: Int
problem416 = 0

----------------------------------------

answer417 :: Sol
answer417 = sol 417
    ""
    ""
    ""
    problem417

problem417 :: Int
problem417 = 0

----------------------------------------

answer418 :: Sol
answer418 = sol 418
    ""
    ""
    ""
    problem418

problem418 :: Int
problem418 = 0

----------------------------------------

answer419 :: Sol
answer419 = sol 419
    ""
    ""
    ""
    problem419

problem419 :: Int
problem419 = 0

----------------------------------------

answer420 :: Sol
answer420 = sol 420
    ""
    ""
    ""
    problem420

problem420 :: Int
problem420 = 0

----------------------------------------

answer421 :: Sol
answer421 = sol 421
    ""
    ""
    ""
    problem421

problem421 :: Int
problem421 = 0

----------------------------------------

answer422 :: Sol
answer422 = sol 422
    ""
    ""
    ""
    problem422

problem422 :: Int
problem422 = 0

----------------------------------------

answer423 :: Sol
answer423 = sol 423
    ""
    ""
    ""
    problem423

problem423 :: Int
problem423 = 0

----------------------------------------

answer424 :: Sol
answer424 = sol 424
    ""
    ""
    ""
    problem424

problem424 :: Int
problem424 = 0

----------------------------------------

answer425 :: Sol
answer425 = sol 425
    ""
    ""
    ""
    problem425

problem425 :: Int
problem425 = 0

----------------------------------------

answer426 :: Sol
answer426 = sol 426
    ""
    ""
    ""
    problem426

problem426 :: Int
problem426 = 0

----------------------------------------

answer427 :: Sol
answer427 = sol 427
    ""
    ""
    ""
    problem427

problem427 :: Int
problem427 = 0

----------------------------------------

answer428 :: Sol
answer428 = sol 428
    ""
    ""
    ""
    problem428

problem428 :: Int
problem428 = 0

----------------------------------------

answer429 :: Sol
answer429 = sol 429
    ""
    ""
    ""
    problem429

problem429 :: Int
problem429 = 0

----------------------------------------

answer430 :: Sol
answer430 = sol 430
    ""
    ""
    ""
    problem430

problem430 :: Int
problem430 = 0

----------------------------------------

answer431 :: Sol
answer431 = sol 431
    ""
    ""
    ""
    problem431

problem431 :: Int
problem431 = 0

----------------------------------------

answer432 :: Sol
answer432 = sol 432
    ""
    ""
    ""
    problem432

problem432 :: Int
problem432 = 0

----------------------------------------

answer433 :: Sol
answer433 = sol 433
    ""
    ""
    ""
    problem433

problem433 :: Int
problem433 = 0

----------------------------------------

answer434 :: Sol
answer434 = sol 434
    ""
    ""
    ""
    problem434

problem434 :: Int
problem434 = 0

----------------------------------------

answer435 :: Sol
answer435 = sol 435
    ""
    ""
    ""
    problem435

problem435 :: Int
problem435 = 0

----------------------------------------

answer436 :: Sol
answer436 = sol 436
    ""
    ""
    ""
    problem436

problem436 :: Int
problem436 = 0

----------------------------------------

answer437 :: Sol
answer437 = sol 437
    ""
    ""
    ""
    problem437

problem437 :: Int
problem437 = 0

----------------------------------------

answer438 :: Sol
answer438 = sol 438
    ""
    ""
    ""
    problem438

problem438 :: Int
problem438 = 0

----------------------------------------

answer439 :: Sol
answer439 = sol 439
    ""
    ""
    ""
    problem439

problem439 :: Int
problem439 = 0

----------------------------------------

answer440 :: Sol
answer440 = sol 440
    ""
    ""
    ""
    problem440

problem440 :: Int
problem440 = 0

----------------------------------------

answer441 :: Sol
answer441 = sol 441
    ""
    ""
    ""
    problem441

problem441 :: Int
problem441 = 0

----------------------------------------

answer442 :: Sol
answer442 = sol 442
    ""
    ""
    ""
    problem442

problem442 :: Int
problem442 = 0

----------------------------------------

answer443 :: Sol
answer443 = sol 443
    ""
    ""
    ""
    problem443

problem443 :: Int
problem443 = 0

----------------------------------------

answer444 :: Sol
answer444 = sol 444
    ""
    ""
    ""
    problem444

problem444 :: Int
problem444 = 0

----------------------------------------

answer445 :: Sol
answer445 = sol 445
    ""
    ""
    ""
    problem445

problem445 :: Int
problem445 = 0

----------------------------------------

answer446 :: Sol
answer446 = sol 446
    ""
    ""
    ""
    problem446

problem446 :: Int
problem446 = 0

----------------------------------------

answer447 :: Sol
answer447 = sol 447
    ""
    ""
    ""
    problem447

problem447 :: Int
problem447 = 0

----------------------------------------

answer448 :: Sol
answer448 = sol 448
    ""
    ""
    ""
    problem448

problem448 :: Int
problem448 = 0

----------------------------------------

answer449 :: Sol
answer449 = sol 449
    ""
    ""
    ""
    problem449

problem449 :: Int
problem449 = 0

----------------------------------------

answer450 :: Sol
answer450 = sol 450
    ""
    ""
    ""
    problem450

problem450 :: Int
problem450 = 0

----------------------------------------

answer451 :: Sol
answer451 = sol 451
    ""
    ""
    ""
    problem451

problem451 :: Int
problem451 = 0

----------------------------------------

answer452 :: Sol
answer452 = sol 452
    ""
    ""
    ""
    problem452

problem452 :: Int
problem452 = 0

----------------------------------------

answer453 :: Sol
answer453 = sol 453
    ""
    ""
    ""
    problem453

problem453 :: Int
problem453 = 0

----------------------------------------

answer454 :: Sol
answer454 = sol 454
    ""
    ""
    ""
    problem454

problem454 :: Int
problem454 = 0

----------------------------------------

answer455 :: Sol
answer455 = sol 455
    ""
    ""
    ""
    problem455

problem455 :: Int
problem455 = 0

----------------------------------------

answer456 :: Sol
answer456 = sol 456
    ""
    ""
    ""
    problem456

problem456 :: Int
problem456 = 0

----------------------------------------

answer457 :: Sol
answer457 = sol 457
    ""
    ""
    ""
    problem457

problem457 :: Int
problem457 = 0

----------------------------------------

answer458 :: Sol
answer458 = sol 458
    ""
    ""
    ""
    problem458

problem458 :: Int
problem458 = 0

----------------------------------------

answer459 :: Sol
answer459 = sol 459
    ""
    ""
    ""
    problem459

problem459 :: Int
problem459 = 0

----------------------------------------

answer460 :: Sol
answer460 = sol 460
    ""
    ""
    ""
    problem460

problem460 :: Int
problem460 = 0

----------------------------------------

answer461 :: Sol
answer461 = sol 461
    ""
    ""
    ""
    problem461

problem461 :: Int
problem461 = 0

----------------------------------------

answer462 :: Sol
answer462 = sol 462
    ""
    ""
    ""
    problem462

problem462 :: Int
problem462 = 0

----------------------------------------

answer463 :: Sol
answer463 = sol 463
    ""
    ""
    ""
    problem463

problem463 :: Int
problem463 = 0

----------------------------------------

answer464 :: Sol
answer464 = sol 464
    ""
    ""
    ""
    problem464

problem464 :: Int
problem464 = 0

----------------------------------------

answer465 :: Sol
answer465 = sol 465
    ""
    ""
    ""
    problem465

problem465 :: Int
problem465 = 0

----------------------------------------

answer466 :: Sol
answer466 = sol 466
    ""
    ""
    ""
    problem466

problem466 :: Int
problem466 = 0

----------------------------------------

answer467 :: Sol
answer467 = sol 467
    ""
    ""
    ""
    problem467

problem467 :: Int
problem467 = 0

----------------------------------------

answer468 :: Sol
answer468 = sol 468
    ""
    ""
    ""
    problem468

problem468 :: Int
problem468 = 0

----------------------------------------

answer469 :: Sol
answer469 = sol 469
    ""
    ""
    ""
    problem469

problem469 :: Int
problem469 = 0

----------------------------------------

answer470 :: Sol
answer470 = sol 470
    ""
    ""
    ""
    problem470

problem470 :: Int
problem470 = 0

----------------------------------------

answer471 :: Sol
answer471 = sol 471
    ""
    ""
    ""
    problem471

problem471 :: Int
problem471 = 0

----------------------------------------

answer472 :: Sol
answer472 = sol 472
    ""
    ""
    ""
    problem472

problem472 :: Int
problem472 = 0

----------------------------------------

answer473 :: Sol
answer473 = sol 473
    ""
    ""
    ""
    problem473

problem473 :: Int
problem473 = 0

----------------------------------------

answer474 :: Sol
answer474 = sol 474
    ""
    ""
    ""
    problem474

problem474 :: Int
problem474 = 0

----------------------------------------

answer475 :: Sol
answer475 = sol 475
    ""
    ""
    ""
    problem475

problem475 :: Int
problem475 = 0

----------------------------------------

answer476 :: Sol
answer476 = sol 476
    ""
    ""
    ""
    problem476

problem476 :: Int
problem476 = 0

----------------------------------------

answer477 :: Sol
answer477 = sol 477
    ""
    ""
    ""
    problem477

problem477 :: Int
problem477 = 0

----------------------------------------

answer478 :: Sol
answer478 = sol 478
    ""
    ""
    ""
    problem478

problem478 :: Int
problem478 = 0

----------------------------------------

answer479 :: Sol
answer479 = sol 479
    ""
    ""
    ""
    problem479

problem479 :: Int
problem479 = 0

----------------------------------------

answer480 :: Sol
answer480 = sol 480
    ""
    ""
    ""
    problem480

problem480 :: Int
problem480 = 0

----------------------------------------

answer481 :: Sol
answer481 = sol 481
    ""
    ""
    ""
    problem481

problem481 :: Int
problem481 = 0

----------------------------------------

answer482 :: Sol
answer482 = sol 482
    ""
    ""
    ""
    problem482

problem482 :: Int
problem482 = 0

----------------------------------------

answer483 :: Sol
answer483 = sol 483
    ""
    ""
    ""
    problem483

problem483 :: Int
problem483 = 0

----------------------------------------

answer484 :: Sol
answer484 = sol 484
    ""
    ""
    ""
    problem484

problem484 :: Int
problem484 = 0

----------------------------------------

answer485 :: Sol
answer485 = sol 485
    ""
    ""
    ""
    problem485

problem485 :: Int
problem485 = 0

----------------------------------------

answer486 :: Sol
answer486 = sol 486
    ""
    ""
    ""
    problem486

problem486 :: Int
problem486 = 0

----------------------------------------

answer487 :: Sol
answer487 = sol 487
    ""
    ""
    ""
    problem487

problem487 :: Int
problem487 = 0

----------------------------------------

answer488 :: Sol
answer488 = sol 488
    ""
    ""
    ""
    problem488

problem488 :: Int
problem488 = 0

----------------------------------------

answer489 :: Sol
answer489 = sol 489
    ""
    ""
    ""
    problem489

problem489 :: Int
problem489 = 0

----------------------------------------

answer490 :: Sol
answer490 = sol 490
    ""
    ""
    ""
    problem490

problem490 :: Int
problem490 = 0

----------------------------------------

answer491 :: Sol
answer491 = sol 491
    ""
    ""
    ""
    problem491

problem491 :: Int
problem491 = 0

----------------------------------------

answer492 :: Sol
answer492 = sol 492
    ""
    ""
    ""
    problem492

problem492 :: Int
problem492 = 0

----------------------------------------

answer493 :: Sol
answer493 = sol 493
    ""
    ""
    ""
    problem493

problem493 :: Int
problem493 = 0

