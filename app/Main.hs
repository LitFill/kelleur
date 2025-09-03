{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultilineStrings #-}

module Main where

import Control.Monad      (forM_)
import Data.Char          (isSpace)
import Data.List          (find)
import System.Environment (getArgs)
import System.Process     (readProcess)
import Text.Printf        (printf)

import Solution

md5s :: String -> IO String
md5s = readProcess "md5sum" []

cek :: Sol -> IO ()
cek s = do
    answerhash <- takeWhile (not . isSpace) <$> md5s s.answer
    printf """
        \x1b[33m====================\x1b[0m
        Problem \x1b[32m%d\x1b[0m: \x1b[1m%s\x1b[0m
           \x1b[3m%s\x1b[0m

        * My Solution  : \x1b[1m%s\x1b[0m
        * My Hash      : \x1b[4m%s\x1b[0m
        * Correct Hash : \x1b[4m%s\x1b[0m
          %s


        """
        s.number
        s.title
        s.desc
        s.answer
        answerhash
        s.hash
        (res answerhash)
  where
    res ah =
        if ah == s.hash
        then "\x1b[32mYeay! I am correct 🎉\x1b[0m"
        else "\x1b[31mnooooooo Wrong 😢\x1b[0m"

cekN :: Int -> IO ()
cekN n = case find go solutions of
    Just s  -> cek s
    Nothing -> printf "Problem %d is not solved yet.\n" n
  where
    go s = s.number == n && not (null s.desc)

handleArgs :: [String] -> IO ()
handleArgs = \case
    [n] | num <- read n
        , num > 0 , num < 494
        -> cekN num
    [n, m]
        | n' <- read n
        , n' > 0 , n' < 494
        , m' <- read m
        , m' > 0 , m' < 494
        -> forM_ [n'..m'] cekN
    _   -> putStrLn
        """
        \x1b[1;33mKelleur\x1b[0m - \x1b[3mcli program for verifying the solutions to the Project Euler puzzles.\x1b[0m

        \x1b[1musage: kelleur N [M]\x1b[0m
          where
            \x1b[32mN\x1b[0m     is problem number N (\x1b[4m1 ≤ N ≤ 493\x1b[0m)
            \x1b[32mM\x1b[0m     is like N but optional
        """

main :: IO ()
main = getArgs >>= handleArgs
