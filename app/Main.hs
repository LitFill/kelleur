{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultilineStrings #-}

module Main where

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
cekN n = case find (\s -> s.number == n) solutions of
    Just s -> cek s
    Nothing -> putStrLn $ "Problem " ++ show n ++ " is not solved yet."

handleArgs :: [String] -> IO ()
handleArgs = \case
    [n] | num <- read @Int n
        , num > 0 && num < 494
        -> cekN num
    _   -> putStrLn "usage: <program> N\n    where N is problem number N (1 ≤ N ≤ 493)"

main :: IO ()
main = getArgs >>= handleArgs
