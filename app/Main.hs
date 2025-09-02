{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.List          (find)
import System.Environment (getArgs)
import System.Process     (readProcess)

import Solution
import TH

md5s :: String -> IO String
md5s = readProcess "md5sum" []

cekN :: Int -> IO ()
cekN n = case find (\s -> s.number == n) solutions of
    Just s -> cek s
    Nothing -> putStrLn $ "Problem " ++ show n ++ " is not solved yet."

handleArgs :: [String] -> IO ()
handleArgs = \case
    [n] | num <- read @Int n
        , num > 0 && num < 494
        -- -> cekN num
        -> $(TH.cekN num)
    _   -> putStrLn "usage: <program> N\n    where N is problem number N (1 ≤ N ≤ 493)"

main :: IO ()
main = getArgs >>= handleArgs
