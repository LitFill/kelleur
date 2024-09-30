module Main where

import MyLib qualified (printJawaban)
import Problems qualified as P

main :: IO ()
main = do
    MyLib.printJawaban P.a5
