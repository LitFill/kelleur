module MyLib (printJawaban) where

import System.Process (readProcess)
import Text.Printf (printf)

printJawaban :: (Int, String) -> IO ()
printJawaban (soalKe, jawaban) = do
  hash <- md5s jawaban
  putStrLn $
    printf
      "Jawaban soal %2d: %s\nHash jawaban   : %s\n"
      soalKe
      jawaban
      hash

md5s :: String -> IO String
md5s = readProcess "md5sum" []
