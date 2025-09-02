{-# LANGUAGE TemplateHaskell #-}

module TH where

import Control.Monad (forM)

import Language.Haskell.TH

infixl 6 ++@
(++@) :: Show a => String -> a -> String
nm ++@ i = nm ++ show i

cekN :: Int -> Q Exp
cekN n = [| cek $(varE (mkName ("answer" ++@ n))) |]

-- >>> pprint <$> runQ (cekN 5)
-- "cek answer5"

genCeks :: Int -> Q [Dec]
genCeks n = forM [1..n] mkCekDec
  where
    mkCekDec ix = funD
        (mkName $ "cek" ++@ ix)
        [clause [] (normalB (cekN ix)) []]

genCeksTySig :: Int -> Q [Dec]
genCeksTySig n = forM [1..n] go
  where
    go ix = sigD
        (mkName $ "cek"++@ix)
        (appT (conT (mkName "IO")) (conT (mkName "()")))

genCekN :: Int -> Q [Dec]
genCekN n = do
    tySigs <- genCeksTySig n
    decs <- genCeks n
    pure $ tySigs ++ decs

genAnswers :: Int -> Q [Dec]
genAnswers n = forM [1..n] go
  where
    go ix = funD
        (mkName $ "answer"++@ix)
        [clause [] (normalB (varE (mkName "mempty"))) []]

genAnswersFT :: Int -> Int -> Q [Dec]
genAnswersFT fr to = forM [fr..to] go
  where
    go ix = funD
        (mkName $ "answer"++@ix)
        [clause [] (normalB (varE (mkName "mempty"))) []]

genAnswerTySig :: Int -> Q [Dec]
genAnswerTySig n = forM [1..n] go
  where
    go ix = sigD
        (mkName $ "answer"++@ix)
        (conT (mkName "Sol"))

genAnswerTySigFT :: Int -> Int -> Q [Dec]
genAnswerTySigFT fr to = forM [fr .. to] go
  where
    go ix = sigD
        (mkName $ "answer"++@ix)
        (conT (mkName "Sol"))

genAnswerN :: Int -> Q [Dec]
genAnswerN n = do
    tySigs <- genAnswerTySig n
    decs   <- genAnswers n
    pure $ tySigs ++ decs

genAnswerFT :: Int -> Int -> Q [Dec]
genAnswerFT fr to = do
    tySigs <- genAnswerTySigFT fr to
    decs   <- genAnswersFT fr to
    pure $ tySigs ++ decs

-- >>> pprint <$> runQ (genCeks 10)
-- "cek1 = cek answer1\ncek2 = cek answer2\ncek3 = cek answer3\ncek4 = cek answer4\ncek5 = cek answer5\ncek6 = cek answer6\ncek7 = cek answer7\ncek8 = cek answer8\ncek9 = cek answer9\ncek10 = cek answer10"

-- >>> pprint <$> runQ (genCeksTySig 2)
-- "cek1 :: IO GHC.Tuple.()\ncek2 :: IO GHC.Tuple.()"

-- >>> pprint <$> runQ (genAnswers 3)
-- "answer1 = mempty\nanswer2 = mempty\nanswer3 = mempty"

-- >>> pprint <$> runQ (genAnswerTySig 3)
-- "answer1 :: Sol\nanswer2 :: Sol\nanswer3 :: Sol"

-- >>> pprint <$> runQ (genAnswersFT 50 100)
-- "answer50 = mempty\nanswer51 = mempty\nanswer52 = mempty\nanswer53 = mempty\nanswer54 = mempty\nanswer55 = mempty\nanswer56 = mempty\nanswer57 = mempty\nanswer58 = mempty\nanswer59 = mempty\nanswer60 = mempty\nanswer61 = mempty\nanswer62 = mempty\nanswer63 = mempty\nanswer64 = mempty\nanswer65 = mempty\nanswer66 = mempty\nanswer67 = mempty\nanswer68 = mempty\nanswer69 = mempty\nanswer70 = mempty\nanswer71 = mempty\nanswer72 = mempty\nanswer73 = mempty\nanswer74 = mempty\nanswer75 = mempty\nanswer76 = mempty\nanswer77 = mempty\nanswer78 = mempty\nanswer79 = mempty\nanswer80 = mempty\nanswer81 = mempty\nanswer82 = mempty\nanswer83 = mempty\nanswer84 = mempty\nanswer85 = mempty\nanswer86 = mempty\nanswer87 = mempty\nanswer88 = mempty\nanswer89 = mempty\nanswer90 = mempty\nanswer91 = mempty\nanswer92 = mempty\nanswer93 = mempty\nanswer94 = mempty\nanswer95 = mempty\nanswer96 = mempty\nanswer97 = mempty\nanswer98 = mempty\nanswer99 = mempty\nanswer100 = mempty"

genSols :: Int -> Q [Dec]
genSols n = undefined
