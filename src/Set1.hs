{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set1 where

import MCPrelude

type Gen a = Seed -> (a, Seed)

fiveRands :: [Integer]
fiveRands = [a,b,c,d,e]
    where s0 = mkSeed 1
          (a, s1) = rand s0
          (b, s2) = rand s1
          (c, s3) = rand s2
          (d, s4) = rand s3
          (e, _)  = rand s4

randLetter :: Gen Char
randLetter s = (c, s')
    where (i, s') = rand s
          c = toLetter i

randString3 :: String
randString3 = [a,b,c]
    where s0 = mkSeed 1
          (a, s1) = randLetter s0
          (b, s2) = randLetter s1
          (c, s3) = randLetter s2

generalA :: (Integer -> Integer) -> Gen Integer -> Gen Integer
generalA f g s = (f i, s')
    where (i, s') = g s

randEven :: Gen Integer
randEven = generalA (*2) rand

randOdd :: Gen Integer
randOdd = generalA (+1) randEven

randTen :: Gen Integer
randTen = generalA (*10) rand

randPair :: Gen (Char, Integer)
randPair s = ((l, i), s2)
    where (l, s1) = randLetter s
          (i, s2) = rand s1

generalB :: (a -> b -> c) -> Gen a -> Gen b -> Gen c
generalB f g1 g2 s = (f x y, s2)
    where (x, s1) = g1 s
          (y, s2) = g2 s1

generalPair :: Gen a -> Gen b -> Gen (a, b)
generalPair g1 g2 s = ((x, y), s2)
    where (x, s1) = g1 s
          (y, s2) = g2 s1

generalPair2 :: Gen a -> Gen b -> Gen (a, b)
generalPair2 = generalB (,)

