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

