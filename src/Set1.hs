{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set1 where

import MCPrelude

fiveRands :: [Integer]
fiveRands = [a,b,c,d,e]
    where s0 = mkSeed 1
          (a, s1) = rand s0
          (b, s2) = rand s1
          (c, s3) = rand s2
          (d, s4) = rand s3
          (e, _)  = rand s4

