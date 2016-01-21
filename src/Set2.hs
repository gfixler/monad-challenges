{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set1 where

import MCPrelude

data Maybe a = Nothing | Just a

instance Show a => Show (Maybe a) where
    show Nothing = "Nothing"
    show (Just x) = "Just " ++ show x

