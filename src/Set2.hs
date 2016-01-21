{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax  #-}

module Set1 where

import MCPrelude

data Maybe a = Nothing | Just a

instance Show a => Show (Maybe a) where
    show Nothing = "Nothing"
    show (Just x) = "Just " ++ show x

headMay :: [a] -> Maybe a
headMay [] = Nothing
headMay (x:_) = Just x

tailMay :: [a] -> Maybe [a]
tailMay [] = Nothing
tailMay (_:xs) = Just xs

lookupMay :: Eq a => a -> [(a, b)] -> Maybe b
lookupMay _ [] = Nothing
lookupMay x ((y,z):zs) | x == y = Just z
                       | otherwise = lookupMay x zs

divMay :: (Eq a, Fractional a) => a -> a -> Maybe a
divMay x y | y == 0 = Nothing
divMay x 0 = Nothing
divMay x y = Just (x / y)

maximumMay :: Ord a => [a] -> Maybe a
maximumMay [] = Nothing
maximumMay xs = Just (foldr1 max xs)

minimumMay :: Ord a => [a] -> Maybe a
minimumMay [] = Nothing
minimumMay xs = Just (foldr1 min xs)

queryGreek :: GreekData -> String -> Maybe Double
queryGreek g s = case lookupMay s g of
                     Nothing -> Nothing
                     Just xs -> case tailMay xs of
                                    Nothing -> Nothing
                                    Just t  -> case headMay xs of
                                                   Nothing -> Nothing
                                                   Just h  -> case maximumMay t of
                                                                  Nothing -> Nothing
                                                                  Just m  -> case divMay (fromIntegral m) (fromIntegral h) of
                                                                                 Nothing -> Nothing
                                                                                 Just d  -> Just d

