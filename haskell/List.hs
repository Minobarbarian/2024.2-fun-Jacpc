{-# LANGUAGE GADTs, NoImplicitPrelude #-}
module List where

    data List a where
        Nil :: List a
        Cons :: a -> List a -> List a

    (++) :: [a] -> [a] -> [a]
    [] ++ xs = xs
    (x:xs) ++ ys = x:(xs ++ ys)