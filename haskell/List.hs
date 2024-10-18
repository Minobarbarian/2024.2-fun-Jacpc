{-# LANGUAGE GADTs, NoImplicitPrelude #-}
module List where

    data List a where
        Nil :: List a
        Cons :: a -> List a -> List a

    (++) :: List a -> List a -> List a
    Nil ++ xs = xs
    (Cons x xs) ++ ys = Cons x (xs ++ ys)