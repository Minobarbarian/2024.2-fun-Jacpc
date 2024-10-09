{-# LANGUAGE GADTs #-}

module Nat where

    import Prelude hiding (Num(..))

    data Nat where
        O :: Nat
        S :: Nat -> Nat
        deriving(Eq, Show)

    plus :: Nat -> Nat -> Nat
    plus n O = n
    plus n (S m) = S (plus n m)

    times :: Nat -> Nat -> Nat
    times n O = O
    times n (S m) = plus n (times n m)

    expon :: Nat -> Nat -> Nat
    expon n O = S O
    expon n (S m) = times n (expon n m)

    (+) :: Nat -> Nat -> Nat
    (+) = plus

    (*) :: Nat -> Nat -> Nat
    (*) = times

    (^) :: Nat -> Nat -> Nat
    (^) = expon


