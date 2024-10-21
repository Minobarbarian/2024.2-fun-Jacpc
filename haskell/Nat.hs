{-# LANGUAGE GADTs, NoImplicitPrelude #-}
module Nat where

    data Nat where
        O :: Nat
        S :: Nat -> Nat

    (+) :: Nat -> Nat -> Nat
    n + O = n
    n + (S m) = S (n + m)

    (*) :: Nat -> Nat -> Nat
    n * O = O
    n * (S m) = n + (n * m)

    (^) :: Nat -> Nat -> Nat
    n ^ O = S O
    n ^ (S m) = n * (n ^ m)

    (-) :: Nat -> Nat -> Nat
    O - _ = O
    n - O = n
    (S n) - (S m) = n - m

    double :: Nat -> Nat
    double n = n * S (S O)

    pd :: Nat -> Nat
    pd O = O
    pd (S n) = n

    fact :: Nat -> Nat
    fact O = S O
    fact (S n) = fact n * S n

    fib :: Nat -> Nat
    fib (S (S n)) = fib (S n) + fib n
    fib n = n

    neg :: Nat -> Nat
    neg _ = O
    
    min :: (Nat, Nat) -> Nat
    min (S n, S m) = S (min (n, m))
    min (_,_) = O

    max :: (Nat, Nat) -> Nat
    max (n, O) = n
    max (O, n) = n
    max (S n, S m) = S (max (n, m))

    --quot :: (Nat, Nat) -> Nat
    
    --rem :: (Nat, Nat) -> Nat

    --gcd:: (Nat, Nat) -> Nat

    --lcm:: (Nat, Nat) -> Nat
