{-# LANGUAGE GADTs, NoImplicitPrelude #-}
module Nat where

    --import qualified Prelude
    data Nat where
        O :: Nat
        S :: Nat -> Nat
        --deriving (Prelude.Show)

    (+) :: Nat -> Nat -> Nat
    n + O = n
    n + (S m) = S (n + m)

    (*) :: Nat -> Nat -> Nat
    n * O = O
    n * (S m) = n + (n * m)

    (^) :: Nat -> Nat -> Nat
    n ^ O = S O
    n ^ (S m) = n * (n ^ m)

    double :: Nat -> Nat
    double n = n * S (S O)

    pd :: Nat -> Nat
    pd O = O
    pd (S n) = n

    fact :: Nat -> Nat
    fact O = S O
    fact (S n) = fact n * S n

    fib :: Nat -> Nat
    fib O = O
    fib (S O) = S O
    fib (S (S n)) = fib (S n) + fib n

    min :: (Nat, Nat) -> Nat
    min (n, O) = O
    min (O, n) = O
    min (S n, S m) = S (min (n, m))

    max :: (Nat, Nat) -> Nat
    max (n, O) = n
    max (O, n) = n
    max (S n, S m) = S (max (n, m))

    --div :: (Nat, Nat) -> (Nat, Nat)

    --quot :: (Nat, Nat) -> Nat
    
    --rem :: (Nat, Nat) -> Nat

    --gcd:: (Nat, Nat) -> Nat

    --lcm:: (Nat, Nat) -> Nat
