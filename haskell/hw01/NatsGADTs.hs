{-# LANGUAGE GADTs, NoImplicitPrelude #-}
module Nat where

    --import qualified Prelude
    data Nat where
        Z :: Nat
        S :: Nat -> Nat
        --deriving (Prelude.Show)

    (+) :: Nat -> Nat -> Nat
    n + Z = n
    n + (S m) = S (n + m)

    (*) :: Nat -> Nat -> Nat
    n * Z = Z
    n * (S m) = n + (n * m)

    (^) :: Nat -> Nat -> Nat
    n ^ Z = S Z
    n ^ (S m) = n * (n ^ m)

    double :: Nat -> Nat
    double n = n * S (S Z)

    pd :: Nat -> Nat
    pd Z = Z
    pd (S n) = n

    fact :: Nat -> Nat
    fact Z = S Z
    fact (S n) = fact n * S n

    fib :: Nat -> Nat
    fib Z = Z
    fib (S Z) = S Z
    fib (S (S n)) = fib (S n) + fib n

    min :: (Nat, Nat) -> Nat
    min (n, Z) = Z
    min (Z, n) = Z
    min (S n, S m) = S (min (n, m))

    max :: (Nat, Nat) -> Nat
    max (n, Z) = n
    max (Z, n) = n
    max (S n, S m) = S (max (n, m))

    --div :: (Nat, Nat) -> (Nat, Nat)

    --quot :: (Nat, Nat) -> Nat
    
    --rem :: (Nat, Nat) -> Nat

    --gcd:: (Nat, Nat) -> Nat

    --lcm:: (Nat, Nat) -> Nat
