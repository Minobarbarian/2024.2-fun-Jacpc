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
