{-# LANGUAGE GADTs, NoImplicitPrelude #-}
module TypeClasses where
    import Nat
    import Bool (Bool(True, False), not)
    import Prelude
        (Show(..)
        , (++)
        , error
        )
    class Eq a where
        (==) :: a -> a -> Bool
        (/=) :: a -> a -> Bool

        x /= y = not (x == y)
        x == y = not (x /= y)

    class Additive a where
        plus :: a -> a -> a
        zero :: a
        neg :: a -> a
        minus :: a -> a -> a
        x `minus` y = x `plus` (neg y)

    instance Show Nat where
        show O = "0"
        show (S n) = "S" ++ show n
    instance Eq Nat where
        O == O     = True
        S n == S m = n == m
        _ == _     = False
    instance Additive Nat where
        plus = (+)
        zero = O
