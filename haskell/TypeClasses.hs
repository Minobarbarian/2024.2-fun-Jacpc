{-# LANGUAGE GADTs, NoImplicitPrelude #-}
module TypeClasses where
    import Nat
    import Bool (Bool(True, False), not)
    import List ((++))
    import Prelude
        (error, 
        String
        )
    class Show a where
        show :: a -> String

    class Eq a where
        (==) :: a -> a -> Bool
        (/=) :: a -> a -> Bool

        x /= y = not (x == y)

    class Additive a where
        plus :: a -> a -> a
        zero :: a
        negation :: a -> a
        minus :: a -> a -> a
        x `minus` y = x `plus` negation y

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
        negation = neg
        minus = (-)

