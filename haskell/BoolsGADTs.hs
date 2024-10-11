{-# LANGUAGE GADTs, NoImplicitPrelude #-}
module Bool where

    data Bool where
        True :: Bool
        False :: Bool

    ifthenelse :: Bool -> a -> a -> a
    ifthenelse True x _ = x
    ifthenelse False _ y = y

    (&&) :: Bool -> Bool -> Bool
    b && True = b
    b && False = False

    (||) :: Bool -> Bool -> Bool
    False || False = False
    b || c = True

    (¬) :: Bool -> Bool
    (¬) True = False
    (¬) False = True