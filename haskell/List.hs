{-# LANGUAGE GADTs, NoImplicitPrelude #-}
module List where

    data List α where
        Nil :: List α
        Cons :: α -> List α -> List α