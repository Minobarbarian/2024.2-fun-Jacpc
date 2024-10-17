{-# LANGUAGE GADTs, NoImplicitPrelude #-}
module Polymorphism where
    import TypeClasses ()
    import Nat ()
    import List ()

    id :: a -> a
    id x = x