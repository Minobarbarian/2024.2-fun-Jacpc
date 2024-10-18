{-# LANGUAGE GADTs, NoImplicitPrelude #-}
module Functions where
    import Bool
    import List
    import Maybe
    import Nat

    ifthenelse :: Bool -> a -> a -> a
    ifthenelse True x _ = x
    ifthenelse False _ y = y

    