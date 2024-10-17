{-# LANGUAGE GADTs, NoImplicitPrelude #-}
module Maybe where

    data Maybe α where
        Nothing :: Maybe α
        Just :: α -> Maybe α