module ExCurry where

import Prelude hiding ( curry , uncurry )

-- use your mind to infer the types, don't cheat!

-- curry gets a "traditional" binary function
-- and returns its currified version
curry :: ((a, b) -> c) -> a -> b -> c
curry f a b = f (a, b) 

-- uncurry gets a currified function
-- and returns its "traditional" binary version
uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry f (a, b) = f a b