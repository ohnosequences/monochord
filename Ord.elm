module Ord where

{- Ord is an extensible type record for a type whose instances
can be ordered. -}
type alias Ord a = {a | compr : a -> Order}
