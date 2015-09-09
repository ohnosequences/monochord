module Ord where

{- Ord a defines an order on an arbitrary type a. -}
type alias Ord a = (a -> a -> Order)
