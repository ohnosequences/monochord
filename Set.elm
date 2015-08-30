module Set
    ( Set
    , empty, singleton, insert, remove
    , isEmpty, member, size
    , foldl, foldr, map
    , filter, partition
    , union, intersect, diff
    , toList, fromList
    ) where

{-| A set of unique values. The values can be any (Ord c) type. This
includes `Int`, `Float`, `Time`, `Char`, `String`, and tuples or lists
of (Ord c) types.
Insert, remove, and query operations all take *O(log n)* time. Set equality with
`(==)` is unreliable and should not be used.
# Sets
@docs Set
# Build
@docs empty, singleton, insert, remove
# Query
@docs isEmpty, member, size
# Combine
@docs union, intersect, diff
# Lists
@docs toList, fromList
# Transform
@docs map, foldl, foldr, filter, partition
-}

import Basics exposing ((<|))
import Dictionary as Dict
import List as List
import Ord exposing (..)

{-| Represents a set of unique values. So `(Set Int)` is a set of integers and
`(Set String)` is a set of strings.
-}
type Set t =
  Set_elm_builtin (Dict.Dict t ())


{-| Create an empty set.
-}
empty : Set (Ord c)
empty =
  Set_elm_builtin Dict.empty


{-| Create a set with one value.
-}
singleton : (Ord c) -> Set (Ord c)
singleton k =
  Set_elm_builtin <| Dict.singleton k ()


{-| Insert a value into a set.
-}
insert : (Ord c) -> Set (Ord c) -> Set (Ord c)
insert k (Set_elm_builtin d) =
  Set_elm_builtin <| Dict.insert k () d


{-| Remove a value from a set. If the value is not found, no changes are made.
-}
remove : (Ord c) -> Set (Ord c) -> Set (Ord c)
remove k (Set_elm_builtin d) =
  Set_elm_builtin <| Dict.remove k d


{-| Determine if a set is empty.
-}
isEmpty : Set (Ord c) -> Bool
isEmpty (Set_elm_builtin d) =
  Dict.isEmpty d


{-| Determine if a value is in a set.
-}
member : (Ord c) -> Set (Ord c) -> Bool
member k (Set_elm_builtin d) =
  Dict.member k d


{-| Determine the number of elements in a set.
-}
size : Set (Ord c) -> Int
size (Set_elm_builtin d) =
  Dict.size d


{-| Get the union of two sets. Keep all values.
-}
union : Set (Ord c) -> Set (Ord c) -> Set (Ord c)
union (Set_elm_builtin d1) (Set_elm_builtin d2) =
  Set_elm_builtin <| Dict.union d1 d2


{-| Get the intersection of two sets. Keeps values that appear in both sets.
-}
intersect : Set (Ord c) -> Set (Ord c) -> Set (Ord c)
intersect (Set_elm_builtin d1) (Set_elm_builtin d2) =
  Set_elm_builtin <| Dict.intersect d1 d2


{-| Get the difference between the first set and the second. Keeps values
that do not appear in the second set.
-}
diff : Set (Ord c) -> Set (Ord c) -> Set (Ord c)
diff (Set_elm_builtin d1) (Set_elm_builtin d2) =
  Set_elm_builtin <| Dict.diff d1 d2


{-| Convert a set into a list.
-}
toList : Set (Ord c) -> List (Ord c)
toList (Set_elm_builtin d) =
  Dict.keys d


{-| Convert a list into a set, removing any duplicates.
-}
fromList : List (Ord c) -> Set (Ord c)
fromList xs = List.foldl insert empty xs


{-| Fold over the values in a set, in order from lowest to highest.
-}
foldl : ((Ord c) -> b -> b) -> b -> Set (Ord c) -> b
foldl f b (Set_elm_builtin d) =
  Dict.foldl (\k _ b -> f k b) b d


{-| Fold over the values in a set, in order from highest to lowest.
-}
foldr : ((Ord c) -> b -> b) -> b -> Set (Ord c) -> b
foldr f b (Set_elm_builtin d) =
  Dict.foldr (\k _ b -> f k b) b d


{-| Map a function onto a set, creating a new set with no duplicates.
-}
map : ((Ord c) -> (Ord d)) -> Set (Ord c) -> Set (Ord d)
map f s = fromList (List.map f (toList s))


{-| Create a new set consisting only of elements which satisfy a predicate.
-}
filter : ((Ord c) -> Bool) -> Set (Ord c) -> Set (Ord c)
filter p (Set_elm_builtin d) =
  Set_elm_builtin <| Dict.filter (\k _ -> p k) d


{-| Create two new sets; the first consisting of elements which satisfy a
predicate, the second consisting of elements which do not.
-}
partition : ((Ord c) -> Bool) -> Set (Ord c) -> (Set (Ord c), Set (Ord c))
partition p (Set_elm_builtin d) =
  let
    (p1, p2) = Dict.partition (\k _ -> p k) d
  in
    (Set_elm_builtin p1, Set_elm_builtin p2)
