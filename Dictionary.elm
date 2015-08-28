module Dictionary where

import Basics exposing (..)
import Maybe exposing (..)
import List exposing (..)
import Debug
import String

{- Ord is an extensible type record for a type whose instances
can be ordered. -}
type alias Ord a = {a | compr : a -> Order}

-- BBlack and NBlack should only be used during the deletion
-- algorithm. Any other occurrence is a bug and should fail an assert.
type NColor
    = Red
    | Black
    | BBlack  -- Double Black, counts as 2 blacks for the invariant
    | NBlack  -- Negative Black, counts as -1 blacks for the invariant


showNColor : NColor -> String
showNColor c =
  case c of
    Red    -> "Red"
    Black  -> "Black"
    BBlack -> "BBlack"
    NBlack -> "NBlack"


type LeafColor
    = LBlack
    | LBBlack -- Double Black, counts as 2


showLColor : LeafColor -> String
showLColor color =
    case color of
      LBlack  -> "LBlack"
      LBBlack -> "LBBlack"


{-| A dictionary of keys and values. So a `(Dict String User)` is a dictionary
that lets you look up a `String` (such as user names) and find the assotiated
`User`.
-}
type Dict k v
    = RBNode_elm_builtin NColor k v (Dict k v) (Dict k v)
    | RBEmpty_elm_builtin LeafColor
    
    
{-| Create an empty dictionary. -}
empty : Dict (Ord c) v
empty = RBEmpty_elm_builtin LBlack

maxWithDefault : (Ord k) -> v -> Dict (Ord k) v -> ((Ord k), v)
maxWithDefault k v r =
    case r of
      RBEmpty_elm_builtin _ ->
          (k, v)

      RBNode_elm_builtin _ kr vr _ rr ->
          maxWithDefault kr vr rr


-- {-| Get the value associated with a key. If the key is not found, return
-- `Nothing`. This is useful when you are not sure if a key will be in the
-- dictionary.
--     animals = fromList [ ("Tom", Cat), ("Jerry", Mouse) ]
--     get "Tom"   animals == Just Cat
--     get "Jerry" animals == Just Mouse
--     get "Spike" animals == Nothing
-- -}
get : Ord c -> Dict (Ord c) v -> Maybe v
get targetKey dict =
    case dict of
      RBEmpty_elm_builtin LBlack ->
          Nothing

      RBNode_elm_builtin _ key value left right ->
           case targetKey.compr {key-compr} of
             LT -> get targetKey left
             EQ -> Just value
             GT -> get targetKey right


{-| Determine if a key is in a dictionary. -}
member : (Ord c) -> Dict (Ord c) v -> Bool
member key dict =
    case get key dict of
      Just _ -> True
      Nothing -> False


{-| Determine the number of key-value pairs in the dictionary. -}
size : Dict (Ord c) v -> Int
size dict =
  case dict of
    RBEmpty_elm_builtin _ ->
      0

    RBNode_elm_builtin _ _ _ left right ->
      1 + size left + size right


{-| Determine if a dictionary is empty.
    isEmpty empty == True
-}
isEmpty : Dict (Ord c) v -> Bool
isEmpty dict =
    dict == empty


ensureBlackRoot : Dict (Ord k) v -> Dict (Ord k) v
ensureBlackRoot dict =
    case dict of
      RBNode_elm_builtin Red key value left right ->
          RBNode_elm_builtin Black key value left right

      RBNode_elm_builtin Black _ _ _ _ ->
          dict

      RBEmpty_elm_builtin LBlack ->
          dict


{-| Insert a key-value pair into a dictionary. Replaces value when there is
a collision. -}
insert : (Ord c) -> v -> Dict (Ord c) v -> Dict (Ord c) v
insert key value dict =
    update key (always (Just value)) dict


{-| Remove a key-value pair from a dictionary. If the key is not found,
no changes are made. -}
remove : (Ord c) -> Dict (Ord c) v -> Dict (Ord c) v
remove key dict =
    update key (always Nothing) dict


type Flag = Insert | Remove | Same

showFlag : Flag -> String
showFlag f = case f of
  Insert -> "Insert"
  Remove -> "Remove"
  Same   -> "Same"


{-| Update the value of a dictionary for a specific key with a given function. -}
update : (Ord c) -> (Maybe v -> Maybe v) -> Dict (Ord c) v -> Dict (Ord c) v
update k alter dict =
  let up dict =
          case dict of
            RBEmpty_elm_builtin LBlack ->
                case alter Nothing of
                  Nothing -> (Same, empty)
                  Just v  -> (Insert, RBNode_elm_builtin Red k v empty empty)

            RBNode_elm_builtin clr key value left right ->
                case k.compr {key-compr} of
                  EQ ->
                    case alter (Just value) of
                      Nothing -> (Remove, rem clr left right)
                      Just newValue ->
                          (Same, RBNode_elm_builtin clr key newValue left right)

                  LT ->
                    let (flag, newLeft) = up left in
                    case flag of
                      Same   -> (Same, RBNode_elm_builtin clr key value newLeft right)
                      Insert -> (Insert, balance clr key value newLeft right)
                      Remove -> (Remove, bubble clr key value newLeft right)

                  GT ->
                    let (flag, newRight) = up right in
                    case flag of
                      Same   -> (Same, RBNode_elm_builtin clr key value left newRight)
                      Insert -> (Insert, balance clr key value left newRight)
                      Remove -> (Remove, bubble clr key value left newRight)

      (flag, updatedDict) = up dict
  in
      case flag of
        Same   -> updatedDict
        Insert -> ensureBlackRoot updatedDict
        Remove -> blacken updatedDict


{-| Create a dictionary with one key-value pair. -}
singleton : (Ord c) -> v -> Dict (Ord c) v
singleton key value =
    insert key value empty


isBBlack : Dict (Ord k) v -> Bool
isBBlack dict =
    case dict of
      RBNode_elm_builtin BBlack _ _ _ _ -> True
      RBEmpty_elm_builtin LBBlack -> True
      _ -> False


moreBlack : NColor -> NColor
moreBlack color =
    case color of
      Black  -> BBlack
      Red    -> Black
      NBlack -> Red
      BBlack -> Debug.crash "Can't make a double black node more black!"


lessBlack : NColor -> NColor
lessBlack color =
    case color of
      BBlack -> Black
      Black  -> Red
      Red    -> NBlack
      NBlack -> Debug.crash "Can't make a negative black node less black!"


lessBlackTree : Dict (Ord k) v -> Dict (Ord k) v
lessBlackTree dict =
    case dict of
      RBNode_elm_builtin c k v l r -> RBNode_elm_builtin (lessBlack c) k v l r
      RBEmpty_elm_builtin LBBlack -> RBEmpty_elm_builtin LBlack


reportRemBug : String -> NColor -> String -> String -> a
reportRemBug msg c lgot rgot =
  Debug.crash <|
    String.concat
    [ "Internal red-black tree invariant violated, expected "
    , msg, " and got ", showNColor c, "/", lgot, "/", rgot
    , "\nPlease report this bug to <https://github.com/elm-lang/Elm/issues>"
    ]


-- Remove the top node from the tree, may leave behind BBlacks
rem : NColor -> Dict (Ord k) v -> Dict (Ord k) v -> Dict (Ord k) v
rem c l r =
    case (l, r) of
      (RBEmpty_elm_builtin _, RBEmpty_elm_builtin _) ->
          case c of
            Red   -> RBEmpty_elm_builtin LBlack
            Black -> RBEmpty_elm_builtin LBBlack

      (RBEmpty_elm_builtin cl, RBNode_elm_builtin cr k' v' l' r') ->
          case (c, cl, cr) of
            (Black, LBlack, Red) ->
                RBNode_elm_builtin Black k' v' l' r'

            _ ->
                reportRemBug "Black/LBlack/Red" c (showLColor cl) (showNColor cr)

      (RBNode_elm_builtin cl k' v' l' r', RBEmpty_elm_builtin cr) ->
          case (c, cl, cr) of
            (Black, Red, LBlack) ->
                RBNode_elm_builtin Black k' v' l' r'

            _ ->
                reportRemBug "Black/Red/LBlack" c (showNColor cl) (showLColor cr)

      -- l and r are both RBNodes
      (RBNode_elm_builtin cl kl vl ll rl, RBNode_elm_builtin _ _ _ _ _) ->
          let (k, v) = maxWithDefault kl vl rl
              l'     = remove_max cl kl vl ll rl
          in
              bubble c k v l' r


-- Kills a BBlack or moves it upward, may leave behind NBlack
bubble : NColor -> (Ord k) -> v -> Dict (Ord k) v -> Dict (Ord k) v -> Dict (Ord k) v
bubble c k v l r =
    if isBBlack l || isBBlack r
        then balance (moreBlack c) k v (lessBlackTree l) (lessBlackTree r)
        else RBNode_elm_builtin c k v l r


-- Removes rightmost node, may leave root as BBlack
remove_max : NColor -> (Ord k) -> v -> Dict (Ord k) v -> Dict (Ord k) v -> Dict (Ord k) v
remove_max c k v l r =
    case r of
      RBEmpty_elm_builtin _ ->
          rem c l r

      RBNode_elm_builtin cr kr vr lr rr ->
          bubble c k v l (remove_max cr kr vr lr rr)


-- generalized tree balancing act
balance : NColor -> (Ord k) -> v -> Dict (Ord k) v -> Dict (Ord k) v -> Dict (Ord k) v
balance c k v l r =
    balance_node (RBNode_elm_builtin c k v l r)


blackish : Dict (Ord k) v -> Bool
blackish t =
    case t of
      RBNode_elm_builtin c _ _ _ _ -> c == Black || c == BBlack
      RBEmpty_elm_builtin _        -> True


balance_node : Dict (Ord k) v -> Dict (Ord k) v
balance_node t =
  let assemble col xk xv yk yv zk zv a b c d =
        RBNode_elm_builtin (lessBlack col) yk yv (RBNode_elm_builtin Black xk xv a b) (RBNode_elm_builtin Black zk zv c d)
  in
   if blackish t
   then case t of
     RBNode_elm_builtin col zk zv (RBNode_elm_builtin Red yk yv (RBNode_elm_builtin Red xk xv a b) c) d ->
       assemble col xk xv yk yv zk zv a b c d
     RBNode_elm_builtin col zk zv (RBNode_elm_builtin Red xk xv a (RBNode_elm_builtin Red yk yv b c)) d ->
       assemble col xk xv yk yv zk zv a b c d
     RBNode_elm_builtin col xk xv a (RBNode_elm_builtin Red zk zv (RBNode_elm_builtin Red yk yv b c) d) ->
       assemble col xk xv yk yv zk zv a b c d
     RBNode_elm_builtin col xk xv a (RBNode_elm_builtin Red yk yv b (RBNode_elm_builtin Red zk zv c d)) ->
       assemble col xk xv yk yv zk zv a b c d

     RBNode_elm_builtin BBlack xk xv a (RBNode_elm_builtin NBlack zk zv (RBNode_elm_builtin Black yk yv b c) d) ->
       case d of
         (RBNode_elm_builtin Black _ _ _ _) ->
           RBNode_elm_builtin Black yk yv (RBNode_elm_builtin Black xk xv a b) (balance Black zk zv c (redden d))
         _ -> t

     RBNode_elm_builtin BBlack zk zv (RBNode_elm_builtin NBlack xk xv a (RBNode_elm_builtin Black yk yv b c)) d ->
       case a of
         (RBNode_elm_builtin Black _ _ _ _) ->
           RBNode_elm_builtin Black yk yv (balance Black xk xv (redden a) b) (RBNode_elm_builtin Black zk zv c d)
         _ -> t
     _ -> t
   else t


-- make the top node black
blacken : Dict (Ord k) v -> Dict (Ord k) v
blacken t =
    case t of
      RBEmpty_elm_builtin _ -> RBEmpty_elm_builtin LBlack
      RBNode_elm_builtin _ k v l r -> RBNode_elm_builtin Black k v l r


-- make the top node red
redden : Dict (Ord k) v -> Dict (Ord k) v
redden t =
    case t of
      RBEmpty_elm_builtin _ -> Debug.crash "can't make a Leaf red"
      RBNode_elm_builtin _ k v l r -> RBNode_elm_builtin Red k v l r
-- 
-- 
-- {-| Apply a function to all values in a dictionary. -}
-- map : ((Ord c) -> a -> b) -> Dict (Ord c) a -> Dict (Ord c) b
-- map f dict =
--     case dict of
--       RBEmpty_elm_builtin LBlack ->
--           RBEmpty_elm_builtin LBlack
-- 
--       RBNode_elm_builtin clr key value left right ->
--           RBNode_elm_builtin clr key (f key value) (map f left) (map f right)
-- 
-- 
-- {-| Fold over the key-value pairs in a dictionary, in order from lowest
-- key to highest key. -}
-- foldl : ((Ord c) -> v -> b -> b) -> b -> Dict (Ord c) v -> b
-- foldl f acc dict =
--     case dict of
--       RBEmpty_elm_builtin LBlack -> acc
-- 
--       RBNode_elm_builtin _ key value left right ->
--           foldl f (f key value (foldl f acc left)) right
-- 
-- 
-- {-| Fold over the key-value pairs in a dictionary, in order from highest
-- key to lowest key. -}
-- foldr : ((Ord c) -> v -> b -> b) -> b -> Dict (Ord c) v -> b
-- foldr f acc t =
--     case t of
--       RBEmpty_elm_builtin LBlack -> acc
-- 
--       RBNode_elm_builtin _ key value left right ->
--           foldr f (f key value (foldr f acc right)) left
-- 
-- 
-- {-| Combine two dictionaries. If there is a collision, preference is given
-- to the first dictionary. -}
-- union : Dict (Ord c) v -> Dict (Ord c) v -> Dict (Ord c) v
-- union t1 t2 =
--     foldl insert t2 t1
-- 
-- 
-- {-| Keep a key-value pair when its key appears in the second dictionary.
-- Preference is given to values in the first dictionary. -}
-- intersect : Dict (Ord c) v -> Dict (Ord c) v -> Dict (Ord c) v
-- intersect t1 t2 =
--     filter (\k _ -> k `member` t2) t1
-- 
-- 
-- {-| Keep a key-value pair when its key does not appear in the second dictionary.
-- -}
-- diff : Dict (Ord c) v -> Dict (Ord c) v -> Dict (Ord c) v
-- diff t1 t2 =
--     foldl (\k v t -> remove k t) t1 t2
-- 
-- 
-- {-| Get all of the keys in a dictionary. -}
-- keys : Dict (Ord c) v -> List (Ord c)
-- keys dict =
--     foldr (\key value keyList -> key :: keyList) [] dict
-- 
-- 
-- {-| Get all of the values in a dictionary. -}
-- values : Dict (Ord c) v -> List v
-- values dict =
--     foldr (\key value valueList -> value :: valueList) [] dict
-- 
-- 
-- {-| Convert a dictionary into an association list of key-value pairs. -}
-- toList : Dict (Ord c) v -> List ((Ord c),v)
-- toList dict =
--     foldr (\key value list -> (key,value) :: list) [] dict
-- 
-- 
-- {-| Convert an association list into a dictionary. -}
-- fromList : List ((Ord c),v) -> Dict (Ord c) v
-- fromList assocs =
--     List.foldl (\(key,value) dict -> insert key value dict) empty assocs
-- 
-- 
-- {-| Keep a key-value pair when it satisfies a predicate. -}
-- filter : ((Ord c) -> v -> Bool) -> Dict (Ord c) v -> Dict (Ord c) v
-- filter predicate dictionary =
--     let add key value dict =
--             if predicate key value
--                 then insert key value dict
--                 else dict
--     in
--         foldl add empty dictionary
-- 
-- 
-- {-| Partition a dictionary according to a predicate. The first dictionary
-- contains all key-value pairs which satisfy the predicate, and the second
-- contains the rest.
-- -}
-- partition : ((Ord c) -> v -> Bool) -> Dict (Ord c) v -> (Dict (Ord c) v, Dict (Ord c) v)
-- partition predicate dict =
--     let add key value (t1, t2) =
--             if predicate key value
--                 then (insert key value t1, t2)
--                 else (t1, insert key value t2)
