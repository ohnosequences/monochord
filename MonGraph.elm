module MonGraph where

import Dictionary exposing (..)

type alias ExplicitMonGraph v e = 
    { edges : (Int,Int) -> List e
    , vertices : List v
    , source : (Int,Int) -> Dict e (List v)
    , target : (Int,Int) -> Dict e (List v)
    }

type MonGraph v e = Empty
                  | Coproduct (MonGraph v e) (MonGraph v e) 
                  | Explicit (ExplicitMonGraph v e)

-- Writes the graph as vertices and edges
explicit : MonGraph v e -> ExplicitMonGraph v e
explicit g = case g of
    Empty         -> emptygraph
    Coproduct a b -> coproduct (explicit a) (explicit b)
    Explicit g    -> g

-- Empty graph
emptygraph : ExplicitMonGraph v e 
emptygraph = 
    { edges (n,m) = []
    , vertices = []
    , source e = empty
    , target e = empty
    }

-- Coproduct
coproduct : ExplicitMonGraph v e -> ExplicitMonGraph v e -> ExplicitMonGraph v e
coproduct a b = 
    { edges (n,m)  = (a.edges (n,m)) ++ (b.edges (n,m))
    , vertices     = a.vertices ++ b.vertices
    , source (n,m) = union (a.source (n,m)) (b.source (n,m))
    , target (n,m) = union (a.source (n,m)) (b.source (n,m))
    }
