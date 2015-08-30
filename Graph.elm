module Graph where

import Dictionary exposing (..)
import Ord exposing (..)

type alias Graph e v = 
    { edges : List e
    , vertices : List v
    , source : Dict e v
    , target : Dict e v
    }

-- Empty graph
emptygraph : Graph (Ord e) v 
emptygraph =
    { edges = []
    , vertices = []
    , source = empty
    , target = empty 
    }

-- Building a graph
addvertex : v -> Graph e v -> Graph e v
addvertex a g = { g | vertices <- a :: g.vertices}  

addedge : (Ord e) -> v -> v -> Graph (Ord e) v -> Graph (Ord e) v
addedge e a b g =
    { g
    | edges  <- e :: g.edges
    , source <- insert e a g.source
    , target <- insert e b g.source
    }
