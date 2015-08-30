module Graph where

import Dictionary exposing (..)

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
addvertex : v -> Graph (Ord e) v -> Graph (Ord e) v
addvertex a g = { g | vertices <- a :: g.vertices}  
