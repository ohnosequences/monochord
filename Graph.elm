module Graph where

import Dictionary exposing (..)

type alias Graph a b = 
    { edges : List a
    , vertices : List b
    , source : Dict a b
    , target : Dict a b
    }

-- Empty graph
emptygraph : Graph (Ord (a)) b 
emptygraph =
    { edges = []
    , vertices = []
    , source = empty
    , target = empty 
    }
