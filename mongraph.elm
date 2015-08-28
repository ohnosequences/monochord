module MonGraph where

import Dictionary

type alias MonGraph v e =
    { edges : Dict (Int,Int) (List e)
    , vertices : List v
    , source : Dict e (List v)
    , target : Dict e (List v)
    }

-- Empty graph
emptygraph : MonGraph a e 
emptygraph =
    { edges (n,m) = []
    , vetices = []
    , source e = []
    , target e = [] 
    }
