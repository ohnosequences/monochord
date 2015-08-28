module MonGraph where

type alias MonGraph v e = 
    { edges : (Int,Int) -> List e
    , vertices : List v
    , source : e -> List v
    , target : e -> List v
    }

-- Empty graph
emptygraph : MonGraph v e 
emptygraph =
    { edges (n,m) = []
    , vertices = []
    , source e = []
    , target e = [] 
    }
