module MonGraph where

type alias MonGraph v e = {
    edges : (Int,Int) -> List e,
    vertices : List v,
    source : e -> List v,
    target : e -> List v
  }

-- Empty graph
emptygraph : Graph v e 
emptygraph =
    { edges = []
    , vetices = []
    , source e = []
    , target e = [] 
    }
