module Graph where

type alias Graph v e = 
    { edges : List e
    , vertices : List v
    , source : e -> Maybe v
    , target : e -> Maybe v
    }

-- Empty graph
emptygraph : Graph v e 
emptygraph =
    { edges = []
    , vertices = []
    , source e = Nothing
    , target e = Nothing 
    }
