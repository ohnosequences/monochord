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
    , vetices = []
    , source = Nothing
    , target = Nothing 
    }
