module MonGraph where

type alias MonGraph v e = 
    { edges : (Int,Int) -> List e
    , vertices : List v
    , source : (Int,Int) -> Dict e (List v)
    , target : (Int,Int) -> Dict e (List v)
    }

-- Empty graph
emptygraph : MonGraph v e 
emptygraph =
    { edges (n,m) = []
    , vertices = []
    , source e = []
    , target e = [] 
    }


asdf : Graph a -> qewr
asdf g = case g of
            {a | coprod:}
