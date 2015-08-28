module Graph where

type alias Graph v e = {
    edges    : List e,
    vertices : List v,
    source : e -> v,
    target : e -> v
  }


-- Example: complete graphs
complete : Int -> Graph Int (Int,Int) 
complete n = {
    edges = List.concatMap (\f -> List.map f [1..n]) (List.map (,) [1..n]),
    vertices = [1..n],
    source = fst,
    target = snd
  }

-- Example: the type graph for Open Graphs
type TypeGraphVertices = V | E
type TypeGraphEdges    = VtoE | EtoV | EtoE

typegraph : Graph TypeGraphVertices TypeGraphEdges
typegraph = {
    edges = [VtoE,EtoV,EtoE],
    vertices = [V,E],
    source ed = case ed of
                    VtoE -> V
                    EtoV -> E
                    EtoE -> E,                
    target ed = case ed of
                    VtoE -> E
                    EtoV -> V
                    EtoE -> E
  }
