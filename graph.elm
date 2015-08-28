module Graph where

type alias Graph v e = {
    edges    : List e,
    vertices : List v,
    source : e -> v,
    target : e -> v
  }
