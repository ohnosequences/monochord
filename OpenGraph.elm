module OpenGraph where

import Graph exposing (..)

type OpenGraphType = EdgePoint | Vertex

type alias OpenGraph e v = 
    { graph : Graph e v
    , typing : v -> Typegraph
    }  
