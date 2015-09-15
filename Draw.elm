module Draw where

import Diagrams.Core exposing (..)
import Diagrams.Align exposing (..)
import Diagrams.FillStroke exposing (..)
import Diagrams.FullWindow exposing (..)
import Diagrams.Debug exposing (..)
import Color exposing (..)
import Signal
import Time
import Window

-- drawGraph : Graph -> Diagram t a 
point : Diagram t a
point = circle 0.5 (justSolidFill black)

    

main = fullWindowMain <| ngon 6 100 (justSolidFill black)
