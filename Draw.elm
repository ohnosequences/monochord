module Draw where

import Diagrams.Core exposing (..)
import Diagrams.Align exposing (..)
import Diagrams.Pad exposing (..)
import Diagrams.Geom exposing (..)
import Diagrams.Bezier exposing (..)
import Diagrams.FillStroke exposing (..)
import Diagrams.Actions exposing (..)
import Diagrams.Query exposing (..)
import Diagrams.Debug exposing (..)
import Diagrams.FullWindow as DFW
import Text
import Color
import Signal
import Time
import Window exposing (..)

type alias NodeId = String 

-- drawGraph : Graph -> Diagram t a 
viewNode : NodeId -> Diagram t a
viewNode ids = let
        textid = text ids Text.defaultStyle
        node = circle 10 (fillAndStroke (Solid Color.blue) defaultStroke)
    in above textid node

main = DFW.fullWindowMain <| viewNode "foo"
