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
import Graphics.Collage as C
import Dict
import Text
import Color
import Signal
import Time
import Window exposing (..)

type alias NodeId = String 
type alias DrawEdge = { from : NodeId, to : NodeId }
type alias DrawGraph = { nodes : Dict.Dict NodeId Point
                       , edges : List DrawEdge
                       }

-- Shows a circle representing a node, with a given ID.
viewNode : NodeId -> Diagram t a
viewNode ids = let
        textid = text ids Text.defaultStyle
        node = circle 10 (fillAndStroke (Solid Color.blue) defaultStroke)
    in above textid node

viewEdge : DrawGraph -> DrawEdge -> Diagram t a
viewEdge graph edge =
    let from = case Dict.get edge.from graph.nodes of 
                    Just a -> a
                    Nothing -> error
        to   = Dict.get edge.to graph.nodes
    in viewGenericEdge from to
   
viewGenericEdge : Point -> Point -> Diagram t a 
viewGenericEdge fromCoords toCoords =
      let (fcx, fcy) = fromCoords
          (tcx, tcy) = toCoords
          cpSpacing = 100
      in bezier fromCoords (fcx+cpSpacing, fcy)
                (tcx-cpSpacing, tcy) toCoords
                C.defaultLine

main = DFW.fullWindowMain <| viewNode "foo"
