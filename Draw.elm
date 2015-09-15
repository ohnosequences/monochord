module Draw where

import Diagrams.Core exposing (..)
import Diagrams.Align exposing (..)
import Diagrams.FillStroke exposing (..)
import Diagrams.FullWindow exposing (..)
import Diagrams.Debug exposing (..)
import Color
import Signal
import Time
import Window

main = fullWindowMain <| eqTriangle 10 (justSolidFill Color.blue)
