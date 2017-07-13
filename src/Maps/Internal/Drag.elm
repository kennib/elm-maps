module Maps.Internal.Drag exposing
  ( Drag
  , EventOptions
  , start
  , drag
  , offset
  , events
  )

import Json.Decode as Json

import Html
import Html.Events exposing (on, onWithOptions, defaultOptions, onMouseUp)

import Maps.Internal.Screen as Screen

type Drag
  = StartDrag Screen.Offset
  | Drag Screen.Offset Screen.Offset

type alias EventOptions msg =
  { dragStart : Screen.Offset -> msg
  , dragTo : Screen.Offset -> msg
  , dragStop : msg
  }

start : Screen.Offset -> Drag
start = StartDrag

drag : Screen.Offset -> Drag -> Drag
drag offset state =
  case state of
    StartDrag start -> Drag start offset
    Drag start end -> Drag end offset

offset : Drag -> Screen.Offset
offset drag =
  case drag of
    StartDrag _ ->
      { x = 0, y = 0 }
    Drag start end ->
      { x = end.x - start.x, y = end.y - start.y }

events : EventOptions msg -> Maybe Drag -> List (Html.Attribute msg)
events ({dragStart, dragTo, dragStop}) drag =
  [ -- Mouse
    if drag == Nothing then
      onWithOptions "mousedown"
      { defaultOptions | preventDefault = True }
      <| Json.map dragStart
      <| Screen.decodeOffset
    else
      on "mousemove"
      <| Json.map dragTo
      <| Screen.decodeOffset
  , -- Mouse
    onMouseUp dragStop
  , -- Mobile
    if drag == Nothing then
      onWithOptions "touchstart"
      { defaultOptions | preventDefault = True }
      <| Json.map dragStart
      <| Screen.decodeOffset
    else
      onWithOptions "touchmove"
      { defaultOptions | preventDefault = True }
      <| Json.map dragTo
      <| Screen.decodeOffset
  , -- Mobile
    onWithOptions "touchend"
    { defaultOptions | preventDefault = True }
    <| Json.succeed dragStop
  ]
