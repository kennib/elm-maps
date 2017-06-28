module Maps.Zoom exposing
  ( EventOptions
  , fromPinch
  , events
  )

import Json.Decode as Json

import Html
import Html.Events exposing (on, onWithOptions)

import Maps.Screen as Screen exposing (ZoomLevel)
import Maps.Pinch as Pinch exposing (Pinch)

type alias EventOptions msg =
  { zoom : Screen.Offset -> ZoomLevel -> msg
  , pinchStart : Screen.TwoFingers -> msg
  , pinchTo : Screen.TwoFingers -> msg
  , pinchStop : msg
  }

fromPinch : Float -> Float -> Pinch -> (ZoomLevel, Screen.Offset)
fromPinch mapWidth mapHeight pinch =
  let
    (start, end) = Pinch.startEnd pinch
  in
    ( logBase 2 (end.length / start.length)
    , start.center
    )

events : EventOptions msg -> ZoomLevel -> List (Html.Attribute msg)
events ({zoom, pinchStart, pinchTo, pinchStop}) mapZoom =
  [ -- Mouse
    onWithOptions "dblclick"
    { preventDefault = True, stopPropagation = False }
    <| Json.map (\offset -> zoom offset 1)
    <| Screen.decodeOffset
  , --Mouse
    onWithOptions "wheel"
    { preventDefault = True, stopPropagation = False }
      <| Json.map2
        zoom
        Screen.decodeOffset
        Screen.decodeZoom
  , -- Mobile
    onWithOptions "touchstart"
    { preventDefault = True, stopPropagation = False }
    <| Json.map (Maybe.withDefault pinchStop)
    <| Json.map (Maybe.map pinchStart)
    <| Screen.decodeTwoFingers
  , -- Mobile
    onWithOptions "touchmove"
    { preventDefault = True, stopPropagation = False }
    <| Json.map (Maybe.withDefault pinchStop)
    <| Json.map (Maybe.map pinchTo)
    <| Screen.decodeTwoFingers
  , -- Mobile
    onWithOptions "touchend"
    { preventDefault = True, stopPropagation = False }
    <| Json.succeed pinchStop
  ]
