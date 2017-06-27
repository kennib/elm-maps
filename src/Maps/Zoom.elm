module Maps.Zoom exposing
  ( EventOptions
  , events
  )

import Json.Decode as Json

import Html
import Html.Events exposing (on, onWithOptions)

import Maps.Screen as Screen exposing (ZoomLevel)

type alias EventOptions msg =
  { zoom : Screen.Offset -> ZoomLevel -> msg
  }

events : EventOptions msg -> List (Html.Attribute msg)
events ({zoom}) =
  [ onWithOptions "dblclick"
    { preventDefault = True, stopPropagation = False }
    <| Json.map (\offset -> zoom offset 1)
    <| Screen.decodeOffset
  , onWithOptions "wheel"
    { preventDefault = True, stopPropagation = False }
      <| Json.map2
        zoom
        Screen.decodeOffset
        Screen.decodeZoom
  ]
