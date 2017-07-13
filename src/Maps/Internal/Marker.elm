module Maps.Internal.Marker exposing
  ( Marker(..)
  , view
  )

import Html exposing (Html)
import Html.Attributes as Attr

import Maps.Internal.Screen as Screen exposing (ZoomLevel)
import Maps.Internal.LatLng as LatLng exposing (LatLng)

type Marker
  = DefaultMarker LatLng

view : {a | tileSize : Float, zoom : ZoomLevel, width : Float, height : Float, center : LatLng} -> Marker -> Html msg
view map marker =
  case marker of
    DefaultMarker latLng ->
      let
        offset = Screen.offsetFromLatLng map latLng
      in
        Html.span
          [ Attr.style
            [ ("position", "absolute")
            , ("left", toString offset.x ++ "px")
            , ("top", toString offset.y ++ "px")
            , ("display", "inline-block")
            , ("width", "10px")
            , ("height", "16px")
            , ("background-color", "red")
            ]
          ]
          [
          ]
