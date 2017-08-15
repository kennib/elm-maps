module Maps.Internal.Marker exposing
  ( Marker(..)
  , view
  )

import Html exposing (Html)
import Html.Attributes as Attr

import Maps.Internal.Screen as Screen exposing (ZoomLevel)
import Maps.Internal.LatLng as LatLng exposing (LatLng)

type Marker msg
  = DefaultMarker LatLng
  | CustomMarker (Html msg) LatLng

view : {a | tileSize : Float, zoom : ZoomLevel, width : Float, height : Float, center : LatLng} -> Marker msg -> Html msg
view map marker =
  case marker of
    DefaultMarker latLng ->
      let
        offset = Screen.offsetFromLatLng map latLng
        width = 18
        height = 36
      in
        Html.span
          [ Attr.style
            [ ("position", "absolute")
            , ("left", toString (offset.x  - width/2) ++ "px")
            , ("top", toString (offset.y - height) ++ "px")
            , ("display", "inline-block")
            , ("width", toString width ++ "px")
            , ("height", toString height ++ "px")
            , ("background-image", "url(\"data:image/svg+xml;utf-8,"++markerSvg++"\")")
            , ("background-size", "100% auto")
            ]
          ]
          [
          ]
    CustomMarker html latLng ->
      let
        offset = Screen.offsetFromLatLng map latLng
      in
        Html.span
          [ Attr.style
            [ ("position", "absolute")
            , ("left", toString offset.x ++ "px")
            , ("top", toString offset.y ++ "px")
            , ("pointer-events", "initial")
            , ("display", "inline-block")
            , ("text-align", "center")
            , ("-webkit-transform", "translateX(-50%) translateY(-50%)")
            , ("-moz-transform", "translateX(-50%) translateY(-50%)")
            , ("transform", "translateX(-50%) translateY(-50%)")
            ]
          ]
          [ html
          ]

markerSvg =
  """<svg xmlns='http://www.w3.org/2000/svg' x='0px' y='0px' width='12' height='24' fill='#cc6666' stroke='#000000' version='1.1' viewBox='0 0 12 24'><path d='M 2,12 A 5.5,5.5 0 1,1 10,12 C 8,14 6,22 6,24 C 6,22 4,14 2,12 z  M 4,8.5 A 2,2 0 1,0 8,8.5 A 2,2 0 0,0 4,8.5 z' fill-rule='evenodd'/></svg>"""
