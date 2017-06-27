module Maps.Tile exposing
  ( Tile
  , Offset
  , url
  , fromLatLng
  , toLatLng
  , view
  )

import Regex exposing (HowMany(..), regex)

import Html exposing (Html)
import Html.Attributes as Attr

import Maps.LatLng as LatLng exposing (LatLng)
import Maps.Utils exposing (wrap, sinh)

type alias Url = String
type alias Tile = (Url, Offset)

type alias Offset =
  { x : Float
  , y : Float
  }

url : String -> Int -> Int -> Int -> Url
url tileServer zoom x y =
  tileServer
    |> formatInt "{z}" zoom
    |> formatInt "{x}" x
    |> formatInt "{y}" y

fromLatLng : Float -> LatLng -> Offset
fromLatLng zoom loc =
  let
    n = 2 ^ zoom
    x = n * ((loc.lng + 180) / 360)
    latRad = loc.lat * pi / 180
    y = n * (1 - (logBase e <| abs <| tan latRad + (1/cos latRad)) / pi) / 2
  in
    Offset x y

toLatLng : Float -> Float -> Float -> LatLng
toLatLng zoom tileX tileY =
  let
    n = 2 ^ zoom
    lngDeg = tileX / n * 360 - 180 |> wrap -180 180
    latRad = atan <| sinh <| pi * (1 - 2 * tileY / n)
    latDeg = latRad * 180 / pi
  in
    LatLng latDeg lngDeg

formatInt : String -> Int -> String -> String
formatInt replace number =
  Regex.replace All (regex replace) (\_ -> toString number)

view : Float -> Tile -> Html msg
view tileSize (url, offset) =
  Html.img
    [ Attr.src url
    , Attr.style
      [ ("position", "absolute")
      , ("left", toString offset.x ++ "px")
      , ("top", toString offset.y ++ "px")
      , ("width", toString tileSize ++ "px")
      , ("height", toString tileSize ++ "px")
      , ("background-color", "#ddd")
      ]
    ]
    []
