module Maps.Screen exposing
  ( Offset
  , ZoomLevel
  , offsetToTileOffset
  , decodeOffset
  , decodeZoom
  )

import Json.Decode as Json

import Maps.Tile as Tile

type alias Offset =
  { x : Float
  , y : Float
  }

type alias ZoomLevel = Float

offsetToTileOffset : Float -> Offset -> Tile.Offset
offsetToTileOffset tileSize offset =
  Offset (offset.x/tileSize) (offset.y/tileSize)

decodeOffset : Json.Decoder Offset
decodeOffset =
  Json.map2 Offset
    (Json.field "clientX" Json.float)
    (Json.field "clientY" Json.float)

decodeZoom : Json.Decoder ZoomLevel
decodeZoom =
  Json.map scrollToZoom
    (Json.field "deltaY" Json.float)

scrollToZoom : Float -> ZoomLevel
scrollToZoom scroll =
  scroll * 0.05
