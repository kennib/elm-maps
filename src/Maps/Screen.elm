module Maps.Screen exposing
  ( Offset
  , ZoomLevel
  , offsetToTileOffset
  , decodeOffset
  , decodeZoom
  )

{-| The Screen module defines common types and functions relating to Map events.

# Definition
@docs Offset
@docs ZoomLevel

# Conversions
@docs offsetToTileOffset

# Event Decoders
The following are decoders for getting Screen types from [HTML events](https://developer.mozilla.org/en-US/docs/Web/API/Event)
like the [mouse event](https://developer.mozilla.org/en-US/docs/Web/API/MouseEvent) etc.

@docs decodeOffset
@docs decodeZoom
-}

import Json.Decode as Json

import Maps.Tile as Tile

{-| The Offset type defines an offset as it relates pixels making up a map.
For example the position of a mouse click, or a scroll action.
-}
type alias Offset =
  { x : Float
  , y : Float
  }

{-| The level of zooming for the map.
This number has a [specific relation](http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames#Zoom_levels) to the map tiles shown.
-}
type alias ZoomLevel = Float

{-| This function is for converting a Screen.Offset to a Tile.Offset.
It is used to figure out which tile, and subsequently which geographic point, a user clicked on for example.

Note that It needs to know the size of the tiles to perform this conversion.
-}
offsetToTileOffset : Float -> Offset -> Tile.Offset
offsetToTileOffset tileSize offset =
  Offset (offset.x/tileSize) (offset.y/tileSize)


{-| Decodes an HTML event which has an X and Y location into an Offset
-}
decodeOffset : Json.Decoder Offset
decodeOffset =
  Json.map2 Offset
    (Json.field "clientX" Json.float)
    (Json.field "clientY" Json.float)

{-| Decodes an [HTML wheel event](https://developer.mozilla.org/en-US/docs/Web/API/WheelEvent) into a ZoomLevel
-}
decodeZoom : Json.Decoder ZoomLevel
decodeZoom =
  Json.map scrollToZoom
    (Json.field "deltaY" Json.float)

scrollToZoom : Float -> ZoomLevel
scrollToZoom scroll =
  scroll * 0.05
