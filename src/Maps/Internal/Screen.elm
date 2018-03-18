module Maps.Internal.Screen exposing
  ( Offset
  , TwoFingers
  , ZoomLevel
  , offsetToTileOffset
  , offsetFromTileOffset
  , offsetToLatLng
  , offsetFromLatLng
  , decodeOffset
  , decodeZoom
  , decodeTwoFingers
  )

{-| The Screen module defines common types and functions relating to Map events.

# Definition
@docs Offset
@docs TwoFingers
@docs ZoomLevel

# Conversions
@docs offsetToTileOffset
@docs offsetFromTileOffset
@docs offsetToLatLng
@docs offsetFromLatLng

# Event Decoders
The following are decoders for getting Screen types from [HTML events](https://developer.mozilla.org/en-US/docs/Web/API/Event)
like the [mouse event](https://developer.mozilla.org/en-US/docs/Web/API/MouseEvent) etc.

@docs decodeOffset
@docs decodeZoom
@docs decodeTwoFingers
-}

import Json.Decode as Json

import Maps.Internal.Utils exposing (wrap)
import Maps.Internal.LatLng exposing (LatLng)
import Maps.Internal.Tile as Tile

{-| The Offset type defines an offset as it relates pixels making up a map.
For example the position of a mouse click, or a scroll action.
-}
type alias Offset =
  { x : Float
  , y : Float
  }

{-| The TwoFingers type defines the position of two fingers on the screen.
It is used for the pinching action for zooming.
-}
type alias TwoFingers =
  { length : Float
  , center : Offset
  }

{-| The level of zooming for the map.
This number has a [specific relation](http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames#Zoom_levels) to the map tiles shown.
-}
type alias ZoomLevel = Float

{-| This function is for converting a Screen.Offset to a Tile.Offset.
It is used to figure out which tile, and subsequently which geographic point, a user clicked on for example.

Note that it needs to know the zoom level, size of the tiles, size of the map and position of the map to perform this conversion.
-}
offsetToTileOffset : {a | tileSize : Float, zoom : ZoomLevel, width : Float, height : Float, center : LatLng} -> Offset -> Tile.Offset
offsetToTileOffset {tileSize, zoom, width, height, center} offset =
  let
    centerTile = Tile.fromLatLng (toFloat <| ceiling zoom) center
    centerOffset = Offset (offset.x - width/2) (offset.y - height/2)
  in
    Tile.Offset
      (centerTile.x + centerOffset.x/tileSize)
      (centerTile.y + centerOffset.y/tileSize)

{-| This function is for converting a Tile.Offset to a Screen.Offset.

Note that it needs to know the zoom level, size of the tiles, size of the map and position of the map to perform this conversion.
-}
offsetFromTileOffset : {a | tileSize : Float, zoom : ZoomLevel, width : Float, height : Float, center : LatLng} -> Tile.Offset -> Offset
offsetFromTileOffset {tileSize, zoom, width, height, center} tile =
  let
    centerTile = Tile.fromLatLng (toFloat <| ceiling zoom) center
  in
    Offset
      ((tile.x - centerTile.x)*tileSize + width/2)
      ((tile.y - centerTile.y)*tileSize + height/2)

{-| This function is for converting a Screen.Offset to a LatLng.

Note that it needs to know the zoom level, size of the tiles, size of the map and position of the map to perform this conversion.
-}
offsetToLatLng : {a | tileSize : Float, zoom : ZoomLevel, width : Float, height : Float, center : LatLng} -> Offset -> LatLng
offsetToLatLng ({zoom} as map) offset =
  offset
  |> offsetToTileOffset map
  |> \tile -> Tile.toLatLng (toFloat <| ceiling zoom) tile.x tile.y

{-| This function is for converting a LatLng to a Screen.Offset.

Note that it needs to know the zoom level, size of the tiles, size of the map and position of the map to perform this conversion.
-}
offsetFromLatLng : {a | tileSize : Float, zoom : ZoomLevel, width : Float, height : Float, center : LatLng} -> LatLng -> Offset
offsetFromLatLng ({tileSize, zoom} as map) latLng =
    latLng
    |> Tile.fromLatLng (toFloat <| ceiling zoom)
    |> offsetFromTileOffset map

{-| Decodes an HTML event which has an X and Y location into an Offset
-}
decodeOffset : Json.Decoder Offset
decodeOffset =
  let
    xy = Json.map2 Offset
      (Json.field "clientX" Json.float)
      (Json.field "clientY" Json.float)
  in
    Json.oneOf
      [ xy
      , Json.at ["touches", "0"] xy
      ]

{-| Decodes an [HTML wheel event](https://developer.mozilla.org/en-US/docs/Web/API/WheelEvent) into a ZoomLevel
-}
decodeZoom : Json.Decoder ZoomLevel
decodeZoom =
  Json.map scrollToZoom
    (Json.field "deltaY" Json.float)

scrollToZoom : Float -> ZoomLevel
scrollToZoom scroll =
  negate (scroll * 0.02)

{-| Decodes a touch event into a zoom level and movement offset.
-}
decodeTwoFingers : Json.Decoder (Maybe TwoFingers)
decodeTwoFingers =
  let
    xy = Json.map2 Offset
      (Json.field "clientX" Json.float)
      (Json.field "clientY" Json.float)
    twoFingers first second =
      TwoFingers
        (((first.x - second.x)^2 + (first.y - second.y)^2)^0.5)
        (Offset ((first.x+second.x)/2) ((first.y+second.y)/2))
  in
    Json.maybe
    <| Json.map2 twoFingers
      (Json.at ["touches", "0"] xy)
      (Json.at ["touches", "1"] xy)
