module Maps.Map exposing
  ( Map
  , setTileServer
  , setWidth
  , setHeight
  , setTileSize
  , move
  , moveTo
  , setZoom
  , zoom
  , zoomTo
  , viewBounds
  )

{-| Functions for manipulating the Map.

@docs Map

# Setters
The setters can be used to modify a map

For example, MapBox tiles on a large map:

    map
    |> setTileServer ("https://api.mapbox.com/v4/mapbox.streets/{z}/{x}/{y}.png?access_token=" ++ accessToken)
    |> setWidth 1200
    |> setHeight 800

@docs setTileServer
@docs setWidth
@docs setHeight
@docs setTileSize

# Map Movement and Zooming
@docs move
@docs moveTo
@docs setZoom
@docs zoom
@docs zoomTo
@docs viewBounds
-}

import Maps.Internal.OpaqueTypes as OpaqueTypes exposing (Map(..), opaqueMap)
import Maps.Geo
import Maps.Internal.Map as Map
import Maps.Internal.Screen as Screen

{-| The map type contains all the information necessary to display a map on the screen.
The map type is opaque, so use the functions in this module to maniuplate the map.
-}
type alias Map = OpaqueTypes.Map

{-| Set the tile server.

The format is a URL with {x} {y} {z} placeholders for the x, y and zoom coordinates.
-}
setTileServer : String -> Map -> Map
setTileServer tileServer = opaqueMap <| Map.setTileServer tileServer

{-| Set the width, as displayed on the screen, of a given Map.
-}
setWidth : Float -> Map -> Map
setWidth width = opaqueMap <| Map.setWidth width

{-| Set the height, as displayed on the screen, of a given Map.
-}
setHeight : Float -> Map -> Map
setHeight height = opaqueMap <| Map.setHeight height

{-| Set the width/height in px of a tile.
Note, this is dependent on the tile server, and the default of 256px is almost always correct.
-}
setTileSize : Float -> Map -> Map
setTileSize tileSize = opaqueMap <| Map.setTileSize tileSize

{-| Move the map a given number of pixels in the x and y dimensions.

For example, up 10px and right 10px:

    map
    |> move { x = 10, y = -10 }
-}
move : Screen.Offset -> Map -> Map
move offset = opaqueMap <| Map.move offset

{-| Move the center of the map to a specific location.

For example, move the map to Shanghai:

    map
    |> moveTo (Maps.Geo.LatLng 31.267401 121.522179)
 -}
moveTo : Maps.Geo.LatLng -> Map -> Map
moveTo latLng = opaqueMap <| Map.moveTo latLng

{-| Sets the zoom to a specific level

For example, zoom all the way out

    map
    |> zoom 0

Or all the way in:

    map
    |> zoom 19
-}
setZoom : Float -> Map -> Map
setZoom zoomLevel = opaqueMap <| Map.setZoom zoomLevel

{-| Zoom into the center of the map.

For example zoom out two levels:

    map
    |> zoom -2
-}
zoom : Float -> Map -> Map
zoom zoomLevel = opaqueMap <| Map.zoom zoomLevel

{-| Zoom into an x,y co-ordinate on the map.

For example, zoom into the top left corner of the map:

    map
    |> zoom 1 { x = 0, y = 0}
-}
zoomTo : Float -> Screen.Offset -> Map -> Map
zoomTo zoom offset = opaqueMap <| Map.zoomTo zoom offset

{-| Move and zoom the map to cover given the bounds.

For example, view th bounds of Madagascar:

    let
      madagascar =
        Maps.Geo.bounds
          { northEast = Maps.Geo.latLng -11.9519639 50.48377989999999
          , southWest = Maps.Geo.latLng -25.6065157 43.1851395
          }
    in
      map
      |> viewBounds madagascar

-}
viewBounds : Maps.Geo.Bounds -> Map -> Map
viewBounds bounds = opaqueMap <| Map.viewBounds bounds
