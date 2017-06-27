module Maps.Map exposing
  ( Map
  , move
  , zoom
  , zoomTo
  , viewBounds
  , drag
  , tiles
  )

import Maps.Screen as Screen exposing (ZoomLevel)
import Maps.LatLng as LatLng exposing (LatLng)
import Maps.Bounds as Bounds exposing (Bounds)
import Maps.Tile as Tile exposing (Tile)
import Maps.Drag as Drag exposing (Drag)
import Maps.Zoom as Zoom
import Maps.Utils exposing (wrap, cartesianMap)

type alias Map =
  { tileServer : String
  , zoom : ZoomLevel
  , center : LatLng
  , width : Float
  , height : Float
  , tileSize : Float
  }

move : Screen.Offset -> Map -> Map
move offset map =
  let
    mapTile = Tile.fromLatLng (toFloat <| ceiling map.zoom) map.center
    tile = Screen.offsetToTileOffset map.tileSize offset
    center =
      Tile.toLatLng
        (toFloat <| ceiling map.zoom)
        (mapTile.x - tile.x)
        (mapTile.y - tile.y)
  in
    { map | center = center }

zoom : ZoomLevel -> Map -> Map
zoom zoomLevel map =
  { map | zoom = min 19 <| max 0 <| map.zoom + zoomLevel }

zoomTo : ZoomLevel -> Screen.Offset -> Map -> Map
zoomTo zoomLevel offset map =
  map
  |> move { x = map.width/2 - offset.x, y = map.height/2 - offset.y }
  |> zoom zoomLevel
  |> move { x = -(map.width/2 - offset.x), y = -(map.height/2 - offset.y) }

viewBounds : Bounds -> Map -> Map
viewBounds bounds map =
  let
    zoom = Bounds.zoom map.tileSize map.width map.height bounds
  in
    { map | zoom = zoom, center = Bounds.center bounds }

drag : Drag -> Map -> Map 
drag dragState map =
  move (Drag.offset dragState) map

tiles : Map -> List Tile
tiles map =
  let
    xCount = map.width/map.tileSize
    yCount = map.height/map.tileSize
    tile = Tile.fromLatLng (toFloat <| ceiling map.zoom) map.center
    xTiles = List.range (floor <| -xCount/2) (ceiling <| xCount/2)
    yTiles = List.range (floor <| -yCount/2) (ceiling <| yCount/2)
    wrapTile = wrap 0 (2^(ceiling map.zoom))
    tileXY x y =
      ( Tile.url
        map.tileServer
        (ceiling map.zoom)
        (floor tile.x + x |> wrapTile)
        (floor tile.y + y |> wrapTile)
      , Tile.Offset
        (map.width/2  + (toFloat (floor tile.x) - tile.x + toFloat x) * map.tileSize)
        (map.height/2 + (toFloat (floor tile.y) - tile.y + toFloat y) * map.tileSize)
      )
  in
    cartesianMap tileXY xTiles yTiles
      |> List.concat
