module Maps exposing
  ( Msg(..)
  , Map
  , Options
  , Tile
  , Bounds(..)
  , GeoLocation
  , map
  , defaultOptions
  , update
  , subscriptions
  , view
  )

import Regex exposing (HowMany(..), regex)

import Json.Decode as Json

import Html exposing (Html, program)
import Html.Keyed
import Html.Events exposing (on, onWithOptions, onMouseUp, onMouseLeave)
import Html.Attributes as Attr

type Msg
  = DragStart Offset
  | DragTo Offset
  | DragStop
  | Zoom Offset Float
  | SetBounds Bounds

type alias Map =
  { tileServer : String
  , zoom : Float
  , center : GeoLocation
  , width : Float
  , height : Float
  , tileSize : Float
  , drag : Maybe Drag
  }

type alias Options =
  { tileServer : String
  , bounds : Bounds
  , width : Float
  , height : Float
  , tileSize : Float
  }

type alias Tile = (String, Offset)

type alias Offset =
  { x : Float
  , y : Float
  }

type Bounds
  = Bounds
    { northEast : GeoLocation
    , southWest : GeoLocation
    }
  | Centered
    { zoom : Float
    , center : GeoLocation
    }

type alias GeoLocation =
  { lat : Float
  , lng : Float
  }

type Drag
  = StartDrag Offset
  | Drag Offset Offset

map : Options ->
  { init : (Map, Cmd Msg)
  , update : Msg -> Map -> (Map, Cmd Msg)
  , subscriptions : Map -> Sub Msg
  , view : Map -> Html Msg
  }
map options =
  let
    model =
      { tileServer = options.tileServer
      , zoom = boundsZoom options.tileSize options.width options.height options.bounds
      , center = boundsCenter options.bounds
      , width = options.width
      , height = options.height
      , tileSize = options.tileSize
      , drag = Nothing
      }
  in
    { init = (model, Cmd.none)
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

defaultOptions : Options
defaultOptions =
  { tileServer = "http://a.tile.osm.org/{z}/{x}/{y}.png"
  , bounds = Centered
    { zoom = 10
    , center = sydney
    }
  , width = 600
  , height = 400
  , tileSize = 256
  }

sydney : GeoLocation
sydney = { lat = -33.865143, lng = 151.209900 }

boundsZoom : Float -> Float -> Float -> Bounds -> Float
boundsZoom tileSize width height bounds =
  case bounds of
    Bounds bounds ->
      let
        (ne, sw) = (bounds.northEast, bounds.southWest)
        -- The following assumes a Mercator projection
        latY lat = sin (lat * pi / 180)
        radX2 lat = (logBase e ((1 + latY lat) / (1 - latY lat))) / 2
        latRad lat = (max (-pi) <| min (radX2 lat) pi) / 2
        latFraction = (latRad ne.lat) - (latRad sw.lat)
        lngFraction = ((ne.lng - sw.lng) |> wrap 0 360) / 360
        zoom mapSize tileSize frac = logBase 2 (mapSize / tileSize / frac)
      in
        min
          (zoom width tileSize lngFraction)
          (zoom height tileSize latFraction)
    Centered bounds ->
      bounds.zoom

boundsCenter : Bounds -> GeoLocation
boundsCenter bounds =
  case bounds of
    Bounds bounds ->
      { lat = (bounds.northEast.lat + bounds.southWest.lat) / 2
      , lng = (bounds.northEast.lng + bounds.southWest.lng) / 2
      }
    Centered bounds ->
      bounds.center

update : Msg -> Map -> (Map, Cmd Msg)
update msg map =
  case msg of
    DragStart offset ->
      ({ map | drag = Just <| StartDrag offset }, Cmd.none)
    DragTo offset ->
      let
        dragState = Maybe.map (drag offset) map.drag
        draggedMap =
          case dragState of
            Just drag -> applyDrag drag map
            Nothing -> map
      in
        ({ draggedMap | drag = dragState }, Cmd.none)
    DragStop ->
      ({ map | drag = Nothing }, Cmd.none)
    Zoom offset zoom ->
      let
        zoomedMap =
          map
          |> applyOffset { x = map.width/2 - offset.x, y = map.height/2 - offset.y }
          |> applyZoom zoom
          |> applyOffset { x = -(map.width/2 - offset.x), y = -(map.height/2 - offset.y) }
      in
        (zoomedMap, Cmd.none)
    SetBounds bounds ->
      let
        zoom = boundsZoom map.tileSize map.width map.height bounds
      in
        ({ map | zoom = zoom, center = boundsCenter bounds }, Cmd.none)

subscriptions : Map -> Sub Msg
subscriptions map =
  Sub.none

drag : Offset -> Drag -> Drag
drag offset state =
  case state of
    StartDrag start -> Drag start offset
    Drag start end -> Drag end offset

applyDrag : Drag -> Map -> Map
applyDrag drag map =
  case drag of
    StartDrag _ -> map
    Drag start end ->
      applyOffset { x = end.x - start.x, y = end.y - start.y } map

applyOffset : Offset -> Map -> Map
applyOffset pos map =
  let
    mapTile = locationTile (toFloat <| ceiling map.zoom) map.center
    tile = screenTile map pos.x pos.y
    center =
      tileLocation
        (toFloat <| ceiling map.zoom)
        (mapTile.x - tile.x)
        (mapTile.y - tile.y)
  in
    { map | center = center }

applyZoom : Float -> Map -> Map
applyZoom zoom map =
  { map | zoom = min 19 <| max 0 <| map.zoom + zoom }

scrollToZoom : Float -> Float
scrollToZoom scroll =
  scroll * 0.05

view : Map -> Html Msg
view map =
  Html.div
    [ Attr.style
      [ ("width", toString map.width ++ "px")
      , ("height", toString map.height ++ "px")
      ]
    ]
    [ Html.Keyed.node "div"
      [ Attr.style
        [ ("position", "absolute")
        , ("width", toString map.width ++ "px")
        , ("height", toString map.height ++ "px")
        , ("overflow", "hidden")
        ]
      , if map.drag == Nothing then
        onWithOptions "mousedown"
        { preventDefault = True, stopPropagation = False }
        <| Json.map DragStart
        <| Json.map2 Offset
          (Json.field "clientX" Json.float)
          (Json.field "clientY" Json.float)
      else
        on "mousemove"
        <| Json.map DragTo
        <| Json.map2 Offset
          (Json.field "clientX" Json.float)
          (Json.field "clientY" Json.float)
      , onMouseUp DragStop
      , onWithOptions "wheel"
        { preventDefault = True, stopPropagation = False }
        <| Json.map3
          (\x y scroll -> Zoom (Offset x y) <| scrollToZoom scroll)
          (Json.field "clientX" Json.float)
          (Json.field "clientY" Json.float)
          (Json.field "deltaY" Json.float)
      , onWithOptions "dblclick"
        { preventDefault = True, stopPropagation = False }
        <| Json.map2
          (\x y -> Zoom (Offset x y) 1)
          (Json.field "clientX" Json.float)
          (Json.field "clientY" Json.float)
      ]
      <| List.map (\(url, offset) -> (url, viewTile map (url, offset)))
      <| tiles map
    ]

viewTile : Map -> Tile -> Html Msg
viewTile map (url, offset) =
  Html.img
    [ Attr.src url
    , Attr.style
      [ ("position", "absolute")
      , ("left", toString offset.x ++ "px")
      , ("top", toString offset.y ++ "px")
      , ("width", toString map.tileSize ++ "px")
      , ("height", toString map.tileSize ++ "px")
      , ("background-color", "#ddd")
      ]
    ]
    []

tiles : Map -> List Tile
tiles map =
  let
    xCount = map.width/map.tileSize
    yCount = map.height/map.tileSize
    tile = locationTile (toFloat <| ceiling map.zoom) map.center
    xTiles = List.range (floor <| -xCount/2) (ceiling <| xCount/2)
    yTiles = List.range (floor <| -yCount/2) (ceiling <| yCount/2)
    wrapTile = wrap 0 (2^(ceiling map.zoom))
    tileXY x y =
      ( tileUrl
        map.tileServer
        (ceiling map.zoom)
        (floor tile.x + x |> wrapTile)
        (floor tile.y + y |> wrapTile)
      , Offset
        (map.width/2  + (toFloat (floor tile.x) - tile.x + toFloat x) * map.tileSize)
        (map.height/2 + (toFloat (floor tile.y) - tile.y + toFloat y) * map.tileSize)
      )
  in
    tileXY
      |> (flip List.map) xTiles
      |> List.map ((flip List.map) yTiles)
      |> List.concat

screenTile : Map -> Float -> Float -> Offset
screenTile map x y =
  Offset (x/map.tileSize) (y/map.tileSize)

locationTile : Float -> GeoLocation -> Offset
locationTile zoom loc =
  let
    n = 2 ^ zoom
    x = n * ((loc.lng + 180) / 360)
    lat_rad = loc.lat * pi / 180
    y = n * (1 - (logBase e <| abs <| tan lat_rad + (1/cos lat_rad)) / pi) / 2
  in
    Offset x y

tileLocation : Float -> Float -> Float -> GeoLocation
tileLocation zoom x y =
  let
    n = 2 ^ zoom
    lng_deg = x / n * 360 - 180 |> wrap -180 180
    lat_rad = atan <| sinh <| pi * (1 - 2 * y / n)
    lat_deg = lat_rad * 180 / pi
    sinh x = ((e ^ x) - (e ^ -x)) / 2
  in
    GeoLocation lat_deg lng_deg

tileUrl : String -> Int -> Int -> Int -> String
tileUrl tileServer zoom x y =
  tileServer
    |> formatInt "{z}" zoom
    |> formatInt "{x}" x
    |> formatInt "{y}" y

wrap min max n =
  if n < min then
    wrap min max <| n + (max-min)
  else if n >= max then
    wrap min max <| n - (max-min)
  else
    n

formatInt : String -> Int -> String -> String
formatInt replace number =
  Regex.replace All (regex replace) (\_ -> toString number)
