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
import Html.Events exposing (on)
import Html.Attributes as Attr

type Msg
  = SetBounds Bounds

type alias Map =
  { tileServer : String
  , bounds : Bounds
  , width : Float
  , height : Float
  , tileSize : Float
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
  = Centered
    { zoom : Float
    , center : GeoLocation
    }

type alias GeoLocation =
  { lat : Float
  , lng : Float
  }

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
      , bounds = options.bounds
      , width = options.width
      , height = options.height
      , tileSize = options.tileSize
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

update : Msg -> Map -> (Map, Cmd Msg)
update msg map =
  case msg of
    SetBounds bounds ->
      ({ map | bounds = bounds }, Cmd.none)

subscriptions : Map -> Sub Msg
subscriptions map =
  Sub.none

view : Map -> Html Msg
view map =
  Html.div
    [ Attr.style
      [ ("width", toString map.width ++ "px")
      , ("height", toString map.height ++ "px")
      ]
    ]
    [ Html.div
      [ Attr.style
        [ ("position", "absolute")
        , ("width", toString map.width ++ "px")
        , ("height", toString map.height ++ "px")
        , ("overflow", "hidden")
        ]
      , on "mousedown"
        <| Json.map
        (\pos ->
          case map.bounds of
            Centered bounds ->
              SetBounds
              <| Centered
                { zoom = bounds.zoom
                , center =
                  let
                    mapTile = locationTile (toFloat <| ceiling bounds.zoom) bounds.center
                    centerTile = screenTile map (map.width/2) (map.height/2)
                    tile = screenTile map pos.x pos.y
                  in
                    tileLocation
                      bounds.zoom
                      (mapTile.x + tile.x - centerTile.x)
                      (mapTile.y + tile.y - centerTile.y)
                }
        )
        <| Json.map2 Offset
          (Json.field "clientX" Json.float)
          (Json.field "clientY" Json.float)
      ]
      <| List.map (viewTile map)
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
      ]
    ]
    []

tiles : Map -> List Tile
tiles map =
  case map.bounds of
    Centered bounds ->
      let
        xCount = map.width/map.tileSize
        yCount = map.height/map.tileSize
        tile = locationTile (toFloat <| ceiling bounds.zoom) bounds.center
        xTiles = List.range (floor <| -xCount/2) (ceiling <| xCount/2)
        yTiles = List.range (floor <| -yCount/2) (ceiling <| yCount/2)
        tileXY x y =
          ( tileUrl
            map.tileServer
            (ceiling bounds.zoom)
            (floor tile.x + x)
            (floor tile.y + y)
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
    lng_deg = x / n * 360 - 180
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

formatInt : String -> Int -> String -> String
formatInt replace number =
  Regex.replace All (regex replace) (\_ -> toString number)
