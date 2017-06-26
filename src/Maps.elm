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

import Html exposing (Html, program)
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
    { zoom : Int
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
        xTiles = List.range (floor <| -xCount/2) (ceiling <| xCount/2)
        yTiles = List.range (floor <| -yCount/2) (ceiling <| yCount/2)
        n = toFloat (2 ^ bounds.zoom)
        x = n * ((bounds.center.lng + 180) / 360)
        lat_rad = bounds.center.lat * pi / 180
        y = n * (1 - (logBase e <| abs <| tan lat_rad + (1/cos lat_rad)) / pi) / 2
        tileXY xx yy =
          ( tileUrl map.tileServer bounds.zoom (floor x + xx) (floor y + yy)
          , Offset
            (map.width/2  + (toFloat (floor x) - x + toFloat xx) * map.tileSize)
            (map.height/2 + (toFloat (floor y) - y + toFloat yy) * map.tileSize)
          )
      in
        tileXY
          |> (flip List.map) xTiles
          |> List.map ((flip List.map) yTiles)
          |> List.concat

tileUrl : String -> Int -> Int -> Int -> String
tileUrl tileServer zoom x y =
  tileServer
    |> formatInt "{z}" zoom
    |> formatInt "{x}" x
    |> formatInt "{y}" y

formatInt : String -> Int -> String -> String
formatInt replace number =
  Regex.replace All (regex replace) (\_ -> toString number)
