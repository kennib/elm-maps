module Maps exposing
  ( Msg(..)
  , Model
  , Options
  , map
  , defaultOptions
  , update
  , subscriptions
  , view
  )

{-| The Maps library contains the functions neccessary for an
[HTML.program](http://package.elm-lang.org/packages/elm-lang/html/2.0.0/Html#program).

# Creating a map
The quickest way to get up and running is to create a map with default options

    import Maps
    import Html exposing (program)

    main = program <| Maps.map Maps.defaultOptions

@docs map
@docs Options
@docs defaultOptions

# Definitions
@docs Msg
@docs Model

# Program functions
@docs update
@docs subscriptions
@docs view
-}

import List.Extra as List

import Json.Decode as Json

import Html exposing (Html, program)
import Html.Keyed
import Html.Attributes as Attr
import Html.Events exposing (onWithOptions)

import Maps.Map as Map exposing (Map)
import Maps.Screen as Screen exposing (Offset, ZoomLevel)
import Maps.LatLng as LatLng exposing (LatLng)
import Maps.Bounds as Bounds exposing (Bounds)
import Maps.Tile as Tile exposing (Tile)
import Maps.Drag as Drag exposing (Drag)
import Maps.Zoom as Zoom

{-| The map has events for dragging, zooming and setting the bounds displayed by the map.
-}
type Msg
  = DragStart Offset
  | DragTo Offset
  | DragStop
  | Zoom Offset ZoomLevel
  | SetBounds Bounds

{-| The map's model consists of the [properties necessary to display a static map](Maps-Map#Map),
a cache of the previous map (for simulated zooming/panning before the real tiles load in)
and the state of the map being dragged.
-}
type alias Model =
  { map : Map
  , cache : List Map
  , drag : Maybe Drag
  }

{-| The Options type allows you to configure the map display properties.

The tileServer property is a URL template of the form

    "http://somedomain.com/blabla/{z}/{x}/{y}.png"

Where {z} is the zoom level and {x}/{y} are the x/y tile coordinates.

The [bounds](Map-Bounds#Bounds) defines the area being viewed.

The width and height define the width and height of the map in pixels.

The tileSize defines the size of an individual tile in pixels (this is usually 256px).
-}
type alias Options =
  { tileServer : String
  , bounds : Bounds
  , width : Float
  , height : Float
  , tileSize : Float
  }

{-| Creates the functions needed to make a map program from a set of [options](#Options).
-}
map : Options ->
  { init : (Model, Cmd Msg)
  , update : Msg -> Model -> (Model, Cmd Msg)
  , subscriptions : Model -> Sub Msg
  , view : Model -> Html Msg
  }
map options =
  let
    mapModel =
      { tileServer = options.tileServer
      , zoom = Bounds.zoom options.tileSize options.width options.height options.bounds
      , center = Bounds.center options.bounds
      , width = options.width
      , height = options.height
      , tileSize = options.tileSize
      }
    model =
      { map = mapModel
      , cache = []
      , drag = Nothing
      }
  in
    { init = (model, Cmd.none)
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

{-| A default set of options that displays Open Street Map tiles looking at Sydney.

    { tileServer = "http://a.tile.osm.org/{z}/{x}/{y}.png"
    , bounds = Bounds.Centered
      { zoom = 10
      , center = LatLng.sydney
      }
    , width = 600
    , height = 400
    , tileSize = 256
    }
-}
defaultOptions : Options
defaultOptions =
  { tileServer = "http://a.tile.osm.org/{z}/{x}/{y}.png"
  , bounds = Bounds.Centered
    { zoom = 10
    , center = LatLng.sydney
    }
  , width = 600
  , height = 400
  , tileSize = 256
  }

{-| -}
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    DragStart offset ->
      ({ model | drag = Just <| Drag.start offset }, Cmd.none)
    DragTo offset ->
      let
        dragState = Maybe.map (Drag.drag offset) model.drag
        draggedMap map =
          dragState
          |> Maybe.map (flip Map.drag <| map)
          |> Maybe.withDefault map
      in
        (updateMap draggedMap { model | drag = dragState }, Cmd.none)
    DragStop ->
      ({ model | drag = Nothing }, Cmd.none)
    Zoom offset zoom ->
      (updateMap (Map.zoomTo zoom offset) model, Cmd.none)
    SetBounds bounds ->
      (updateMap (Map.viewBounds bounds) model, Cmd.none)

updateMap : (Map -> Map) -> Model -> Model
updateMap update model =
  { model
  | map = update model.map
  , cache = model.map :: model.cache |> List.uniqueBy (.zoom >> ceiling)
  }

{-| -}
subscriptions : Model -> Sub Msg
subscriptions map =
  Sub.none

{-| -}
view : Model -> Html Msg
view ({map, cache, drag} as model) =
  Html.div
    [ Attr.style
      [ ("width", toString map.width ++ "px")
      , ("height", toString map.height ++ "px")
      , ("background-color", "#ddd")
      ]
    ]
    [ Html.div
      [ Attr.style
        [ ("position", "absolute")
        , ("width", toString map.width ++ "px")
        , ("height", toString map.height ++ "px")
        , ("overflow", "hidden")
        ]
      , onWithOptions "mouseDown"
        { preventDefault = True, stopPropagation = False }
        <| Json.fail "No interaction"
      ]
      <| List.map (cachedTilesView map)
      <| List.reverse
      <| cache
    , Html.Keyed.node "div"
      ([ Attr.style
        [ ("position", "absolute")
        , ("width", toString map.width ++ "px")
        , ("height", toString map.height ++ "px")
        , ("overflow", "hidden")
        ]
      ]
      ++ zoomEvents
      ++ dragEvents drag
      )
      <| List.map (\((url, offset) as tile) -> (url, Tile.view map.tileSize tile))
      <| Map.tiles map
    ]

cachedTilesView : Map -> Map -> Html Msg
cachedTilesView map cachedMap =
  Html.Keyed.node "div"
    [ Attr.style <| Map.transformationStyle map.width map.height <| Map.diff map cachedMap ]
    <| List.map (\((url, offset) as tile) -> (url, Tile.view map.tileSize tile))
    <| Map.tiles cachedMap

zoomEvents : List (Html.Attribute Msg)
zoomEvents =
  Zoom.events { zoom = Zoom }

dragEvents : Maybe Drag -> List (Html.Attribute Msg)
dragEvents drag =
  Drag.events { dragStart = DragStart, dragTo = DragTo, dragStop = DragStop } drag
