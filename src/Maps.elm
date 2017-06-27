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

import Html exposing (Html, program)
import Html.Keyed
import Html.Attributes as Attr

import Maps.Map as Map exposing (Map)
import Maps.Screen as Screen exposing (Offset, ZoomLevel)
import Maps.LatLng as LatLng exposing (LatLng)
import Maps.Bounds as Bounds exposing (Bounds)
import Maps.Tile as Tile exposing (Tile)
import Maps.Drag as Drag exposing (Drag)
import Maps.Zoom as Zoom

type Msg
  = DragStart Offset
  | DragTo Offset
  | DragStop
  | Zoom Offset ZoomLevel
  | SetBounds Bounds

type alias Model =
  { map : Map
  , drag : Maybe Drag
  }

type alias Options =
  { tileServer : String
  , bounds : Bounds
  , width : Float
  , height : Float
  , tileSize : Float
  }

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
  , bounds = Bounds.Centered
    { zoom = 10
    , center = LatLng.sydney
    }
  , width = 600
  , height = 400
  , tileSize = 256
  }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    DragStart offset ->
      ({ model | drag = Just <| Drag.start offset }, Cmd.none)
    DragTo offset ->
      let
        dragState = Maybe.map (Drag.drag offset) model.drag
        draggedMap =
          dragState
          |> Maybe.map (flip Map.drag <| model.map)
          |> Maybe.withDefault model.map
      in
        ({ model | map = draggedMap, drag = dragState }, Cmd.none)
    DragStop ->
      ({ model | drag = Nothing }, Cmd.none)
    Zoom offset zoom ->
      (updateMap (Map.zoomTo zoom offset) model, Cmd.none)
    SetBounds bounds ->
      (updateMap (Map.viewBounds bounds) model, Cmd.none)

updateMap : (Map -> Map) -> Model -> Model
updateMap update model =
  { model | map = update model.map }

subscriptions : Model -> Sub Msg
subscriptions map =
  Sub.none

view : Model -> Html Msg
view ({map, drag} as model) =
  Html.div
    [ Attr.style
      [ ("width", toString map.width ++ "px")
      , ("height", toString map.height ++ "px")
      ]
    ]
    [ Html.Keyed.node "div"
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

zoomEvents : List (Html.Attribute Msg)
zoomEvents =
  Zoom.events { zoom = Zoom }

dragEvents : Maybe Drag -> List (Html.Attribute Msg)
dragEvents drag =
  Drag.events { dragStart = DragStart, dragTo = DragTo, dragStop = DragStop } drag
