module FullScreen exposing (..)

import Task
import Window

import Html exposing (program)
import Html.Events exposing (onInput)

import Maps
import Maps.Map as Map

type Msg
  = MapMsg (Maps.Msg ())
  | Resize Window.Size

main = program
  { init = init
  , update = update
  , subscriptions = subscriptions
  , view = view
  }

init =
  ( Maps.defaultModel
  , Task.attempt (Result.withDefault defaultSize >> Resize) Window.size
  )

defaultSize =
  Window.Size 500 500

update msg model =
  case msg of
    MapMsg msg ->
      Maps.update msg model
      |> Tuple.mapSecond (Cmd.map MapMsg)
    Resize size ->
      ( model
        |> Maps.updateMap (Map.setWidth <| toFloat size.width)
        |> Maps.updateMap (Map.setHeight <| toFloat size.height)
      , Cmd.none
      )

subscriptions model =
  Sub.batch
    [ Sub.map MapMsg <| Maps.subscriptions model
    , Window.resizes Resize
    ]

view model =
  Html.map MapMsg <| Maps.view model
