module Polygons exposing (..)

import Html exposing (program)

import Http
import Json.Decode as Json
import GeoJson exposing (GeoJson, Geometry(..))

import Maps
import Maps.Geo
import Maps.Map as Map
import Maps.Geometry as Geometry exposing (Geometry)

type Msg
  = MapsMsg (Maps.Msg Msg)
  | LoadGeometry (Result Http.Error GeoJson)

type alias Model =
  { map : Maps.Model Msg
  }

main = program
  { init = init
  , update = update 
  , subscriptions = subscriptions
  , view = view
  }

init =
  ( { map =
      Maps.defaultModel
      |> Maps.updateMap (Map.setZoom 1)
      |> Maps.updateMap (Map.moveTo <| Maps.Geo.latLng 0 0)
    }
  , Http.send LoadGeometry getGeometry
  )

getGeometry : Http.Request GeoJson
getGeometry =
  Http.get "./example.geo.json" GeoJson.decoder

subscriptions : Model -> Sub Msg
subscriptions model =
  Maps.subscriptions model.map
  |> Sub.map MapsMsg

update msg model =
  case msg of
    MapsMsg msg ->
      model.map
        |> Maps.update msg
        |> Tuple.mapFirst (\map -> { model | map = map }) 
        |> Tuple.mapSecond (Cmd.map MapsMsg)
    LoadGeometry result ->
      ( { model
        | map = model.map |> Maps.updateGeometry (\_ -> resultToGeometry result |> List.map Geometry.create)
        }
     , Cmd.none
     )

resultToGeometry : Result Http.Error GeoJson -> List GeoJson.Geometry
resultToGeometry result =
  case result of
    (Result.Ok (GeoJson.FeatureCollection features, _)) ->
      features
      |> List.filterMap .geometry
    _ -> []

view model =
  Html.div
    []
    [ Maps.view model.map |> Maps.mapView MapsMsg
    ]
