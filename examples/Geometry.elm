module Polygons exposing (..)

import Html exposing (program)
import Html.Attributes as Attr
import Html.Events exposing (onClick)

import Http
import Json.Decode as Json
import GeoJson exposing (GeoJson, Geometry(..))

import Maps
import Maps.Geo
import Maps.Map as Map
import Maps.Geometry as Geometry exposing (Geometry)

type Msg
  = MapsMsg (Maps.Msg Msg)
  | Select Country
  | LoadCountries (Result Http.Error GeoJson)

type alias Model =
  { map : Maps.Model Msg
  , countries : List Country
  , selected : Maybe Country
  }

type alias Country = (String, GeoJson.Geometry)

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
    , countries = []
    , selected = Nothing
    }
  , Http.send LoadCountries getCountries
  )

getCountries : Http.Request GeoJson
getCountries = 
  Http.get "./countries.geo.json" GeoJson.decoder

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
    Select country ->
      ({ model | selected = Just country }, Cmd.none)
    LoadCountries result ->
      let
        countries = resultToCountries result
      in
        ( { model
          | map = model.map |> Maps.updateGeometry (\geometry -> countries |> List.map Tuple.second |> List.map Geometry.create)
          , countries = countries
          }
       , Cmd.none
       )

resultToCountries : Result Http.Error GeoJson -> List Country
resultToCountries result =
  let
    featureToCountry feature =
      Maybe.map2 (,)
        (property "name" feature |> Result.toMaybe)
        (feature.geometry)
    property propertyName feature =
      Json.decodeValue
        (Json.field propertyName Json.string)
        feature.properties
  in
    case result of
      (Result.Ok (GeoJson.FeatureCollection features, _)) ->
        features
        |> List.filterMap featureToCountry
      _ -> []

view model =
  Html.div
    []
    [ Maps.view model.map |> Maps.mapView MapsMsg
    , case model.selected of
      Just (country, geometry) ->
        Html.div
          []
          [ Html.text country ]
      Nothing ->
        Html.p [] [ Html.text "Mouse over a country" ]
    ]
