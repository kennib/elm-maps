module Search exposing (..)

import Html exposing (program)
import Html.Events exposing (onInput)
import Geocoding
import Http

import Maps exposing (Msg)
import Maps.Geo exposing (Bounds)
import Maps.Map as Map

type Msg
  = MapMsg (Maps.Msg ())
  | GeoCode String
  | GoToGeoCode String Bounds
  | NoResults String

apiKey = "AIzaSyCkOFxL5NF1feuebbB6PW8fP3SDg1aa6tM"

main = program
  { init = init
  , update = update
  , subscriptions = subscriptions
  , view = view
  }

init =
  ( { map = Maps.defaultModel
      |> Maps.updateMap (Map.setHeight 600)
      |> Maps.updateMap (Map.setWidth 1000)
    , place = ""
    }
  , Cmd.none)

update msg model =
  case msg of
    MapMsg mapMsg ->
      Maps.update mapMsg model.map
      |> Tuple.mapFirst (\map -> { model | map = map })
      |> Tuple.mapSecond (Cmd.map MapMsg)
    GeoCode place ->
      ({ model | place = place }, geocode place)
    GoToGeoCode place bounds ->
      if place == model.place then
        model.map
        |> Maps.updateMap (Map.viewBounds bounds)
        |> \map -> ({ model | map = map }, Cmd.none)
      else
        (model, Cmd.none)
    NoResults place ->
      (model, Cmd.none)

geocode : String -> Cmd Msg
geocode place =
  Geocoding.requestForAddress apiKey place 
  |> Geocoding.send (Maybe.withDefault (NoResults place) << Maybe.map (GoToGeoCode place) << getFirstBounds)

getFirstBounds : Result Http.Error Geocoding.Response -> Maybe Bounds
getFirstBounds result =
  result
  |> Result.toMaybe
  |> Maybe.map .results
  |> Maybe.andThen List.head
  |> Maybe.map .geometry
  |> Maybe.map .viewport
  |> Maybe.map (\bounds ->
    Maps.Geo.bounds
    { northEast = { lat = bounds.northeast.latitude, lng = bounds.northeast.longitude }
    , southWest = { lat = bounds.southwest.latitude, lng = bounds.southwest.longitude }
    }
  )

subscriptions model =
  Sub.map MapMsg <| Maps.subscriptions model.map

view model =
  Html.div
    []
    [ Html.map MapMsg <| Maps.view model.map
    , Html.input
      [ onInput GeoCode
      ]
      []
    ]
