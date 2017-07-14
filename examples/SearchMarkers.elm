module Markers exposing (..)

import Html exposing (program)
import Html.Events exposing (onInput)
import Geocoding
import Http

import Maps exposing (Msg)
import Maps.Geo exposing (LatLng, Bounds)
import Maps.Marker as Marker exposing (Marker)
import Maps.Map as Map

type Msg
  = MapMsg (Maps.Msg ())
  | GeoCode String
  | GoToGeoCode String (Bounds, LatLng)
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
    GoToGeoCode place (bounds, latLng) ->
      if place == model.place then
        model.map
        |> Maps.updateMap (Map.viewBounds bounds)
        |> Maps.updateMarkers (\_ -> [Marker.create latLng])
        |> \map -> ({ model | map = map }, Cmd.none)
      else
        (model, Cmd.none)
    NoResults place ->
      (model, Cmd.none)

geocode : String -> Cmd Msg
geocode place =
  Geocoding.requestForAddress apiKey place 
  |> Geocoding.send (Maybe.withDefault (NoResults place) << Maybe.map (GoToGeoCode place) << getFirstLocation)

getFirstLocation : Result Http.Error Geocoding.Response -> Maybe (Bounds, LatLng)
getFirstLocation result =
  result
  |> Result.toMaybe
  |> Maybe.map .results
  |> Maybe.andThen List.head
  |> Maybe.map .geometry
  |> Maybe.map (\geometry ->
    let
      bounds =
        Maps.Geo.bounds
        { northEast = { lat = geometry.viewport.northeast.latitude, lng = geometry.viewport.northeast.longitude }
        , southWest = { lat = geometry.viewport.southwest.latitude, lng = geometry.viewport.southwest.longitude }
        }
      latLng =
        { lat = geometry.location.latitude
        , lng = geometry.location.longitude
        }
    in
      (bounds, latLng)
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
