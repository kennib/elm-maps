module Example exposing (..)

import Html exposing (program)
import Html.Events exposing (onInput)
import Geocoding
import Http

import Maps exposing (Msg(..), defaultOptions)

type Msg
  = MapMsg Maps.Msg
  | GeoCode String
  | GoToGeoCode String Maps.Bounds
  | NoResults String

apiKey = "AIzaSyCkOFxL5NF1feuebbB6PW8fP3SDg1aa6tM"

main = program
  { init = init
  , update = update
  , subscriptions = subscriptions
  , view = view
  }

map =
  Maps.map
  { defaultOptions
  | height = 600
  , width = 1000
  }

init =
  map.init
  |> Tuple.mapFirst (\map -> { map = map, place = "" })
  |> Tuple.mapSecond (Cmd.map MapMsg)

update msg model =
  case msg of
    MapMsg mapMsg ->
      map.update mapMsg model.map
      |> Tuple.mapFirst (\map -> { model | map = map })
      |> Tuple.mapSecond (Cmd.map MapMsg)
    GeoCode place ->
      ({ model | place = place }, geocode place)
    GoToGeoCode place bounds ->
      if place == model.place then
        map.update (SetBounds bounds) model.map
        |> Tuple.mapFirst (\map -> { model | map = map })
        |> Tuple.mapSecond (Cmd.map MapMsg)
      else
        (model, Cmd.none)
    NoResults place ->
      (model, Cmd.none)

geocode : String -> Cmd Msg
geocode place =
  Geocoding.requestForAddress apiKey place 
  |> Geocoding.send (Maybe.withDefault (NoResults place) << Maybe.map (GoToGeoCode place) << getFirstBounds)

getFirstBounds : Result Http.Error Geocoding.Response -> Maybe Maps.Bounds
getFirstBounds result =
  result
  |> Result.toMaybe
  |> Maybe.map .results
  |> Maybe.andThen List.head
  |> Maybe.map .geometry
  |> Maybe.map .viewport
  |> Maybe.map (\bounds ->
    Maps.Bounds
    { northEast = { lat = bounds.northeast.latitude, lng = bounds.northeast.longitude }
    , southWest = { lat = bounds.southwest.latitude, lng = bounds.southwest.longitude }
    }
  )

subscriptions model =
  Sub.map MapMsg <| map.subscriptions model.map

view model =
  Html.div
    []
    [ Html.map MapMsg <| map.view model.map
    , Html.input
      [ onInput GeoCode
      ]
      []
    ]
