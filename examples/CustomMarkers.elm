module CustomMarkers exposing (..)

import Html exposing (program)
import Html.Attributes as Attr
import Html.Events exposing (onClick)

import Http
import Json.Decode as Json
import GeoJson exposing (GeoJson, Geometry(..))

import Maps
import Maps.Geo
import Maps.Map as Map
import Maps.Marker as Marker exposing (Marker)

type Msg
  = MapsMsg (Maps.Msg Msg)
  | Select SportsGround
  | LoadSportsGrounds (Result Http.Error GeoJson)

type alias Model =
  { map : Maps.Model Msg
  , sportsGrounds : List SportsGround
  , selected : Maybe SportsGround
  }

type alias SportsGround =
  { latLng : Maps.Geo.LatLng
  , name : String
  , kind : String
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
      |> Maps.updateMap (Map.setZoom 11 >> Map.moveTo ballarat)
    , sportsGrounds = []
    , selected = Nothing
    }
  , Http.send LoadSportsGrounds getSportsGrounds
  )

ballarat = Maps.Geo.latLng -37.5672953 143.782701

getSportsGrounds : Http.Request GeoJson
getSportsGrounds = 
  Http.get "./ballarat-sports-grounds.json" GeoJson.decoder

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
    Select ground ->
      ({ model | selected = Just ground }, Cmd.none)
    LoadSportsGrounds result ->
      let
        grounds = resultToSportsGrounds result
      in
        ( { model
          | map = model.map |> Maps.updateMarkers (\markers -> grounds |> List.map groundMarker)
          , sportsGrounds = grounds
          }
       , Cmd.none
       )

resultToSportsGrounds : Result Http.Error GeoJson -> List SportsGround
resultToSportsGrounds result =
  let
    featureToSportsGround feature =
      case feature.geometry of
        Just (Point (lng, lat, _)) ->
          Result.map2 (SportsGround (Maps.Geo.latLng lat lng))
            (property "site" feature)
            (property "feat_type" feature)
          |> Result.toMaybe
        _ ->
          Nothing
    property propertyName feature =
      Json.decodeValue
        (Json.field propertyName Json.string)
        feature.properties
  in
    case result of
      (Result.Ok (GeoJson.FeatureCollection features, _)) ->
        features
        |> List.filterMap featureToSportsGround
      _ -> []

groundMarker : SportsGround -> Marker Msg
groundMarker ground =
  let
    view =
      Html.span
        [ onClick <| Select ground, Attr.style [("cursor", "pointer")] ]
        [ Html.text <| groundSymbol ground.kind ]
  in
    Marker.createCustom view ground.latLng

groundSymbol : String -> String
groundSymbol kind =
  let
    symbol (name, symbol) = if String.contains name kind then symbol else ""
    default defaultString string = if String.isEmpty string then defaultString else string
  in
    [ ("Football", "🏉")
    , ("Soccer", "⚽")
    , ("Cricket", "🔴")
    , ("Softball", "⚾")
    , ("Equestrian", "🐎")
    ]
    |> List.map symbol
    |> String.join ""
    |> default "❌"

view model =
  Html.div
    []
    [ Maps.view model.map |> Maps.mapView MapsMsg
    , case model.selected of
      Just ground ->
        Html.div
          []
          [ Html.h1 [] [ Html.text ground.name ]
          , Html.p [] [ Html.text ground.kind ]
          ]
      Nothing ->
        Html.p [] [ Html.text "Click on a map marker" ]
    ]
