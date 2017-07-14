module SearchMarkers exposing (..)

import Task
import Window

import Html exposing (program)
import Html.Events exposing (onInput)

import Maps
import Maps.Geo
import Maps.Map as Map
import Maps.Marker as Marker

main = program
  { init = init
  , update = Maps.update 
  , subscriptions = Maps.subscriptions
  , view = Maps.view
  }

init =
  ( Maps.defaultModel
    |> Maps.updateMap (Map.setZoom 14 >> Map.moveTo sydney)
    |> Maps.updateMarkers ((::) (Marker.create sydney))
    |> Maps.updateMarkers (\markers -> List.map Marker.create attractions ++ markers)
  , Cmd.none
  )

sydney = Maps.Geo.latLng -33.865143 151.209900

attractions =
  List.map (uncurry Maps.Geo.latLng)
    [ (-33.852324, 151.210819)
    , (-33.856872, 151.215239)
    , (-33.870397, 151.208835)
    ]
