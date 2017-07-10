module Tests.Map exposing
  ( moveTest
  , zoomToTest
  )

import Expect exposing (Expectation)
import Test exposing (..)

import Fuzzers exposing (..)
import Expects

import Maps.Screen as Screen exposing (ZoomLevel)
import Maps.Tile as Tile
import Maps.LatLng exposing (LatLng)
import Maps.Map exposing (..)

moveTest : Test
moveTest =
  describe "Map movement tests"
    [ describe "Fuzz test map movement" 
      [ fuzz2 noOffset map "Moving a map with no offset should not change the map"
        <| \offset map ->
            Expects.equalMap 0.0001
              map
              (move offset map)
      , fuzz2 offset map "Moving a map should result in a change in the map"
        <| \offset map ->
          if offset.x == 0 && offset.y == 0 then
            Expect.equal map <| move offset map
          else
            Expect.notEqual map <| move offset map
      ]
    ]

zoomToTest : Test
zoomToTest =
  describe "Map zoom to tests"
    [ describe "Fuzz test zoom to functionality" 
      [ fuzz3 zoomLevel offset latLng
        "Zooming to an offset should result in that offset's latitude and longitude being at the same offset"
        <| \zoomLevel offset center ->
          let
            map = { tileServer = "", zoom = 15, center = center, width = 5000, height = 5000, tileSize = 256 }
          in
            Expects.equalLatLng 0.0001
              (Screen.offsetToLatLng map offset)
              (Screen.offsetToLatLng (zoomTo zoomLevel offset map) offset)
      ]
   ]
