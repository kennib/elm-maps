module Tests.Tile exposing
  ( conversionsTest
  )

import Test exposing (..)

import Fuzzers exposing (..)
import Expects exposing (..)

import Maps.LatLng exposing (LatLng)
import Maps.Tile exposing (..)

conversionsTest : Test
conversionsTest =
  describe "Tile offset conversion tests"
    [ fuzz latLng "Converting a LatLng to and from a tile offset should cause no change in the LatLng"
      <| \loc ->
        let
          zoom = 15
          tile = fromLatLng zoom loc
          convertedLoc = toLatLng zoom tile.x tile.y
        in
          equalLatLng 0.0001
            loc
            convertedLoc
    ]
