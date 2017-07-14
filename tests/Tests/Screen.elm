module Tests.Screen exposing
  ( conversionsTest
  )

import Test exposing (..)

import Fuzzers exposing (..)
import Expects exposing (..)

import Maps.Internal.LatLng exposing (LatLng)
import Maps.Internal.Utils exposing (wrap)
import Maps.Internal.Screen exposing (..)

conversionsTest : Test
conversionsTest =
  describe "Screen offset conversion tests"
    [ fuzz offset "Converting a screen offset to and from a LatLng should cause no change in the offset"
      <| \offset ->
        let
          map = { zoom = 18, width = 10000, height = 10000, center = {lat = 0, lng = 0}, tileSize = 256 }
          loc = offsetToLatLng map offset
          convertedOffset = offsetFromLatLng map loc
        in
          equalOffsets 0.5
            offset
            convertedOffset
    , fuzz latLng "Converting central offset to a LatLng should be the same as the center LatLng of the map"
      <| \center ->
        let
          map = { zoom = 18, width = 512, height = 512, center = center, tileSize = 256 }
          latLng = offsetToLatLng map {x = map.width/2, y = map.height/2}
        in
          equalLatLng 0.0001
            center
            latLng
    ]
