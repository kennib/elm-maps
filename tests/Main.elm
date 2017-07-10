module Main exposing (..)

import Test exposing (..)

import Tests.Map
import Tests.Screen
import Tests.Tile

tests : Test
tests =
  describe "All tests"
    [ describe "Map tests"
      [ Tests.Map.moveTest
      , Tests.Map.zoomToTest
      ]
    , describe "Screen tests"
      [ Tests.Screen.conversionsTest
      ]
    , describe "Tile tests"
      [ Tests.Tile.conversionsTest
      ]
    ]
