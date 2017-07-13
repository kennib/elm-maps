module Example exposing (..)

import Html exposing (program)

import Maps

main = program
  { init = (Maps.defaultModel, Cmd.none)
  , subscriptions = Maps.subscriptions
  , update = Maps.update
  , view = Maps.view
  }
