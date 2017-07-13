module Maps.Internal.OpaqueTypes exposing
  ( Model(..)
  , Map(..)
  , opaqueModel
  , opaqueMap
  , transparentMap
  )

import Maps.Internal.Map as Map
import Maps.Internal.Maps as Maps

type Model = Model Maps.Model
type Map = Map Map.Map

opaqueModel : (Maps.Model -> Maps.Model) -> (Model -> Model)
opaqueModel update (Model model) = Model <| update model

opaqueMap : (Map.Map -> Map.Map) -> (Map -> Map)
opaqueMap update (Map map) = Map <| update map

transparentMap : (Map -> Map) -> (Map.Map -> Map.Map) 
transparentMap update map = 
  let
    (Map updatedMap) = update <| Map map
  in
    updatedMap
