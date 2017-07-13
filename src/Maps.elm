module Maps exposing
  ( Msg
  , Model
  , updateMap
  , updateMarkers
  , defaultModel
  , subscriptions
  , update
  , view
  )

{-| Functions for creating a maps program and maniuplating the maps model.

# Showing a map
You can use the functions below to display a map.

    import Maps
    import Html exposing (program)

    main = program
		{ init = (Maps.defaultModel, Cmd.none)
		, subscriptions = Maps.subscriptions
		, update = Maps.update
		, view = Maps.view
		}

@docs defaultModel
@docs subscriptions
@docs update
@docs view

# Update Model
@docs updateMap
@docs updateMarkers

# Types
The following types are [opaque](http://package.elm-lang.org/help/design-guidelines#keep-tags-and-record-constructors-secret).
Use the functions above to maniuplate and extract information from them.

@docs Msg
@docs Model
-}

import Html exposing (Html)

import Maps.Internal.OpaqueTypes as OpaqueTypes exposing (Model(..), opaqueModel, transparentMap)
import Maps.Marker exposing (Marker)
import Maps.Map exposing (Map)
import Maps.Internal.Maps as Maps

{-| -}
type alias Msg = Maps.Msg
{-| -}
type alias Model = OpaqueTypes.Model

{-| Change the map inside of a model.

For example, set the width/height of a map and zoom into Seoul, South Korea:

    import Maps.Geo
    import Maps.Map as Map

    let
      seoul = Maps.Geo.latLng 37.532600 127.024612
    in
      model
      |> updateMap (Map.setHeight 600)
      |> updateMap (Map.setWidth 1000)
      |> updateMap (Map.viewBounds <| Maps.Geo.centeredBounds 10 seoul)

See [Maps.Map](./Maps-Map) for documentation of the Map functions.
-}
updateMap : (Map -> Map) -> Model -> Model
updateMap update = opaqueModel <| Maps.updateMap <| transparentMap update

{-| Change the markers inside of the model

For example, add markers for some Sydney attractions and then another marker for the city center:

    import Maps.Geo
    import Maps.Marker as Marker

    let
      attractions =
        List.map (uncurry Maps.Geo.latLng)
          [ (-33.852324, 151.210819)
          , (-33.856872, 151.215239)
          , (-33.870397, 151.208835)
          ]
      sydney = Maps.Geo.latLng -33.865143 151.209900
    in
      model
      |> updateMarkers (\markers -> List.map Marker.create attractions ++ markers)
      |> updateMarkers ((::) (Marker.create sydney))

See [Maps.Marker](./Maps-Marker) for documentation of the Marker functions.
-}
updateMarkers : (List Marker -> List Marker) -> Model -> Model
updateMarkers update = opaqueModel <| Maps.updateMarkers update

{-| The default model is a map zoomed into Sydney, Australia with no markers.
-}
defaultModel : Model
defaultModel = Model <| Maps.defaultModel

{-| -}
subscriptions : Model -> Sub Msg
subscriptions (Model model) = Maps.subscriptions model

{-| -}
update : Msg -> Model -> (Model, Cmd Msg)
update msg (Model model) =
  Maps.update msg model
  |> Tuple.mapFirst Model

{-| -}
view : Model -> Html Msg
view (Model model) = Maps.view model
