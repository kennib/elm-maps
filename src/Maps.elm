module Maps exposing
  ( Msg
  , Model
  , updateMap
  , updateMarkers
  , defaultModel
  , subscriptions
  , update
  , view
  , mapView
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
@docs mapView

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
type alias Msg msg = Maps.Msg msg
{-| -}
type alias Model msg = OpaqueTypes.Model msg

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
updateMap : (Map -> Map) -> Model msg -> Model msg
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
updateMarkers : (List (Marker msg) -> List (Marker msg)) -> Model msg -> Model msg
updateMarkers update = opaqueModel <| Maps.updateMarkers update

{-| The default model is a map zoomed into Sydney, Australia with no markers.
-}
defaultModel : Model msg
defaultModel = Model <| Maps.defaultModel

{-| -}
subscriptions : Model msg -> Sub (Msg msg)
subscriptions (Model model) = Maps.subscriptions model

{-| -}
update : Msg msg -> Model msg -> (Model msg, Cmd (Msg msg))
update msg (Model model) =
  Maps.update msg model
  |> Tuple.mapFirst Model

{-| -}
view : Model msg -> Html (Msg msg)
view (Model model) = Maps.view model

{-| Transforms the Maps HTML view into an arbitrary HTML view.
Requires a function that can transform `Maps.Msg`s into `msg`s.

```
import Html
import Html.Event exposing (onClick)
import Maps

type MyMsg = Click | MapsMsg Maps.Msg

...

view msg model =
  Html.div
    []
    [ Maps.view model.map |> Maps.mapView MapsMsg
    , Html.button [ onClick Click ] [ Html.text "Click!" ]
    ]
```
-}
mapView : (Msg msg -> msg) -> Html (Msg msg) -> Html msg
mapView = Maps.mapView
