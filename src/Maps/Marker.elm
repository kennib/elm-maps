module Maps.Marker exposing
  ( Marker
  , create
  , createCustom
  )

{-| Markers are for displaying geographic locations on the map.

@docs Marker

# Create a marker
@docs create
@docs createCustom
-}

import Html exposing (Html)

import Maps.Geo
import Maps.Internal.Marker as Marker exposing (Marker(..))

{-| There are currently two types of marker:

 * A default marker
 * A custom HTML marker
-}
type alias Marker msg = Marker.Marker msg


{-| Create a default style of marker at the given latitude/longitude.

    import Maps.Marker
    import Maps.Geo

    newYork = Maps.Geo.latLng 40.730610 -73.935242
    newYorkMarker = Maps.Marker.create newYork

-}
create : Maps.Geo.LatLng -> Marker msg
create = Marker.DefaultMarker

{-| Create a custom HTML marker at the given latitude/longitude.
-}
createCustom : Html msg -> Maps.Geo.LatLng -> Marker msg
createCustom = Marker.CustomMarker
