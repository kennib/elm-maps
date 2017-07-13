module Maps.Marker exposing
  ( Marker
  , create
  )

{-| Markers are for displaying geographic locations on the map.

@docs Marker

# Create a marker
@docs create

-}

import Maps.Geo
import Maps.Internal.Marker as Marker exposing (Marker(..))

{-| There is currently one type of marker:

 * A default marker
-}
type alias Marker = Marker.Marker


{-| Create a default style of marker at the given latitude/longitude.

    import Maps.Marker
    import Maps.Geo

    newYork = Maps.Geo.latLng 40.730610 -73.935242
    newYorkMarker = Maps.Marker.create newYork

-}
create : Maps.Geo.LatLng -> Marker
create = Marker.DefaultMarker
