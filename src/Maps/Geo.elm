module Maps.Geo exposing
  ( LatLng
  , Bounds
  , latLng
  , bounds
  , centeredBounds
  )

{-| Geographic types and constructors.

# Latitude/Longitude
@docs LatLng

# Bounds
@docs bounds
@docs bounds
-}

import Maps.Internal.LatLng as LatLng
import Maps.Internal.Bounds as Bounds

{-| The LatLng type is a simple record containing latitude and longitude.

You can create a longitude in two equivalent ways:

    Maps.Geo.latLng 10 -80 == { lat = 10, lng = -80}
-}
type alias LatLng = LatLng.LatLng

{-| Create a LatLng.

For example:

    latLng 45 -175

-}
latLng : Float -> Float -> LatLng
latLng = LatLng.LatLng

{-| The Bounds type has several variations.
All of them can be used to calculate the position and zoom of a map.

## NorthEast/SouthWest bounds
@docs bounds

## Center and Zoom Level
@docs centerBounds
-}
type alias Bounds = Bounds.Bounds

{-| Create a Bounds using a northeast and southwest point.

For example, the bounds of Ecuador

    ecuador =
      bounds
        { northEast = latLng 1.4284875 -75.188794
        , southWest = latLng -5.0143511 -81.08498089999999
        }
-}
bounds : { northEast : LatLng, southWest : LatLng } -> Bounds
bounds = Bounds.Bounds

{-| Create a Bounds centered on a location with a given zoom level.

For example, zoomed into the streets of Baku, Azerbaijan:

    baku =
      centeredBounds
        14
        (latLng 40.409264 49.867092)
-}
centeredBounds : Float -> LatLng -> Bounds
centeredBounds zoom latLng =
  Bounds.Centered
    { zoom = zoom
    , center = latLng
    }
