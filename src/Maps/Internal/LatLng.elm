module Maps.Internal.LatLng exposing
  ( LatLng
  , sydney
  )

{-| This module defines the LatLng type for the Maps library.
The LatLng type is used for defining geographical points.

# Definition
@docs LatLng

# Examples
@docs sydney
-}


{-| The LatLng type consists of a [latitude](https://en.wikipedia.org/wiki/Latitude) and [longitude](https://en.wikipedia.org/wiki/Longitude).
-}
type alias LatLng =
  { lat : Float
  , lng : Float
  }

{-| An example LatLng for Sydney.

    sydney = { lat = -33.865143, lng = 151.209900 }
-}
sydney : LatLng
sydney = { lat = -33.865143, lng = 151.209900 }
