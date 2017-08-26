module Maps.Geometry exposing
  ( Geometry
  , create
  )

{-| Geometry is for displaying geographic points, lines and polygons on the map.

@docs Marker

# Create geometry 
@docs create
-}

import GeoJson
import Html exposing (Html)

import Maps.Internal.Geometry as Geometry exposing (Geometry(..))

{-| There is currently one type of geometry:

 * Default geometry
-}
type alias Geometry msg = Geometry.Geometry msg


{-| Create a default style of marker at the given latitude/longitude.

    import Maps.Marker
    import Maps.Geo

    newYork = Maps.Geo.latLng 40.730610 -73.935242
    newYorkMarker = Maps.Marker.create newYork

-}
create : GeoJson.Geometry -> Geometry msg
create = Geometry.DefaultGeometry
