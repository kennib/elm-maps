module Maps.Bounds exposing
  ( Bounds(..)
  , zoom
  , center
  )

import Maps.LatLng as LatLng exposing (LatLng)
import Maps.Screen as Screen exposing (ZoomLevel)
import Maps.Utils exposing (wrap)

type Bounds
  = Bounds
    { northEast : LatLng
    , southWest : LatLng
    }
  | Centered
    { zoom : Float
    , center : LatLng
    }

zoom : Float -> Float -> Float -> Bounds -> ZoomLevel
zoom tileSize mapWidth mapHeight bounds =
  case bounds of
    Bounds bounds ->
      let
        (ne, sw) = (bounds.northEast, bounds.southWest)
        -- The following assumes a Mercator projection
        -- See https://en.wikipedia.org/wiki/Mercator_projection#Alternative_expressions for details
        latY lat = sin (lat * pi / 180)
        radX2 lat = (logBase e ((1 + latY lat) / (1 - latY lat))) / 2
        latRad lat = (max (-pi) <| min (radX2 lat) pi) / 2
        latFraction = (latRad ne.lat) - (latRad sw.lat)
        lngFraction = ((ne.lng - sw.lng) |> wrap 0 360) / 360
        zoom mapSize tileSize frac = logBase 2 (mapSize / tileSize / frac)
      in
        min
          (zoom mapWidth tileSize lngFraction)
          (zoom mapHeight tileSize latFraction)
    Centered bounds ->
      bounds.zoom

center : Bounds -> LatLng
center bounds =
  case bounds of
    Bounds bounds ->
      { lat = (bounds.northEast.lat + bounds.southWest.lat) / 2
      , lng = (bounds.northEast.lng + bounds.southWest.lng) / 2
      }
    Centered bounds ->
      bounds.center
