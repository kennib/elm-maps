module Maps.LatLng exposing
  ( LatLng
  , sydney
  )

type alias LatLng =
  { lat : Float
  , lng : Float
  }

sydney : LatLng
sydney = { lat = -33.865143, lng = 151.209900 }
