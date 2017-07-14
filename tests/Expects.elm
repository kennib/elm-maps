module Expects exposing
  ( equalMap
  , equalLatLng
  , equalOffsets
  )

import Expect exposing (Expectation)

import Maps.Internal.Utils exposing (wrap)
import Maps.Internal.Map exposing (Map)
import Maps.Internal.LatLng exposing (LatLng)
import Maps.Internal.Screen exposing (Offset)

equalMap : Float -> Map -> Map -> Expectation
equalMap delta mapA mapB =
  Expect.all
    [ \(mapA, mapB) -> Expect.equal mapA.tileServer mapB.tileServer
    , \(mapA, mapB) -> Expect.equal mapA.zoom mapB.zoom
    , \(mapA, mapB) -> equalLatLng delta mapA.center mapB.center
    , \(mapA, mapB) -> Expect.equal mapA.width mapB.width
    , \(mapA, mapB) -> Expect.equal mapA.height mapB.height
    , \(mapA, mapB) -> Expect.equal mapA.tileSize mapB.tileSize
    ]
    (mapA, mapB)

equalLatLng : Float -> LatLng -> LatLng -> Expectation
equalLatLng delta latLngA latLngB =
  let
    latDiff = abs <| latLngA.lat - latLngB.lat
    lngDiff = abs <| (latLngA.lng |> wrap -180 180) - (latLngB.lng |> wrap -180 180)
  in
    Expect.true
    ("Expected the two offsets to be within "++toString delta++"\n"
    ++ toString latLngA ++ "\n" ++ toString latLngB)
    <| latDiff < delta && lngDiff < delta

equalOffsets : Float -> Offset -> Offset -> Expectation
equalOffsets delta offsetA offsetB =
  let
    xDiff = abs <| offsetA.x - offsetB.x
    yDiff = abs <| offsetA.y - offsetB.y
  in
    Expect.true
    ("Expected the two offsets to be within "++toString delta++"\n"
    ++ toString offsetA ++ "\n" ++ toString offsetB)
    <| xDiff < delta && yDiff < delta
