module Maps.Internal.Geometry exposing
  ( Geometry(..)
  , view
  )

import GeoJson exposing (Geometry(..))

import Html exposing (Html)
import Html.Attributes as Attr

import Svg
import Svg.Attributes as Attr
import Svg.Path exposing (pathToString, subpath, startAt, lineToMany, emptySubpath, open, closed)

import Maps.Internal.Screen as Screen exposing (ZoomLevel)
import Maps.Internal.LatLng as LatLng exposing (LatLng)

type alias Map a = {a | tileSize : Float, zoom : ZoomLevel, width : Float, height : Float, center : LatLng}

type Geometry msg
  = DefaultGeometry GeoJson.Geometry


view : Map a -> Geometry msg -> Html msg
view map geometry =
  case geometry of
    DefaultGeometry geometry ->
      geometryView map geometry

geometryView : Map a -> GeoJson.Geometry -> Html msg
geometryView map geometry =
  case geometry of
    Point position ->
      Svg.g [] []
    MultiPoint positions -> 
      Svg.g
        []
        <| (positions |> List.map Point |> List.map (geometryView map))
    LineString line ->
      Svg.path
        [ line
          |> List.map (positionToOffset map)
          |> linePath
          |> \subpath -> pathToString [ subpath ]
          |> Attr.d
        , Attr.fill "none"
        , Attr.stroke color
        , Attr.strokeWidth strokeWidth
        ]
        []
    MultiLineString lines ->
      Svg.g
        []
        <| (lines |> List.map LineString |> List.map (geometryView map))
    Polygon polygon ->
      Svg.path
        [ polygon
          |> List.map (List.map (positionToOffset map))
          |> List.map polygonPath
          |> pathToString
          |> Attr.d
        , Attr.fill (color++"66")
        , Attr.stroke color
        , Attr.strokeWidth strokeWidth
        ]
        []
    MultiPolygon polygons ->
      Svg.g
        []
        <| (polygons |> List.map Polygon |> List.map (geometryView map))
    GeometryCollection geometries ->
      Svg.g
        []
        <| (geometries |> List.map (geometryView map))

positionToOffset : Map a -> GeoJson.Position -> (Float, Float) 
positionToOffset map (lng, lat, _) =
  let
    offset = Screen.offsetFromLatLng map <| LatLng lat lng
  in
    (offset.x, offset.y)

linePath ps =
  case ps of
    [] ->
      emptySubpath
    x :: xs ->
      subpath (startAt x) open [ lineToMany xs ]

polygonPath ps =
  case ps of
    [] ->
      emptySubpath
    x :: xs ->
      subpath (startAt x) closed [ lineToMany xs ]

color : String
color = "#5555dd"

strokeWidth : String
strokeWidth = "2px"
