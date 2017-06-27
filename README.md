# Elm Maps

An elm library for interactive maps.

# Examples
It's very simple to get create a map with this library

```elm
    import Maps
    import Html exposing (program)

    main = program <| Maps.map Maps.defaultOptions
```

Its a bit trickier when we try and merge events and state with other programs.
See the following examples:

 * Map | [source code](https://github.com/kennib/elm-maps/blob/master/examples/Example.elm) | [live](https://kennib.github.io/elm-maps/examples/Example)
 * Geocode Search | [source code](https://github.com/kennib/elm-maps/blob/master/examples/Search.elm) | [live](https://kennib.github.io/elm-maps/examples/Search)

# Documentation
See the [Elm docs](http://package.elm-lang.org/packages/kennib/elm-maps/latest/Maps) for this library.
