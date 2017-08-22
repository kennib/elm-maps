# Elm Maps

An elm library for interactive maps.

# Examples
It's very simple to get create a map with this library

```elm
    import Maps
    import Html exposing (program)

    main = program
		{ init = (Maps.defaultModel, Cmd.none)
		, subscriptions = Maps.subscriptions
		, update = Maps.update
		, view = Maps.view
		}
```

Its a bit trickier when we try and merge events and state with other programs.
See the following examples:

 * Map | [source code](https://github.com/kennib/elm-maps/blob/master/examples/Example.elm) | [live](https://kennib.github.io/elm-maps/examples/Example)
 * Fullscreen Map | [source code](https://github.com/kennib/elm-maps/blob/master/examples/Fullscreen.elm) | [live](https://kennib.github.io/elm-maps/examples/Fullscreen)
 * Map Markers | [source code](https://github.com/kennib/elm-maps/blob/master/examples/Markers.elm) | [live](https://kennib.github.io/elm-maps/examples/Markers)
 * Custom Markers | [source code](https://github.com/kennib/elm-maps/blob/master/examples/CustomMarkers.elm) | [live](https://kennib.github.io/elm-maps/examples/CustomMarkers)
 * Geocode Search | [source code](https://github.com/kennib/elm-maps/blob/master/examples/Search.elm) | [live](https://kennib.github.io/elm-maps/examples/Search)
 * Gecode Search Markers | [source code](https://github.com/kennib/elm-maps/blob/master/examples/SearchMarkers.elm) | [live](https://kennib.github.io/elm-maps/examples/SearchMarkers)

# Documentation
See the [Elm docs](http://package.elm-lang.org/packages/kennib/elm-maps/latest/Maps) for this library.
