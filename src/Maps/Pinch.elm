module Maps.Pinch exposing
  ( Pinch
  , start
  , pinch
  , startEnd
  )

import Maps.Screen as Screen exposing (TwoFingers)

type Pinch
  = StartPinch TwoFingers
  | Pinch TwoFingers TwoFingers

start : TwoFingers -> Pinch
start = StartPinch

pinch : TwoFingers -> Pinch -> Pinch
pinch twoFingers state =
  case state of
    StartPinch start -> Pinch start twoFingers
    Pinch start end -> Pinch start twoFingers

startEnd : Pinch -> (TwoFingers, TwoFingers)
startEnd pinch =
  case pinch of
    StartPinch start -> (start, start)
    Pinch start end -> (start, end)
