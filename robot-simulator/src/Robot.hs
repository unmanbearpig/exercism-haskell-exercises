module Robot
    ( Bearing(East,North,South,West)
    , bearing
    , coordinates
    , mkRobot
    , simulate
    , turnLeft
    , turnRight
    ) where

import Data.List

data Bearing = North
             | East
             | South
             | West
             deriving (Eq, Show, Enum)

type Coordinates = (Integer, Integer)

data Robot = Robot { bearing :: Bearing, coordinates :: Coordinates }

data Command = TurnRight | TurnLeft | Advance

parseCommand :: Char -> Command
parseCommand c = case c of
  'A' -> Advance
  'R' -> TurnRight
  'L' -> TurnLeft
  _   -> error "invalid command"

mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot = Robot

simulate :: Robot -> String -> Robot
simulate r = foldl executeCommand r . map parseCommand

executeCommand :: Robot -> Command -> Robot
executeCommand r c = case c of
  TurnRight -> r { bearing = turnRight $ bearing r }
  TurnLeft  -> r { bearing = turnLeft $ bearing r }
  Advance   -> r { coordinates = advanceCoordinates (bearing r) (coordinates r) }

advanceCoordinates :: Bearing -> Coordinates -> Coordinates
advanceCoordinates b (x, y) = case b of
  North -> (x, y+1)
  South -> (x, y-1)
  East  -> (x+1, y)
  West  -> (x-1, y)

directions = cycle [North, West, South, East]

turnLeft :: Bearing -> Bearing
turnLeft North = West
turnLeft b     = pred b

turnRight :: Bearing -> Bearing
turnRight West = North
turnRight b    = succ b
