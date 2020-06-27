module Robot
  ( Bearing (East, North, South, West),
    bearing,
    coordinates,
    mkRobot,
    move,
  )
where

import Control.Monad.Trans.State

data Bearing
  = North
  | East
  | South
  | West
  deriving (Eq, Show)

data Instruction
  = R
  | L
  | A
  deriving (Eq, Show)

type Coordinates = (Integer, Integer)

data Robot = Robot Bearing Coordinates deriving (Show, Eq)

instruction :: Char -> Maybe Instruction
instruction ch =
  case ch of
    'R' -> Just R
    'L' -> Just L
    'A' -> Just A
    _ -> Nothing

forward :: Bearing -> Bearing
forward b =
  case b of
    North -> East
    East -> South
    South -> West
    West -> North

backward :: Bearing -> Bearing
backward b =
  case b of
    North -> West
    West -> South
    South -> East
    East -> North

exececute :: Instruction -> State Robot Robot
exececute i = do
  _ <-
    modify
      ( \(Robot b (x, y)) ->
          case (b, i) of
            (_, R) -> Robot (forward b) (x, y)
            (_, L) -> Robot (backward b) (x, y)
            (North, A) -> Robot b (x, (y + 1))
            (East, A) -> Robot b ((x + 1), y)
            (South, A) -> Robot b (x, (y -1))
            (West, A) -> Robot b ((x -1), y)
      )
  get

bearing :: Robot -> Bearing
bearing (Robot b _) = b

coordinates :: Robot -> (Integer, Integer)
coordinates (Robot _ c) = c

mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot direction coordinates = Robot direction coordinates

move :: Robot -> String -> Robot
move robot instructions =
  case traverse instruction instructions of
    Nothing -> robot
    Just inst -> last $ evalState (traverse exececute inst) robot
