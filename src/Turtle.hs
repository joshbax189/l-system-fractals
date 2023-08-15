module Turtle where

import Control.Monad.State

-- |Symbolic representation of the movements the cursor can make.
data TurtleAction =
  Draw Double -- ^Move the cursor and draw between the new position and original.
              -- This is easier to implement that drawing using the previous position.
  | Move Double -- ^Move the cursor the given distance along the current angle.
  | Turn Double -- ^Increment cursor angle clockwise (radians).
  deriving (Eq, Show)

data TurtleLoc = TurtleLoc {
  coordX :: Double
  , coordY :: Double
  , angle :: Double
  }

-- |Compute new position when the argument to 'Move' (and 'Draw') means the
-- straight-line distance.
moveStraight :: Double -> TurtleLoc -> TurtleLoc
moveStraight n loc = loc {coordX = newX, coordY = newY}
  where ang = angle loc
        newX = coordX(loc) + n * sin(ang)
        newY = coordY(loc) + n * cos(ang)

turnClockwise :: Double -> TurtleLoc -> TurtleLoc
turnClockwise n loc = loc { angle = angle(loc) + n }

-- |State monad with the current 'TurtleLoc'.
-- A sequence of 'TurtleAction's is evaluated by producing some output and
-- modifying the current location.
type TurtleState a = State TurtleLoc a
-- State is equivalent to StateT TurtleLoc Identity a

-- |Given an interpretation of a 'TurtleAction' producing a 'b', allow
-- converting a sequence of actions to 'b's by assuming the cursor starts at
-- `{ coordX=0, coordY=0, angle=0 }`.
runTurtle :: (TurtleAction -> TurtleState b)
             -- ^The interpretation function
             -> [TurtleAction]
             -> ([b], TurtleLoc)
             -- ^Output and final 'TurtleLoc'
runTurtle action = \st -> runState (mapM action st) (TurtleLoc 0 0 0)
