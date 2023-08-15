module Main where

import Draw
import LSystem
import Turtle

-- Iteratively produce the Cantor set.
-- I.e. the kth iteration is the result of removing the middle third of each line
-- in the k-1th iteration.
cantor :: LSystem TurtleAction
cantor =
  let a = Draw 5.0
      b = Move 5.0
  in LSystem [
    Rule a [a,b,a]
    , Rule b [b, b, b]
  ]
  [a]

-- the quadratic type-1 curve
quad :: LSystem TurtleAction
quad =
  let f = Draw 10.0
      min = Turn (pi/2)
      pl = Turn (-pi/2)
  in LSystem [
    Rule f [f, pl, f, min, f, min, f, pl, f]
     ]
     [f]

-- Actual koch snowflake
kochSnowflake :: LSystem TurtleAction
kochSnowflake =
  let f = Draw 10.0
      o = Turn (-pi/3)
      t = Turn (2 * pi/3)
  in LSystem [
    Rule f [f, o, f, t, f, o, f]
    ]
  [f, t, f, t, f]

sierpinsky :: LSystem TurtleAction
sierpinsky =
  let f = Draw 10.0
      g = Draw 10.0
      pl = Turn (2 * pi / 3)
      min = Turn (-2 * pi /3)
  in LSystem [
    Rule f [f, min, g, pl, g, pl, g, min, f]
    , Rule g [g,g]
    ]
  [f, min, g, min, g]

main :: IO ()
main = do
  let n = 5
      diag = runSteps n sierpinsky
      config = SVGConfig { svgAttributes = [Width_ <<- "3000", Height_ <<- "2000"]
                         , groupAttributes = [Transform_ <<- "translate(100,100)"] }
  -- TODO take user input
  writeFile "koch.svg" (show $ toSVG config diag)
