module Main where

import Draw
import LSystem

cantor :: Sys TurtleAction
cantor =
  let a = Draw 5.0
      b = Move 5.0
  in [
    Rule a [a,b,a]
    , Rule b [b, b, b]
  ]

koch :: Sys TurtleAction
koch =
  let f = Draw 10.0
      min = Turn (pi/2)
      pl = Turn (-pi/2)
  in [
    Rule f [f, pl, f, min, f, min, f, pl, f]
     ]

sier :: Sys TurtleAction
sier =
  let f = Draw 10.0
      g = Draw 10.0
      pl = Turn (2 * pi / 3)
      min = Turn (-2 * pi /3)
  in [
    Rule f [f, min, g, pl, g, pl, g, min, f]
    , Rule g [g,g]
    ]

sierStart :: [TurtleAction]
sierStart =
  let f = Draw 10.0
      g = Draw 10.0
      min = Turn (-2 * pi /3)
  in [f, min, g, min, g]

main :: IO ()
main = do
  let n = 5
      diag = last $ take n $ iterate (apply sier) sierStart
  print $ toSVG diag
      
  
