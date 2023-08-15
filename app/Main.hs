{-# LANGUAGE OverloadedStrings #-}

module Main where

import Graphics.Svg

import Draw (SVGConfig(..), toSVG)
import Systems
import LSystem (runSteps)

main :: IO ()
main = do
  let n = 2
      diag = runSteps n sierpinsky
      config = SVGConfig { svgAttributes = [Width_ <<- "3000", Height_ <<- "2000"]
                         , groupAttributes = [Transform_ <<- "translate(100,100)"] }
  -- TODO take user input
  writeFile "koch.svg" (show $ toSVG config diag)
