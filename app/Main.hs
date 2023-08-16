{-# LANGUAGE OverloadedStrings #-}

module Main where

import Graphics.Svg

import Draw (SVGConfig(..), toSVG)
import Systems
import LSystem (LSystem, runSteps)
import Turtle (TurtleAction)

import Control.Applicative
import Options

data SystemOptions = Cantor | Quad | Koch | Sierpinsky
  deriving (Bounded, Enum, Show)

toSystem :: SystemOptions -> LSystem TurtleAction
toSystem Cantor = cantor
toSystem Quad = quad
toSystem Koch = kochSnowflake
toSystem Sierpinsky = sierpinsky

data MainOptions = MainOptions
  { optIterations :: Int
  , optName :: SystemOptions
  , optFile :: String
  }

instance Options MainOptions where
  defineOptions = pure MainOptions
    <*> defineOption optionType_int (\o -> o
          { optionShortFlags = "i"
          , optionDefault = 3
          , optionDescription = "Number of iterations to run."
          })
    <*> defineOption (optionType_enum "String") (\o -> o
          { optionLongFlags = ["name"]
          , optionDefault = Sierpinsky
          , optionDescription = "Name of system to run. Pass empty option to see choices"
          })
    <*> defineOption optionType_string (\o -> o
          { optionShortFlags = "f"
          , optionDefault = ""
          , optionDescription = "Filename for output. If none given, prints to stdout"
          })

main :: IO ()
main = runCommand $ \opts _ -> do
  let n = optIterations opts
      sysName = optName opts
      sys = toSystem sysName
      filename = optFile opts
      config = SVGConfig { svgAttributes = [Width_ <<- "3000", Height_ <<- "2000"]
                         , groupAttributes = [Transform_ <<- "translate(100,100)"] }

  putStrLn $ "Running " ++ (show sysName) ++ " for " ++ (show n) ++ " steps"

  let diagram = runSteps n sys
      result = toSVG config diagram

  if (filename == "")
    then print result
    else writeFile filename $ show result
