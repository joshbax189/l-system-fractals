{-# LANGUAGE OverloadedStrings #-}

module Draw ( SVGConfig(..)
            , toSVG
            ) where

import Graphics.Svg
import Control.Monad.State
import Data.Maybe (catMaybes)
import qualified Data.Text as Text

import Turtle

data SVGConfig = SVGConfig { svgAttributes :: [Attribute]
                           , groupAttributes :: [Attribute]
                           }

-- |Wrap an SVG drawing in the necessary boilerplate.
svg :: SVGConfig -> Element -> Element
svg config content =
     doctype
  <> with (svg11_ content) ([Version_ <<- "1.1"] ++ (svgAttributes config))

-- Unused example

-- contents :: Element
-- contents =
--      rect_   [ Width_ <<- "100%", Height_ <<- "100%", "red" ->> Fill_]
--   <> circle_ [ Cx_ <<- "150", Cy_ <<- "100", R_ <<- "80", Fill_ <<- "green"]
--   <> text_   [ X_ <<- "150", Y_ <<- "125", Font_size_ <<- "60"
--              , Text_anchor_ <<- "middle", Fill_ <<- "white"] "SVG"

-- |Convert an action string to an SVG.
-- This renders the Turtle's path.
toSVG :: SVGConfig -> [TurtleAction] -> Element
toSVG config actions = svg config
                       $ g_ (groupAttributes config)
                       $ mconcat
                       $ convert actions

showText :: Show a => a -> Text.Text
showText = Text.pack . show

action :: TurtleAction -> TurtleState (Maybe Element)
action (Draw n) = do
  p1 <- get
  modify $ moveStraight n
  p2 <- get
  return $ Just $ line_ [X1_ <<- showText (coordX p1),
                         Y1_ <<- showText (coordY p1),
                         X2_ <<- showText (coordX p2),
                         Y2_ <<- showText (coordY p2),
                         Stroke_width_ <<- "2px",
                         Stroke_ <<- "black"]
action (Move n) = do
  modify $ moveStraight n
  return Nothing
action (Turn n) = do
  modify $ turnClockwise n
  return Nothing

convert :: [TurtleAction] -> [Element]
convert = catMaybes . fst . runTurtle action
