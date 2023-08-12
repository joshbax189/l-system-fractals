{-# LANGUAGE OverloadedStrings #-}

module Draw where

import Graphics.Svg
import Control.Monad.State
import Control.Monad.Identity
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (catMaybes)

-- import Data.Text.Lazy (Text)
import qualified Data.Text as Text

svg :: Element -> Element
svg content =
     doctype
  <> with (svg11_ content) [Version_ <<- "1.1", Width_ <<- "3000", Height_ <<- "2000"]

contents :: Element
contents =
     rect_   [ Width_ <<- "100%", Height_ <<- "100%", "red" ->> Fill_]
  <> circle_ [ Cx_ <<- "150", Cy_ <<- "100", R_ <<- "80", Fill_ <<- "green"]
  <> text_   [ X_ <<- "150", Y_ <<- "125", Font_size_ <<- "60"
             , Text_anchor_ <<- "middle", Fill_ <<- "white"] "SVG"

toSVG :: [TurtleAction] -> Element
toSVG actions = svg $ g_ [Transform_ <<- "translate(100,100)"] $ mconcat $ convert actions

-- actions: draw, move, turn n
data TurtleAction =
  Draw Double
  | Move Double
  | Turn Double
  deriving (Eq, Show)

showText :: Show a => a -> Text.Text
showText = Text.pack . show

--       x',y'
--     /  | o
-- x,y---|
-- x' = x+h*sin(ang)
-- y' = y+h*cos(ang)
action :: TurtleAction -> TurtleState (Maybe Element)
action (Draw n) = do
  locX <- coordX <$> get 
  locY <- coordY <$> get
  ang <- gets angle
  let locX' = locX + n * sin(ang)
      locY' = locY + n * cos(ang)
  modify (\s -> s {coordX = locX', coordY = locY'})
  return $ Just $ line_ [X1_ <<- showText locX, Y1_ <<- showText locY, X2_ <<- showText locX', Y2_ <<- showText locY', Stroke_width_ <<- "2px", Stroke_ <<- "black"]
action (Move n) = do
  locX <- gets coordX
  locY <- gets coordY
  ang <- gets angle
  let locX' = locX + n * sin(ang)
      locY' = locY + n * cos(ang)
  modify (\s -> s {coordX = locX', coordY = locY'})
  return Nothing
action (Turn n) = do
  modify (\s -> s { angle = angle s + n })
  return Nothing

data TurtleLoc = TurtleLoc {
  coordX :: Double
  , coordY :: Double
  , angle :: Double
  }

type TurtleState a = StateT TurtleLoc Identity a

convert :: [TurtleAction] -> [Element]
convert st = catMaybes $ fst $ runState (mapM action st) (TurtleLoc 0 0 0)
