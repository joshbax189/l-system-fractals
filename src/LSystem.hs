module LSystem where

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List as List
import Data.Maybe (fromMaybe)

type Axiom a = [a]
data Rule a = Rule {
  tgt :: a
  , res :: [a]
  }

type Sys a = [Rule a]

ruleApply :: (Eq a) => Rule a -> a -> [a]
ruleApply (Rule c s) c'
  | c == c' = s
  | otherwise = [c']

apply :: (Eq a) => Sys a -> Axiom a -> [a]
-- apply sys axiom = foldl (\acc r -> foldMap (ruleApply r) acc) axiom sys
apply sys axiom = foldMap (applyAnyRule sys) axiom
  where applyAnyRule sys c = fromMaybe [c] $ res <$> List.find ((c==) . tgt) sys 

-- should be able to just do
-- take 3 $ iterate (apply cantor) "A" 
