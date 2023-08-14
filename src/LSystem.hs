module LSystem where

import qualified Data.List as List
import Data.Maybe (fromMaybe)

-- |The starting string of the L-System
type Axiom a = [a]

-- |Rules are context-free, replacing a single letter with a string.
data Rule a = Rule {
  -- |The letter to replace.
  tgt :: a
  -- |The string to replace it with.
  , res :: [a]
  }

-- |The collection of rules in the system. Assumes the axiom is given
-- separately. See 'apply'.
type Sys a = [Rule a]

-- TODO unused
-- An alternative definition of 'apply' is
-- apply sys axiom = foldl (\acc r -> foldMap (ruleApply r) acc) axiom sys
ruleApply :: (Eq a) => Rule a -> a -> [a]
ruleApply (Rule c s) c'
  | c == c' = s
  | otherwise = [c']

-- |Apply all rules in a system to an axiom.
-- Assumes system is deterministic and context-free.
--
-- > take 3 $ iterate (apply cantor) "A"
apply :: (Eq a) => Sys a -> Axiom a -> [a]
apply sys axiom = foldMap (applyAnyRule) axiom
  -- TODO the following is a bit obscure
  -- TODO this only works for deterministic systems, but this is not enforced
  where applyAnyRule c = fromMaybe [c] $ res <$> List.find ((c==) . tgt) sys
