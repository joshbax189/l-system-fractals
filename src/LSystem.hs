module LSystem ( Axiom
               , Rule(..)
               , LSystem(..)
               , runSteps
               ) where

import qualified Data.List as List
import Data.Maybe (fromMaybe)

-- |The starting string of the L-System
type Axiom a = [a]

-- |Rules are context-free, replacing a single letter with a string.
data Rule a = Rule { target :: a
                   -- ^The letter to replace.
                   , replacement :: [a]
                   -- ^The string to replace it with.
                   }

data LSystem a = LSystem { rules :: [Rule a]
                         , axiom :: Axiom a
                         }

-- |Apply all rules in a system to an axiom.
-- Assumes system is deterministic and context-free.
apply :: (Eq a) => [Rule a] -> Axiom a -> [a]
apply sys ax = foldMap (applyAnyRule) ax
  -- TODO the following is a bit obscure
  -- TODO this only works for deterministic systems, but this is not enforced
  where applyAnyRule c = fromMaybe [c] $ replacement <$> List.find ((c==) . target) sys

-- |Run the L-System for 'n' steps.
runSteps :: Eq a => Int -> LSystem a -> [a]
runSteps n sys = last $ take n $ iterate (apply $ rules sys) (axiom sys)
