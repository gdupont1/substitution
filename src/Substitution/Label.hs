{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, UndecidableInstances #-}
{-|
Module      : Substitution.Label
Description : Label system for automata
Copyright   : (c) Guillaume Dupont, 2021
License     : GPLv3
Maintainer  : guillaume.dupont55@gmail.com

As a matter of genericity, labels are not provided via types but
rather via typeclasses.
The typeclass agregate everything required from a label for the
automaton to oeprate.
-}
module Substitution.Label (
        Matcher(..), Label(..)
    ) where

infix 1 //>

-- | Typeclass to model any type that can be "matched" against a given
-- other type. This allows to provide more general labels to automata,
-- instead of having strict equality rule (e.g. `.` or `\s` in regex).
--
-- Note that order is required during automaton evaluation. It basically
-- specifies which edge to take when several labels match.
class Ord l => Matcher l a | l -> a where
  (//>) :: l -> a -> Bool       -- ^ Check the given label against a token of type `a`
  isConsuming :: l -> Bool      -- ^ Determines if given label actually consumes a token when matched (by default, yes)
  isConsuming _ = True

-- | An automaton label. This is mostly a wrapper type, consisting of
-- either an actual label, or special "commands" to begin/end capture
-- or to skip a step (tau-transition in an automaton).
data Label l =
      Skip              -- ^ The label that always match, and is the lowest in the label poset
    | BeginCapture      -- ^ The label that always match, and initiate a capture group
    | EndCapture        -- ^ The label that always match, and finishes a capture group
    | Label l           -- ^ Any other specific label (provided by the user)

instance (Eq l) => Eq (Label l) where
  Skip == Skip = True
  BeginCapture == BeginCapture = True
  EndCapture == EndCapture = True
  (Label l1) == (Label l2) = (l1 == l2)

instance (Ord l) => Ord (Label l) where
  compare (Label l1) (Label l2) = compare l1 l2
  compare _          (Label l2) = LT
  compare (Label l1) _          = GT
  compare BeginCapture BeginCapture = EQ
  compare BeginCapture _            = GT
  compare _            BeginCapture = LT
  compare EndCapture   EndCapture   = EQ
  compare EndCapture   _            = GT
  compare _            EndCapture   = LT
  compare _            _            = EQ

instance (Show l) => Show (Label l) where
  show (Label l) = show l
  show BeginCapture = "@("
  show EndCapture = ")@"
  show Skip = ">>>"

-- | Matcher instance for @Label l@ allows to nest labels
-- and achieve richer matching behaviours
instance (Ord l, Matcher l a) => Matcher (Label l) a where
  -- Skip never matches in code, because it is treated differently.
  Skip         //> _ = False
  BeginCapture //> _ = True
  EndCapture   //> _ = True
  (Label l)    //> a = l //> a
  isConsuming (Label l) = isConsuming l
  isConsuming _         = False





