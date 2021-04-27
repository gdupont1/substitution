{-# LANGUAGE MultiParamTypeClasses #-}
{-|
Module      : Substitution
Description : main module for performing substitutions
Copyrigh    : (c) Guillaume Dupont, 2021
License     : GPLv3
Maintainer  : guillaume.dupont55@gmail.com

This module define substitution of a list of tokens with
a set of rules.
-}
module Substitution (
        RuleMatch,
        match, lookupPattern,
        substituteOnce, substitute
    ) where

import Substitution.Label
import Substitution.Automaton
import Substitution.Template
import Substitution.Rule
import qualified Substitution.Automaton.Reader as R

-- | A match for a rule
data RuleMatch l t = RuleMatch {
    rule :: Rule l t,
    matched :: [t],
    remaining :: [t],
    captures :: [[t]]
}

-- | Try to match the given rule to the given stream of tokens.
-- Yields `Nothing` if it does not match, or the corresponding
-- `RuleMatch`.
match :: Matcher l t => Rule l t -> [t] -> Maybe (RuleMatch l t)
match rl formula =
    let rd = R.stepuntilvalidating $ R.reader (rule_automaton rl) formula in
        if R.validatingState rd then
            Just $ RuleMatch {
                rule      = rl,
                matched   = R.readTokens rd,
                remaining = R.remainingTokens rd,
                captures  = R.captures rd
            }
        else Nothing

-- | Find the first rule that match the given string of tokens among the list
-- of rules as parameter.
lookupPattern :: Matcher l t => Rules l t -> [t] -> Maybe (RuleMatch l t)
lookupPattern [] input = Nothing
lookupPattern (x:xs) input =
    case match x input of
      Nothing -> lookupPattern xs input
      justx   -> justx

-- | Try to match a stream a tokens at any position.
-- Returns `Nothing` if no rule could be matched, or the new
-- tokens if it has been changed.
substituteOnce :: Matcher l t => Rules l t -> [t] -> Maybe [t]
substituteOnce table [] = Nothing
substituteOnce table input@(t:ts) =
    case lookupPattern table input of
      Nothing -> (:) <$> t <*> substituteOnce table ts
      Just rm -> 
          let filledTemplate = fill (matched rm) (captures rm) (rule_template $ rule rm) in
              Just $ filledTemplate ++ (remaining rm)

-- | Substitute a stream of tokens given a set of rules as much as
-- possible.
-- This function stops when no substitution is possible anymore.
-- Note that if the result of a substitutiton can be matched by a rule, this
-- may result in an infinite loop!
substitute :: Matcher l t => Rules l t -> [t] -> [t]
substitute table input =
    subst input
    where subst input =
            case substituteOnce table input of
              Nothing -> input
              Just x  -> subst x








