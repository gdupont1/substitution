{-|
Module      : Substitution.Rule
Description : module for handling substitution rule
Copyrigh    : (c) Guillaume Dupont, 2021
License     : GPLv3
Maintainer  : guillaume.dupont55@gmail.com

A substitution rule is a pair (pattern => template). The idea is that,
whenever the left-hand side of a rule is matched, then whatever is matched
is replaced by the template, which holes are filled-in with the captured
field of the pattern.
-}
module Substitution.Rule (
        Rule(..), Rules, collisions, compile, rerrstring, check
    ) where

import Substitution.Pattern
import Substitution.Automaton
import Substitution.Template
import Data.List (partition)

-- | Rule, matching a pattern and yielding a template
data Rule l t = Rule {
        rule_pattern :: Pattern l,
        rule_automaton :: Automaton l,
        rule_template :: Template t
    }

instance (Show l, Show t) => Show (Rule l t) where
  show (Rule patt _ temp)  = "@" ++ (show patt) ++ "@ => @" ++ (concat $ map show temp) ++ "@" 

instance (Eq l, Eq t) => Eq (Rule l t) where
  r == r' = (rule_automaton r == rule_automaton r') && (rule_template r == rule_template r')

-- | A list of rules, as a type synonym (for convenience and readability)
type Rules l t = [Rule l t]

-- | Find collisions in a list of rules. A collision is a set of rules that
-- are essentially identical
collisions :: (Eq l, Eq t) => Rules l t -> [Rules l t]
collisions table =
    filter ((> 1) . length) $ takeAppart table
    where takeAppart [] = []
          takeAppart (t:ts) =
              let (same,others) = partition (== t) ts in
                  (t:same):(takeAppart others)

-- | Errors relative to rule coherence
data RuleError =
    InvalidCaptureNumber Int Int

-- | Transform an error to a human-readable string
rerrstring :: RuleError -> String
rerrstring (InvalidCaptureNumber tn pn) =
    "Given capture group number in template (" ++ show tn ++ ") is incompatible with provided pattern (" ++ show pn ++ " groups)"

-- | Check coherence for a rule. In particular, check if the capture
-- groups that are being accessed are allowed by the pattern.
check :: Rule l t -> Maybe RuleError
check rl =
    if refs > capt
        then Just $ InvalidCaptureNumber refs capt
        else Nothing
    where capt = countCaptures $ rule_pattern rl
          refs = maxRef $ rule_template rl

-- | Compile a rule. This basically means to derive the automaton
-- from the pattern of the lhs of the rule and store it inside the
-- rule.
compile :: Monad m => Rule l t -> m (Rule l t)
compile rule = do
    aut <- derive $ rule_pattern rule
    return $ rule { rule_automaton = aut }



