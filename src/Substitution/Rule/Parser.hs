{-|
Module      : Substitution.Rule.Parser
Description : module for handling substitution rule
Copyrigh    : (c) Guillaume Dupont, 2021
License     : GPLv3
Maintainer  : guillaume.dupont55@gmail.com

A generic, modular parser for substitution rules.

A rule is textually represented by:
```
<begin delim> <pattern> <end delim> <arrow> <begin delim> <template> <end delim>
```

The parser uses a pattern parser and a template parser, and manages to parse
a line into a rule, propagating and wrapping any error encountered.
-}
module Substitution.Rule.Parser (
        ParseError(..), ParserConfig(..), defaultConfig,
        ParserResult(..),
        errstring,
        parseLine, parseLines, process,
        removeEmpty
    ) where

import Substitution.Internal.Util
import Substitution.Rule
import Substitution.Pattern
import Substitution.Template
import Substitution.Automaton (emptyAutomaton)
import qualified Substitution.Pattern.Parser as PP
import qualified Substitution.Template.Parser as TP
import Control.Monad (foldM, forM)

-- | A parsing error
data ParseError =
      IllFormedRule                 -- ^ Rule is ill-formed (missing delimiter, typically)
    | EmptyRule                     -- ^ The rule does not contain an arrow (can be used to extract 'comments' or other non-ules)
    | Unescapable                   -- ^ Escape character found but no character to escape (e.g. end of line reached)
    | TemplateError TP.ParseError   -- ^ Error while parsing template (rule RHS)
    | PatternError  PP.ParseError   -- ^ Error while parsing pattern (rule LHS)
    | AtLine ParseError Int         -- ^ An error with a location
    deriving Show

-- | Transform an error to a human-readable string
errstring :: ParseError -> String
errstring IllFormedRule      = "Rule is ill-formed (probably missing a delimiter)"
errstring EmptyRule          = "Rule is empty"
errstring Unescapable        = "Escape character met at end of stream"
errstring (TemplateError pe) = "Error in template: " ++ (TP.errstring pe)
errstring (PatternError pe)  = "Error in pattern: " ++ (PP.errstring pe)
errstring (AtLine p l)       = "line " ++ (show l) ++ ": " ++ (errstring p)

-- | Parser configuration
data ParserConfig = ParserConfig {
        beginDelim :: String,       -- ^ Delimiter at the beginning of a side
        endDelim   :: String,       -- ^ Delimiter at the end of a side
        arrow      :: String,       -- ^ Arrow between LHS and RHS
        escape     :: Char          -- ^ Escape character
    }

-- | Default configuration. Rules are of the form:
-- ```
-- @<pattern>@ => @<template>@
-- ```
defaultConfig :: ParserConfig
defaultConfig = ParserConfig {
        beginDelim = "@",
        endDelim = "@",
        arrow = "=>",
        escape = '\\'
    }

-- | Result of parsing as a type synonym
type ParserResult a = Either ParseError a

-- | Given a string and a parser config, extract the three parts
-- of a potential rule: pattern, template, and trailing characters.
extract :: ParserConfig -> String -> ParserResult (String,String,String)
extract conf str = do
    (lhs, rhs) <- findN (arrow conf) str
    if null rhs then Left EmptyRule else return ()
    (_   , pattstart) <- findN (beginDelim conf) lhs
    (patt, _        ) <- findE (endDelim   conf) pattstart
    (_   , tempstart) <- findN (beginDelim conf) rhs
    (temp, opt      ) <- findE (endDelim   conf) tempstart
    return (patt, temp, opt)
    where findE = findEnd Unescapable (\_ -> IllFormedRule) (escape conf)
          findN = findNext Unescapable (escape conf)

-- | Parse a line with a given configuration, a pattern parser and a tempate parser.
parseLine :: ParserConfig -> (String -> PP.ParserResult (Pattern l)) -> (String -> TP.ParserResult (Template t)) -> String -> ParserResult (Rule l t)
parseLine conf parsePatt parseTemp str = do
    (patt, temp, opt) <- extract conf str
    thePattern  <- transpose PatternError  $ parsePatt patt
    theTemplate <- transpose TemplateError $ parseTemp temp
    -- parseOption opt
    return $ Rule { rule_pattern = thePattern,
                    rule_template = theTemplate,
                    rule_automaton = emptyAutomaton }
    -- --------------
    where transpose cstr (Left e ) = Left $ cstr e
          transpose _    (Right a) = Right a

-- | Parse a set of lines with a given configuration, a pattern parser and
-- a template parser.
-- Returns a list of `ParserResult`, as to delegate error management to the caller.
parseLines :: ParserConfig -> (String -> PP.ParserResult (Pattern l)) -> (String -> TP.ParserResult (Template t)) -> [String] -> [ParserResult (Rule l t)]
parseLines conf parsePatt parseTemp lines =
    zipWith decorate (map (parseLine conf parsePatt parseTemp) lines) [1..]
    where decorate (Left  e) i = Left $ e `AtLine` i
          decorate p         _ = p

-- | Ignore empty rules (if it does not require to be treated as an error).
removeEmpty :: [ParserResult (Rule l t)] -> [ParserResult (Rule l t)]
removeEmpty =
    filter isEmpty
    where isEmpty (Left EmptyRule) = False
          isEmpty _                = True

-- | Process a list of parser result and refine it into a list of rules.
process :: (Eq l, Eq t, Monad m) => (String -> m ()) -> [ParserResult (Rule l t)] -> m (Rules l t)
process print results = do
    rules <- reverse <$> foldM process1 [] results
    let cols = collisions rules in
        if null cols
            then return ()
            else print $ "Warning: " ++ (show $ length cols) ++ " collision sets found."
    print "Compiling..."
    crules <- forM rules compile
    return crules
    where process1 acc (Left e) = do
              print $ errstring e
              return acc
          process1 acc (Right r) =
              case check r of
                Nothing -> return (r:acc)
                Just er -> do
                    print $ rerrstring er
                    return acc


