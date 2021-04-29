{-# LANGUAGE ExistentialQuantification, ScopedTypeVariables #-}
{-|
Module      : Substitution.Pattern.Parser
Description : a generic modular parser for handling patterns
Copyrigh    : (c) Guillaume Dupont, 2021
License     : GPLv3
Maintainer  : guillaume.dupont55@gmail.com

This module proposes a generic, modular, configurable parser for
generating patterns from string.

Parsing is done on two levels: a higher level for parsing special
characters that are actually "pattern commands" (repetition,
alternative, etc.) and a lower level for parsing pattern units,
called "labels" in this case (simply because this will be translated
to labels when building the corresponding automaton).

A parser is configured in two steps:
  1. The higher level part is configured using the @ParserConfig@
  record. This basically allows to define the characters that ought
  to be used when performing high-level parsing.
  2. The lower level part is configured by giving a custom parsing
  function `String -> Maybe [l]`, that turns a sequence of characters
  to a list of labels (of type `l`), or to `Nothing` if there is a
  parsing error at this level.

Note that a default configuration is provided, that proposes a high-
level parsing similar to POSIX regex.
-}
module Substitution.Pattern.Parser (
        ParseError(..),
        ParserConfig(..), defaultConfig,
        ParserResult(..),
        errstring,
        prettyPrint,
        parse
    ) where

import Substitution.Internal.Util
import Substitution.Pattern
import Substitution.Label

-- | Error during parsing
data ParseError =
      UnclosedCapture       -- ^ A capture group is never closed
    | ExtraEndCapture       -- ^ A group ending character is encountered but no matching being group has
    | UnclosedEscape        -- ^ An started escaped sequence has never been finished 
    | ExtraEndEscape        -- ^ An escaped sequence ending character is encountered but no matching begin has
    | Unescapable           -- ^ An escape character has been encountered but no character follows (typically at EOL)
    | LabelError String     -- ^ The label parsing function returned `Nothing`
    deriving Show

-- | Transform an error to a human-readable string
errstring :: ParseError -> String
errstring UnclosedCapture = "Capture group started but never ended"
errstring ExtraEndCapture = "Extra end delimiter for capture group"
errstring UnclosedEscape  = "Escaped sequence started but never ended"
errstring ExtraEndEscape  = "Extra end delimiter for escaped sequence"
errstring Unescapable     = "Escape character met at end of stream"
errstring (LabelError tk) = "Unexpected token around '..." ++ tk ++ "...'"

-- | Configuration for the high-level parsing.
data ParserConfig = ParserConfig {
        beginCapture :: String,     -- ^ Character for denoting begining of capture groupe ('(')
        endCapture   :: String,     -- ^ Character for denoting ending of catpure groupe (')')
        repetition   :: String,     -- ^ Character for denoting repetition; always postfix, most associative ('*')
        repetition1  :: String,     -- ^ Character for denoting one or more reptition; always postfix, most associative ('+')
        optional     :: String,     -- ^ Character for denoting option; always postfix, most associative ('?')
        choice       :: String,     -- ^ Character for denoting alternative; always infix, least associative ('|')
        beginEscape  :: String,     -- ^ Character for denoting the begining of an escaped sequence
        endEscape    :: String,     -- ^ Character for denoting the ending of an escaped sequence
        escape       :: Char        -- ^ Character for escaping other character ('\')
    }

-- | Default configuration; POSIX-style regex
defaultConfig :: ParserConfig
defaultConfig = ParserConfig {
        beginCapture = "(",
        endCapture   = ")",
        repetition   = "*",
        repetition1  = "+"
        optional     = "?",
        choice       = "|",
        beginEscape  = "\"",
        endEscape    = "\"",
        escape       = '\\'
    }

-- | Pretty print a pattern according to given configuration.
-- In theory, `parse conf lab . prettyPrint conf lab` should be `id` (or at
-- least an isomorphism).
prettyPrint :: ParserConfig -> (l -> String) -> Pattern l -> String
prettyPrint conf printLab patt =
    print0 patt
    where print0 EmptyPattern = ""
          print0 (Capture p) =
              (beginCapture conf) ++ (print0 p) ++ (endCapture conf)
          print0 (Repeat p) =
              (print0 p) ++ (repetition conf)
          print0 (Optional p) =
              (print0 p) ++ (optional conf)
          print0 (Or p1 p2) =
              (print0 p1) ++ (choice conf) ++ (print0 p2)
          print0 (Seq p1 p2) =
              (print0 p1) ++ (print0 p2)
          print0 (Lab l) =
              printLab l

-- | Type synonym for the result of parsing. This is an `Either` type with errors on left
type ParserResult a = Either ParseError a

-- | Main parsing function.
-- Given a high level @ParserConfig@ parser configuration and a label parsing function
-- (`String -> Maybe [l]`), return a parser, i.e. a function that turns a string into
-- a pattern of labels `l`.
parse :: forall l. ParserConfig -> (String -> Maybe [l]) -> String -> ParserResult (Pattern l)
parse conf parseLabel str =
    normalize <$> parse0 "" EmptyPattern str
    where parse0 :: [Char] -> Pattern l -> String -> ParserResult (Pattern l)
          parse0 acc lastBlock l
              | [] <- l =
                  parseLab lastBlock (id) acc
              | (x:y:xs) <- l, x == escape conf =
                  parse0 (y:x:acc) lastBlock xs
              | [x] <- l, x == escape conf =
                  Left $ Unescapable
              | Just rem <- l `startsWith` (beginCapture conf) = do
                  (captured,rem') <- findEndN rem
                  captpatt <- parse0 "" EmptyPattern captured
                  accPat <- parseLab lastBlock (id) acc
                  parse0 "" (Seq accPat (Capture captpatt)) rem'
              | Just rem <- l `startsWith` (endCapture conf) =
                  Left $ ExtraEndCapture
              | Just rem <- l `startsWith` (beginEscape conf) = do
                  (escaped, rem') <- findEndE rem
                  accPat <- parseLab  lastBlock (id) acc
                  escPat <- parseLab' accPat    (id)  ((beginEscape conf) ++ escaped ++ (endEscape conf))
                  parse0 "" (Seq accPat escPat) rem'
              | Just rem <- l `startsWith` (endEscape conf) 
                  Left $ ExtraEndEscape
              | Just rem <- l `startsWith` (repetition conf) = do
                  accPat <- parseLab lastBlock (Repeat) acc
                  parse0 "" accPat rem
              | Just rem <- l `startsWith` (repetition1 conf) = do
                  accPat <- parseLab lastBlock (\x -> Seq x (Repeat x)) acc
                  parse0 "" accPat rem
              | Just rem <- l `startsWith` (optional conf) = do
                  accPat <- parseLab lastBlock (Optional) acc
                  parse0 "" accPat rem
              | Just rem <- l `startsWith` (choice conf) = do
                  accPat <- parseLab lastBlock (id) acc
                  rempatt <- parse0 "" EmptyPattern rem
                  return $ Or accPat rempatt
              | (x:xs) <- l =
                  parse0 (x:acc) lastBlock xs
          findEndN = findEndNested Unescapable (\_ -> UnclosedCapture) (escape conf) (beginCapture conf) (endCapture conf)
          findEndE = findEnd Unescapable UnclosedEscape (escape conf) (endEscape conf) 
          parseLab' :: Pattern l -> (Pattern l -> Pattern l) -> [Char] -> ParserResult (Pattern l)
          parseLab' lastBlock wrapup s =
              case parseLabel s of
                Nothing -> Left $ LabelError s
                Just [] -> Right $ wrapup lastBlock
                Just ls ->
                    case lastBlock of
                      EmptyPattern -> Right $ toSeq wrapup ls
                      _            -> Right $ Seq lastBlock $ toSeq wrapup ls
          parseLab :: Pattern l -> (Pattern l -> Pattern l) -> [Char] -> ParserResult (Pattern l)
          parseLab lastBlock wrapup s =
              parseLab' lastBlock wrapup $ reverse s
          toSeq :: (Pattern l -> Pattern l) -> [l] -> Pattern l
          toSeq _      [] = EmptyPattern
          toSeq wrapup [x] = wrapup $ Lab x
          toSeq wrapup (x:xs) = Seq (Lab x) (toSeq wrapup xs)







