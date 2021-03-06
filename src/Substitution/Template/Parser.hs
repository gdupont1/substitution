{-|
Module      : Substitution.Template.Parser
Description : generic and modular parsing facility for parsing templates
Copyrigh    : (c) Guillaume Dupont, 2021
License     : GPLv3
Maintainer  : guillaume.dupont55@gmail.com
This module proposes a parsing functions for templates that is modular and
configurable.

The parsing extract "variables" (references to capture groups) based on the
specified configuration (capture reference identification character and 
escape character) and pass the rest to a function provided by the user that
transform the sequence of character into a sequence of remplate tokens.
-}
module Substitution.Template.Parser(
        ParseError(..), ParserConfig(..), defaultConfig,
        errstring,
        ParserResult(..), parse
    ) where

import Substitution.Internal.Util
import Substitution.Template
import Data.Char (isDigit,digitToInt)

-- | An error in the parsing of the template
data ParseError =
      InvalidCaptureRef Char    -- ^ The reference to capture provided is invalid (e.g. not a number)
    | UnclosedEscape            -- ^ A started escaped sequence has never been finished
    | ExtraEndEscape            -- ^ End of escaped sequence met but no escaped sequence started
    | Unescapable               -- ^ Escape character met but no character to escape (e.g. at EOF)
    | LabelError String         -- ^ Error while parsing labels
    deriving Show

-- | Transform an error to a human-readable string
errstring :: ParseError -> String
errstring (InvalidCaptureRef c) = "Invalid capture group reference " ++ show c
errstring UnclosedEscape        = "Escaped sequence started but never ended"
errstring ExtraEndEscape        = "Extra end delimiter for escaped sequence"
errstring Unescapable           = "Escape character met at end of stream"
errstring (LabelError tk)       = "Unexpected token around '..." ++ tk ++ "...'"

-- | Parser configuration
data ParserConfig = ParserConfig {
        captureRef  :: String,  -- ^ Symbol denoting a reference to a capture group (e.g. '$')
        beginEscape :: String,  -- ^ Symbol denoting the begining of an escaped sequence
        endEscape   :: String,  -- ^ Symbol denoting the end of an escaped sequence
        escape      :: Char     -- ^ Symbol denoting the escaping of a character (e.g. '\')
    }

-- | A default configuration for the parser, POSIX-style/sed syntax
defaultConfig :: ParserConfig
defaultConfig = ParserConfig {
        captureRef  = "$",
        beginEscape = "\"",
        endEscape   = "\"",
        escape      = '\\'
    }

-- | Pretty print a template with the given configuration
prettyPrint :: ParserConfig -> (l -> String) -> Template l -> String
prettyPrint conf printTk temp =
    concat $ map print0 temp
    where print0 (Tk t) = printTk t
          print0 (Rf i) = (captureRef conf) ++ (show i)

-- | Result of parsing as a type synonym
type ParserResult a = Either ParseError a

-- | The parsing function.
-- Given a parser configuration and a function parsing labels, return a function
-- that parses a string into a template.
parse :: ParserConfig -> (String -> Maybe [l]) -> String -> ParserResult (Template l)
parse conf parseLabel str =
    parse0 "" str
    where parse0 acc [] = parseLab acc
          parse0 acc l
              | null l =
                  parseLab acc
              | (x:y:xs) <- l, x == escape conf =
                  parse0 (y:x:acc) xs
              | [x] <- l, x == escape conf =
                  Left $ Unescapable
              | Just rem <- l `startsWith` (beginEscape conf) = do
                  (escaped, rem') <- findEnd Unescapable (\_ -> UnclosedEscape) (escape conf) (endEscape conf) rem
                  escT <- parseLab' ((beginEscape conf) ++ escaped ++ (endEscape conf))
                  accT <- parseLab  acc
                  remT <- parse0 "" rem'
                  return $ accT ++ escT ++ remT
              | Just rem <- l `startsWith` (endEscape conf) =
                  Left $ ExtraEndEscape
              | Just rem <- l `startsWith` (captureRef conf) =
                  case rem of
                    [] -> parse0 acc []
                    (xx:xxs) ->
                        if isDigit xx
                            then do
                                accT <- parseLab acc
                                remT <- parse0 "" xxs
                                return $ accT ++ ((Rf $ digitToInt xx):remT)
                            else Left $ InvalidCaptureRef xx
              | (x:xs) <- l =
                  parse0 (x:acc) xs
          parseLab' s =
              case parseLabel s of
                Nothing -> Left $ LabelError s
                Just ls -> Right $ map Tk ls
          parseLab s = parseLab' $ reverse s



