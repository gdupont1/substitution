{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : Substitution.Template
Description : describe templates, paremeterized token strings for replacement
Copyrigh    : (c) Guillaume Dupont, 2021
License     : GPLv3
Maintainer  : guillaume.dupont55@gmail.com

This describe a template, which is a string of tokens with hole.
The main goal of templates is to be used after regex matching, filling
the hole with captured groups.

The template hence references capture groups by number. Group 0 is always the
whole matched string.
-}
module Substitution.Template(
        TemplateToken(..), Template(..),
        maxRef,
        fill
    ) where

import Control.Monad (foldM)

-- | A template token, which is basically a wrapper for a (with either a or a group number)
data TemplateToken a = 
      Tk a              -- ^ Normal token
    | Rf Int            -- ^ Content of a captured group

-- | Eq instance for TemplateToken
instance (Eq a) => Eq (TemplateToken a) where
  (Tk a) == (Tk a') = a == a'
  (Rf i) == (Rf i') = i == i'
  _ == _ = False

-- | Show instance of TemplateToken; capture groupe is represented as $i where i is the group number
instance (Show a) => Show (TemplateToken a) where
  show (Tk tk) = show tk
  show (Rf i)  = "$" ++ show i

-- | Type synonym: a Template is a string of TemplateToken
type Template a = [TemplateToken a]

-- | Get the maximum group number referenced (for error management
-- purposes).
maxRef :: Template a -> Int
maxRef =
    foldl calcMax 0
    where calcMax acc x
            | (Rf i) <- x, i > acc = i
            | otherwise = acc

-- | Given the matched string, the captured group and a template, return the corresponding
-- template where holes have been properly filled
fill :: forall a. [a] -> [[a]] -> Template a -> [a]
fill matched captures temp =
    foldl parse1 [] temp
    where parse1 :: [a] -> TemplateToken a -> [a]
          parse1 acc (Tk tk) = acc ++ [tk]
          parse1 acc r@(Rf i )
              | i == 0 = acc ++ matched
              | otherwise = acc ++ (captures !! (i - 1))



