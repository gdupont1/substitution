{-|
Module      : Substitution.Internal.Util
Description : various general-purpose utilities used throughout the library
Copyrigh    : (c) Guillaume Dupont, 2021
License     : GPLv3
Maintainer  : guillaume.dupont55@gmail.com
-}
module Substitution.Internal.Util where

-- | Matching the beginning of a string
startsWith :: String -> String -> Maybe String
startsWith str patt
    | take (length patt) str == patt = Just $ drop (length patt) str
    | otherwise = Nothing

-- | Find the next occurence of a given pattern, ignoring characters
-- that are escaped.
findNext :: e -> Char -> String -> String -> Either e (String,String)
findNext escerr esc patt str =
    browse "" str
    where n = length patt
          browse acc rem
              | (x:y:xs) <- rem, x == esc = browse (y:esc:acc) xs
              | [x] <- rem, x == esc = Left escerr 
              | length rem < n || null rem = Right $ (reverse $ rem ++ acc, [])
              | take n rem == patt = Right $ (reverse acc, drop n rem)
              | (x:xs) <- rem = browse (x:acc) xs

-- | Find the matching closing delimiter (with given closing delimiter
-- pattern), ignoring characters that are escaped.
--
-- Note that this find the first (unescaped) occurrence of the end
-- delimiter, not concerned with nested symbols.
findEnd :: e -> (String -> e) -> Char -> String -> String -> Either e (String, String)
findEnd escerr illerr esc patt str =
    browse "" str
    where n = length patt
          browse acc rem
              | (x:y:xs) <- rem, x == esc = browse (y:esc:acc) xs
              | [x] <- rem, x == esc = Left escerr
              | length rem < n || null rem = Left $ illerr rem
              | take n rem == patt = Right (reverse acc, drop n rem)
              | (x:xs) <- rem = browse (x:acc) xs

-- | Find the matching closing delimiter.
-- This is basically an implementation of a stack automaton: we want to find
-- the end delimiter character of rank 0. Rank is increased whenever we find
-- a begin delimiter, decreased whenever we find a end delimiter. That way, we
-- are sure that the character found matches the begin capture found earlier.
--
-- The function returns a pair which first element is the string between delimiters,
-- (excluding the begin/end patterns) and the second element is the remainder
-- of the string.
findEndNested :: e -> (String -> e) -> Char -> String -> String -> String -> Either e (String,String)
findEndNested escerr illerr esc begin end str =
    findEnd0 0 "" str
    where findEnd0 n acc rem
              | (x:y:xs) <- rem, x == esc =
                  findEnd0 n (y:esc:acc) xs
              | [x] <- rem, x == esc =
                  Left escerr
              | [] <- rem =
                  Left $ illerr rem
              | take bn rem == begin =
                  findEnd0 (n + 1) ((reverse begin) ++ acc) (drop bn rem)
              | take en rem == end && n == 0 =
                  Right $ (reverse acc, drop en rem)
              | take en rem == end =
                  findEnd0 (n - 1) ((reverse end) ++ acc) (drop en rem)
              | (x:xs) <- rem =
                  findEnd0 n (x:acc) xs
          bn = length begin
          en = length end



