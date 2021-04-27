{-|
Module      : Substitution.Automata.Reader
Description : Enactment of automata
Copyright   : (c) Guillaume Dupont, 2021
License     : GPLv3
Maintainer  : guillaume.dupont55@gmail.com

This module defines the basics of enacting automata (or "reading" them).
-}
module Substitution.Automaton.Reader(
        AutomatonReader,
        captures,readTokens,remainingTokens,
        reader,validatingState,trapped,
        finished,finishedorvalidating,
        step,stepall,stepuntilvalidating
    )where

import Data.List (sortBy)
import Data.Function (on)
import qualified Data.Set as S

import Substitution.Automaton
import Substitution.Label

-- | Token option to add the special "<end of string>" token
data ATk t = AT t | AEnd

instance Show t => Show (ATk t) where
  show (AT t) = show t
  show AEnd = "$$$"

-- | Calculate the tokens have not been read by the given automaton reader
remainingTokens :: AutomatonReader l t -> [t]
remainingTokens autrd =
    map ext $ takeWhile notend $ remainingATokens autrd
    where notend AEnd = False
          notend _    = True
          ext (AT tk) = tk

-- | AutomatonReader: structure that "reads" an automaton and tries to make it survive
-- with a given entry string.
data AutomatonReader l t = AutomatonReader {
    automaton        :: Automaton l,     -- ^ Automaton to read
    currentState     :: Place,           -- ^ Current state in the automaton
    currentCaptures  :: [[t]],           -- ^ Captures that are being made
    captures         :: [[t]],           -- ^ Captures that are complete
    readTokens       :: [t],             -- ^ Tokens that have been read
    remainingATokens :: [ATk t],         -- ^ Remaining tokens
    edgesTaken       :: [Edge l]         -- ^ Edges the reader took
} deriving Show

-- | Build an AutomatonReader from an automaton and an input string.
-- The initial automaton is in its initial state, captures are empty and no
-- tokens have been read.
reader :: (Matcher l t) => Automaton l -> [t] -> AutomatonReader l t
reader aut tks =
    skipall $ AutomatonReader {
        automaton = aut,
        currentState = initState aut,
        currentCaptures = [],
        captures = [],
        readTokens = [],
        remainingATokens = (map AT tks) ++ [AEnd],
        edgesTaken = []
    }

-- | Determine if the current state of the AutomatonReader is validating
validatingState :: AutomatonReader l t -> Bool
validatingState rd = S.member (currentState rd) (validating $ automaton $ rd)

-- | Determine if the AutomatonReader is trapped (i.e. in state `trap`)
trapped :: AutomatonReader l t -> Bool
trapped = (< 0) . ident . currentState

-- | Calculate the next edges of given place in given automaton.
-- This function realizes a partial closure: it returns not only the
-- edges directly related to the palce, but also the full path leaving a 
-- place and made of non-consuming edges.
-- Hence, if we have
--     1 --[a]--> 2 --[skip]--> 3
-- with skip that is non consuming, the function will return [[1 --[a]--> 2, 2 --[skip]--> 3]]
nextedges' :: (Matcher l t) => Automaton l -> Place -> [[Edge l]]
nextedges' aut pl =
    sortBy (compare `on` (label . last)) $ foldl (++) [] $ map resolve $ simplenext pl
    where simplenext pl = filter ((== pl) . source) $ edges aut
          resolve edge
              | isConsuming $ label edge = [[edge]]
              | otherwise = map (edge:) $ nextedges' aut $ desti edge

-- | Determine the next edges in the current status of the reader
nextedges :: (Matcher l t) => AutomatonReader l t -> [[Edge l]]
nextedges autrd = nextedges' (automaton autrd) (currentState autrd)

-- | Auxiliary function that is assured to find the first element verrifying
-- given predicate.
findfirst :: (a -> Bool) -> [a] -> Maybe a
findfirst _ [] = Nothing
findfirst p (t:ts)
    | p t = Just t
    | otherwise = findfirst p ts

-- | Find the next edge matching the given label.
-- Edges are ordered according to label order; usually one takes advantage
-- of such order to prioritize certain labels over some others.
nextedge :: (Matcher l t) => AutomatonReader l t -> t -> Maybe [Edge l]
nextedge autrd tk = findfirst ((//> tk) . label . last) $ nextedges autrd

-- | Skip all edges that need to be skipped.
skipall :: (Matcher l t) => AutomatonReader l t -> AutomatonReader l t
skipall autrd =
    case nextskip autrd of
      Nothing -> autrd
      Just e  -> skipall $ rawstep autrd e --(newcapturemode autrd e) e 
      where nextskip autrd = findfirst ((== Skip) . label) $ filter ((== (currentState autrd)) . source) $ edges $ automaton autrd

-- | Perform a simple step of the reader using given edge. This simply changes the current
-- state and append the edge to the edge history.
rawstep :: AutomatonReader l t -> Edge l -> AutomatonReader l t
rawstep autrd edge = autrd { currentState = desti edge, edgesTaken = (edgesTaken autrd) ++ [edge] }

-- | Perform a step of the reader.
step :: (Matcher l t) => AutomatonReader l t -> AutomatonReader l t
step autrd =
    case remainingATokens autrd of
      -- No tokens remaining: reader is unchanged
      []            -> autrd
      -- End token met: no matter what comes after that, we simply wrap-up the automaton
      (AEnd:_)      -> wrapup $ autrd { remainingATokens = [] }
      -- Read 1 token
      ((AT tk):tks) ->
        case nextedge autrd tk of -- Look for a parting edge
          Nothing -> -- No parting edge => go to trap
              autrd { currentState = trap 
                    , readTokens = tk:(readTokens autrd)
                    , remainingATokens = tks }
          Just edges -> -- A sequence of edges going out of this place (probably multiple skips and one labelled edge)
              let autrd' = foldl onestep autrd $ init edges -- Step all skip edges
                  edge   = last edges in -- Last edge should be the labelled edge
                  let autrd'' = newcapturemode autrd' edge in -- Update capture state
                      let (newreadTokens,newremainingTokens,newcaptures) = consume edge tk tks (readTokens autrd'') (currentCaptures autrd'') in
                          skipall $ ((rawstep autrd'' edge) { -- Skip all incoming edges and update automaton
                            currentCaptures = newcaptures
                            , readTokens = newreadTokens
                            , remainingATokens = newremainingTokens
                          })
    where consume edge tk tks rtks ctks -- Consume (if the edge requires it) a token
            | isConsuming $ label edge = (tk:rtks,tks,map ((:) tk) ctks)
            | otherwise                = (rtks,(AT tk):tks,ctks)
          wrapup autrd = -- "wrap-up" the automaton : make sure every capture is ended and every skip has been skipped
              if validatingState autrd then autrd
              else
                case nextendcapture autrd of
                  Nothing -> autrd
                  Just e -> wrapup $ skipall $ rawstep (newcapturemode autrd e) e
          nextendcapture autrd = -- find the next end capture edge
              findfirst ((== EndCapture) . label) $ filter ((== currentState autrd) . source) $ edges $ automaton autrd
          onestep autrd edge = skipall $ rawstep (newcapturemode autrd edge) edge -- perform one step, that is, one raw step plus every needed skip

-- | Determine the new capture mode of the automaton based on the taken edge
newcapturemode :: (Matcher l t) => AutomatonReader l t -> Edge l -> AutomatonReader l t
newcapturemode autrd edge =
    case label edge of
      BeginCapture -> autrd { currentCaptures = []:(currentCaptures autrd) }
      EndCapture   ->
          let (capture,remaining) = (,) <$> head <*> tail $ currentCaptures autrd in
              autrd {
                currentCaptures = remaining,
                captures = (captures autrd) ++ [reverse capture]
              }
      _ -> autrd

-- | Determine if the reader is finished, i.e. if there are no more tokens remaining or it is trapepd
finished :: AutomatonReader l t -> Bool
finished rd = (null $ remainingATokens rd) || (trapped rd)

-- | Determine if the reader is finished or validating
finishedorvalidating :: AutomatonReader l t -> Bool
finishedorvalidating rd = (validatingState rd) || (finished rd)

-- | Step the automaton until it is finished
stepall :: (Matcher l t) => AutomatonReader l t -> AutomatonReader l t
stepall =
    head . dropWhile (not . finished) . iterate step

-- | Step the automaton until it is finished or validating
stepuntilvalidating :: (Matcher l t) => AutomatonReader l t -> AutomatonReader l t
stepuntilvalidating =
    head . dropWhile (not . finishedorvalidating) . iterate step


