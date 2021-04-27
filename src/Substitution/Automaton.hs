{-|
Module      : Substitution.Automata
Description : Basic stateless automata
Copyright   : (c) Guillaume Dupont, 2021
License     : GPLv3
Maintainer  : guillaume.dupont55@gmail.com

This module describes basic labelled automata. An automata is modelled
by a set of places and edges. Note that an automaton does not have to
be deterministic at this point, but using a non-deterministic automaton
in the remainder of the library may lead to unexpected behaviours.
-}
{-# LANGUAGE TupleSections #-}
module Substitution.Automaton where

import Data.Function (on)
import qualified Data.Set as S
import Data.List (intersect)

import Substitution.Label

-- | Automaton edge. An edge is formed with two places and a label.
-- Label type `l` is generic
data Edge l = Edge {
    label  :: Label l,      -- ^ Edge's label
    source :: Place,        -- ^ Edge's starting point
    desti  :: Place         -- ^ Edge's end point
} deriving (Eq)

-- | Convenient `Show` instance for edges
instance (Show l) => Show (Edge l) where
  show edg =
      (show $ source edg) ++ " --[" ++ (show $ label edg) ++ "]--> " ++ (show $ desti edg)

-- | Type alias for the places' id
type PlaceId = Int

-- | Type for the automaton's place. A place is simply identified
-- by its identifier. Subsequently, two places are equal if and
-- only if they have the same identifier.
data Place = Place {
    ident       :: PlaceId
} deriving (Eq)

-- | Special place implictly contained in every automaton and that
-- represent a "failed" state, i.e. when the input string is not 
-- in the language of the automaton.
trap :: Place
trap = Place (-1)

instance Ord Place where
  compare = compare `on` ident

instance Show Place where
  show pl =
      "(" ++ placename ++ ")"
      where placename =
                if pl == trap then "X" else show $ ident pl

-- | Type synonym for the set of places
type Places = S.Set Place
-- | Type synonym for the list of edges
type Edges l  = [Edge l]

-- | Automaton type
data Automaton l = Automaton {
    places       :: Places,         -- ^ Set of places of the automaton (no duplicate)
    edges        :: Edges l,        -- ^ Edges of the automaton; the structure used here is less restrictive (multiple same edges can coexist)
    initState    :: Place,          -- ^ The automaton's initial state
    validating   :: Places,         -- ^ Set of places that are validating (i.e. denote a "matching" state)
    counter      :: Int
}

-- | Add a place to the automaton
addPlace :: Place -> Automaton l -> Automaton l
addPlace p aut = aut { places = S.insert p $ places aut }

-- | Remove a place from the automaton
removePlace :: Place -> Automaton l -> Automaton l
removePlace p aut =
    aut { places = S.delete p $ places aut, edges = cleanUpEdges $ edges aut }
    where cleanUpEdges = filter (\e -> (source e == p) || (desti e == p))

-- | Add edge to the automaton
addEdge :: Edge l -> Automaton l -> Automaton l
addEdge e aut = aut { edges = e:(edges aut) }

-- | Set initial state
setInit :: Place -> Automaton l -> Automaton l
setInit p aut = aut { initState = p }

-- | Add a place to the set of validatin places
addValidating :: Place -> Automaton l -> Automaton l
addValidating p aut = aut { validating = S.insert p $ validating aut }

-- | Remove a place from the set of validating places
removeValidating :: Place -> Automaton l -> Automaton l
removeValidating p aut = aut { validating = S.delete p $ validating aut }

-- | Convenient `Eq` instance for the Automaton type.
-- Two automata are equal iff they have the same places, same
-- edges, same initial state and same validating states.
instance (Eq l) => Eq (Automaton l) where
  a1 == a2 =
      (places a1 == places a2)
      && (length (intersect (edges a1) (edges a2)) == length (edges a1)) -- edges do not have to be in the same order!
      && (initState a1 == initState a2)
      && (validating a1 == validating a2)

-- | Special empty automaton
emptyAutomaton :: Automaton l
emptyAutomaton = Automaton { places = S.empty, edges = [], validating = S.empty, initState = trap, counter = 0 }

-- | Convenient `Show` instance: output an automaton as a DOT graph
instance (Show l) => Show (Automaton l) where
  show aut = 
      "digraph {"
      ++ "\n  rankdir=LR;"
      ++ (S.foldl showPlace "" $ places aut)
      ++ (foldl showEdge "" $ edges aut)
      ++ "\n}"
      where showPlace acc pl  = acc ++ "\n  " ++ (plid pl) ++ "[label=\"" ++ show pl ++ mstar pl ++ "\"];"
            showEdge  acc edg = acc ++ "\n  " ++ (plid $ source edg) ++ " -> " ++ (plid $ desti edg) ++ "[label=\"" ++ (show $ label edg) ++ "\"];"
            plid pl = "pl_" ++ (show $ ident pl)
            mstar pl = if S.member pl (validating aut) then "*" else ""


-- | Apply an (injective) transformation on every place.
-- If the transformation is injective, then the resulting automaton
-- is semantically equivalent (strongly bi-similar, in fact).
transform :: (PlaceId -> PlaceId) -> Automaton l -> Automaton l
transform f aut =
    Automaton {
        places = S.map processPlace $ places aut,
        edges  = map processEdge $ edges aut,
        initState = processPlace $ initState aut,
        validating = S.map processPlace $ validating aut,
        counter = counter aut
    }
    where processEdge edg = Edge { label = label edg, source = processPlace $ source edg, desti = processPlace $ desti edg }
          processPlace pl = Place { ident = f $ ident pl }
        




