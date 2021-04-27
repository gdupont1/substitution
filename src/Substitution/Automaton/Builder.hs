{-|
Module      : Substitution.Automaton.Builder
Description : state-monad-based building facility for automatas
Copyrigh    : (c) Guillaume Dupont, 2021
License     : GPLv3
Maintainer  : guillaume.dupont55@gmail.com

This module defines a set of building facilities for creating
automata in a readable way (quasi DSML style). It is based around
the `State` monad.
-}
module Substitution.Automaton.Builder (
        BState, BuilderT,
        place', place, newPlace,
        edge, newEdge, newEdge',
        setValidating, unsetValidating, setInitState,
        build
    ) where

import Data.Bits
import Control.Monad.State.Lazy
import Substitution.Automaton
import Substitution.Label

-- | Builder state for the monad. It mostly contains the automaton
-- being built, plus a reference counter for generating place numbers.
data BState l = BState {
        _automaton :: Automaton l,
        _counter :: Int
    }

-- | The builder, as a type synonym involving `StateT`
type BuilderT l = StateT (BState l)

-- | Return a fresh new integer. Every call returns a new integer
-- (in theory).
next_int :: Monad m => BuilderT l m Int
next_int = do
    st <- get
    let n = (_counter st) + 1 in do
        put $ st { _counter = n }
        return n

-- | Upgraded `modify` function to specifically modify the automaton being
-- built.
modifyAutomaton :: Monad m => (Automaton l -> Automaton l) -> BuilderT l m ()
modifyAutomaton f = modify (\st -> st { _automaton = f $ _automaton st })

-- | Initial state for the builder: empty automaton and counter set to 0
initBState :: BState l
initBState = BState { _automaton = emptyAutomaton, _counter = 0 }

-- | Add a new place to the automaton with the given place ID
place' :: Monad m => PlaceId -> BuilderT l m Place
place' pid = place $ Place pid

-- | Add the given place to the automaton
place :: Monad m => Place -> BuilderT l m Place
place p = do
    modifyAutomaton $ addPlace p
    return p

-- | Create a new place and add it to the automaton
newPlace :: Monad m => BuilderT l m Place
newPlace = do
    n <- next_int
    place' n

-- | Add an edge to the automaton
edge :: Monad m => Edge l -> BuilderT l m (Edge l)
edge e = do
    modifyAutomaton $ addEdge e
    return e

-- | Create a new edge and add it to the automaton.
-- This version use directly a @Label@.
newEdge :: Monad m => Place -> Label l -> Place -> BuilderT l m (Edge l)
newEdge src lab dest =
    let e = Edge { source = src, label = lab, desti = dest } in do
        edge e

-- | Create a new edge and add it to the automaton.
-- This version use some `l` and wraps it in a @Label@.
newEdge' :: Monad m => Place -> l -> Place -> BuilderT l m (Edge l)
newEdge' src lab dest = newEdge src (Label lab) dest

-- | Add the given place to the set of validating places of the
-- automaton.
setValidating :: Monad m => Place -> BuilderT l m ()
setValidating p = modifyAutomaton $ addValidating p

-- | Remove the given place from the set of validating places of
-- the automaton.
unsetValidating :: Monad m => Place -> BuilderT l m ()
unsetValidating p = modifyAutomaton $ removeValidating p

-- | Set the initial state of the automaton
setInitState :: Monad m => Place -> BuilderT l m ()
setInitState p = modifyAutomaton $ setInit p

-- | Run the building program, starting with an initial state with
-- an empty automaton and a counter at 0 and return the built automaton.
build :: Monad m => BuilderT l m a -> m (Automaton l)
build s = _automaton <$> (execStateT s initBState)



