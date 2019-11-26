module TigerAllocation where

import Assem
import TigerTemp
import TigerGraph
import TigerMakeGraph
import TigerLiveness

import Data.Map as M
import Data.Set as S
import Control.Monad.State

data Worklists = Worklists {
    moveList :: M.Map Temp [Node Instr],
    adjSet :: S.Set (Temp, Temp),
    adjList :: M.Map Temp (S.Set Temp),
    degree :: M.Map Temp Int,

    activeMoves :: [Node Instr],
    worklistMoves :: [Node Instr],

    precolored :: S.Set Temp, -- TODO: Leer un poco más sobre esto xd
    initial :: S.Set Temp,
    spillWorklist :: [Temp],
    freezeWorklist :: [Temp],
    simplifyWorklist :: [Temp]
}

type Allocator a = State Worklists a

k = 6

build :: FlowGraph Instr -> LivenessMap -> Allocator ()
build flowGraph livenessMap =
    let fnodes = nodes $ graph flowGraph
        moveNodes = ismove flowGraph

        buildNode :: Node Instr -> Allocator ()
        buildNode node = do
            let live = liveOut $ livenessMap M.! node
                live' = if moveNodes M.! node
                            then S.difference live (S.fromList ((use flowGraph) M.! node))
                            else live
            when (moveNodes M.! node) $ do

                -- forall n \in def(I) \union use(I)
                let defAndUse = ((def flowGraph) M.! node) ++ ((use flowGraph) M.! node)
                mapM_ (flip buildMoveList node) defAndUse

                wlists <- get
                put $ wlists { worklistMoves = node:worklistMoves wlists}
            let live'' = S.union live' (S.fromList ((def flowGraph) M.! node))

            mapM_ (\d -> mapM_ (\l -> addEdge d l) live'') (S.fromList ((def flowGraph) M.! node))


        buildMoveList :: Temp -> Node Instr -> Allocator ()
        buildMoveList t node = do
          wlists <- get
          let newMoveList = M.insertWith (++) t [node] (moveList wlists)
          put $ wlists { moveList = newMoveList}

        addEdge :: Temp -> Temp -> Allocator ()
        addEdge d l = do
            wlists <- get
            when (S.notMember (d, l) (adjSet wlists) && d /= l) $ do
              let newAdjSet = S.insert (d, l) $ S.insert (l, d) (adjSet wlists)

                  newAdjList = if (S.notMember d (precolored wlists))
                    then M.insertWith S.union d (S.singleton l) (adjList wlists)
                    else adjList wlists
                  newDegree = if (S.notMember d (precolored wlists))
                    then M.insertWith (+) d 1 (degree wlists)
                    else degree wlists

                  newAdjList' = if (S.notMember l (precolored wlists))
                    then M.insertWith S.union l (S.singleton d) newAdjList
                    else newAdjList
                  newDegree' = if (S.notMember l (precolored wlists))
                    then M.insertWith (+) l 1 newDegree
                    else newDegree
              put $ wlists { adjSet = newAdjSet, adjList = newAdjList', degree = newDegree' }

    in mapM_ buildNode fnodes

makeWorklist :: Allocator ()
makeWorklist = do
  wlists <- get
  mapM_ makeWorklistStep (initial wlists)

  where
    makeWorklistStep :: Temp -> Allocator ()
    makeWorklistStep n = do
      wlists <- get
      moveRelatedBool <- moveRelated n
      let newInitial = initial wlists `S.difference` S.singleton n
      if degree wlists M.! n >= k
        then do
          let newSpillWorklist = spillWorklist wlists ++ [n]
          put $ wlists { initial = newInitial, spillWorklist = newSpillWorklist }
        else if moveRelatedBool
          then do
            let newFreezeWorklist = freezeWorklist wlists ++ [n]
            put $ wlists { initial = newInitial, freezeWorklist = newFreezeWorklist }
          else do
            let newSimplifyWorklist = simplifyWorklist wlists ++ [n]
            put $ wlists { initial = newInitial, simplifyWorklist = newSimplifyWorklist }


moveRelated :: Temp -> Allocator Bool
moveRelated t = do
  nodeSet <- nodeMoves t
  return (nodeSet /= S.empty)

nodeMoves :: Temp -> Allocator (S.Set (Node Instr))
nodeMoves t = do
  wlists <- get
  return $
    S.fromList (moveList wlists M.! t) `S.intersection` S.fromList (activeMoves wlists ++ worklistMoves wlists)