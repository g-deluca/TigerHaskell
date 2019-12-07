module TigerAllocation where

import Assem
import TigerTemp
import TigerGraph
import TigerMakeGraph
import TigerLiveness

import Data.Map as M
import Data.Set as S
import Data.List as L
import Control.Monad.State

k = 6

data Worklists = Worklists {

    -- TODO: ya que en build() tenemos flowGraph y LivenessMap, armamos el interferencaGraph y lo tenemos acá?

    -- Custom
    -- lista de colores disponibles
    okColors :: S.Set Int,
    -- Node worklists, sets and stacks. The following
    -- lists and sets are always mutually disjoint and every node is always
    -- in exactly on of the sets or lists.

    -- machine registers, preassigned a color
    precolored :: S.Set Temp,  -- TODO: Leer un poco más sobre esto xd
    -- temporary registers, not precolored and not yet processed
    initial :: S.Set Temp,
    -- list of low-degree non-move-related nodes
    simplifyWorklist :: [Temp],
    -- low-degree move-related nodes
    freezeWorklist :: [Temp],
    -- high-degree nodes
    spillWorklist :: [Temp],
    -- nodes marked for spilling during this round: initially empty
    spilledNodes :: [Temp],
    -- registers that have been coalesced
    -- when u <- v is coalesced, v is added to this set and u put back in some work-list (or viceversa)
    coalescedNodes :: [Temp],
    -- nodes suceessfully colored
    coloredNodes :: [Temp],
    -- stack containing temporaries removed from the graph
    selectStack :: [Temp],

    -- Move sets. 5 sets of move instructions, and every Move is in exactly one of these
    -- (after Build through the end of Main)
    
    -- moves that have been coalesced
    coalescedMoves :: [Node Instr],
    -- moves whose source and target interfere
    constrainedMoves :: [Node Instr],
    -- moves that will no longer be considered for coalescing
    frozenMoves :: [Node Instr],
    -- moves enabled for possible coalescing
    worklistMoves :: [Node Instr],
    -- moves not yet ready for coalescing
    activeMoves :: [Node Instr],

    -- the set of interference edges (u,v) in the graph. if (u,v) \in adjSet => (v,u) \in adjSet
    adjSet :: S.Set (Temp, Temp),
    -- adjacency list representation of the graph.
    -- for each non-precolored temporary u, adjList[u] is the set of nodes that interfere with u.
    adjList :: M.Map Temp (S.Set Temp),
    -- the current degree of each node
    degree :: M.Map Temp Int,
    -- a mapping from a node to the list of moves it is associated with
    moveList :: M.Map Temp [Node Instr],
    -- when a move (u, v) has been coalesced, and v put in coalescedNodes,
    -- alias[v] = u
    alias :: M.Map Temp Temp,
    -- the color chosen by the algorithm for a node;
    -- for precolored nodes this is initialized to the given color
    color :: M.Map Temp Int

}

type Allocator a = State Worklists a

setOkColors :: Allocator ()
setOkColors = do
  wlists <- get
  put wlists { okColors = S.fromList [0..(k-1)] }

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
    in mapM_ buildNode fnodes

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

adjacent :: Temp -> Allocator [Temp]
adjacent t = do
  wlists <- get
  return $ S.toList $ (adjList wlists M.! t) `S.difference` (S.fromList (selectStack wlists) `S.union` S.fromList (coalescedNodes wlists))

simplify :: Allocator ()
simplify = do
  wlists <- get
  let n = head $ simplifyWorklist wlists
      newSimplifyWorklist = tail $ simplifyWorklist wlists
      newSelectStack = n:(selectStack wlists)
  put $ wlists {simplifyWorklist = newSimplifyWorklist, selectStack = newSelectStack}
  adj <- adjacent n
  mapM_ decrementDegree adj

decrementDegree :: Temp -> Allocator ()
decrementDegree t = do
  wlists <- get
  adj <- adjacent t
  isMoveRelated <- moveRelated t
  let d = degree wlists M.! t
      newDegree = M.insert t (d-1) (degree wlists)
      newSpillWorklist = if (d == k) then L.delete t (spillWorklist wlists) else spillWorklist wlists
  when (d == k) $ do
    enableMoves $ adj ++ [t]
    if isMoveRelated then
      let newFreezeWorklist = (freezeWorklist wlists) ++ [t]
      in put wlists { degree = newDegree, spillWorklist = newSpillWorklist, freezeWorklist = newFreezeWorklist}
    else
      let newSimplifyWorklist = (simplifyWorklist wlists) ++ [t]
      in put wlists { degree = newDegree, spillWorklist = newSpillWorklist, simplifyWorklist = newSimplifyWorklist}
  return ()

enableMoves :: [Temp] -> Allocator ()
enableMoves temps = do
  mapM_ doEnableMoves temps
  where
    doEnableMoves :: Temp -> Allocator ()
    doEnableMoves t = do
      nMoves <- nodeMoves t
      mapM_ doUpdate (S.toList nMoves)
    doUpdate :: Node Instr -> Allocator ()
    doUpdate n = do
      wlists <- get
      when (n `L.elem` (activeMoves wlists)) $ do
        let newActiveMoves = n `L.delete` (activeMoves wlists)
            newWorklistMoves = (worklistMoves wlists) ++ [n]
        put wlists {activeMoves = newActiveMoves, worklistMoves = newWorklistMoves}

coalesce :: Allocator ()
coalesce = do
  wlists <- get
  let m@(Node _ (move)) = head $ worklistMoves wlists
  x <- getAlias $ msrc move
  y <- getAlias $ mdst move
  let (u, v) = if L.elem y (precolored wlists) then (y,x) else (x,y)
  let newWorklistMoves = L.filter (==m) (worklistMoves wlists)
  adjU <- adjacent u
  adjV <- adjacent v
  isConservative <- conservative (adjU ++ adjV)
  isOk <- checkOk v u
  if u == v then do
    addCoalescedMoves m
    addWorkList u
  else  if L.elem v (precolored wlists) && S.member (u,v) (adjSet wlists) then do
          addConstrainedMoves m
          addWorkList u
          addWorkList v
        else  if  L.elem u (precolored wlists) && isOk || 
                  L.notElem u (precolored wlists) && isConservative then do
                addCoalescedMoves m
                combine u v
                addWorkList u
              else do
                addActiveMoves m
  where
    addCoalescedMoves :: Node Instr -> Allocator ()
    addCoalescedMoves m = do
      wlists <- get
      let newCoalescedMoves = (coalescedMoves wlists) ++ [m]
      put wlists {coalescedMoves = newCoalescedMoves}
    addConstrainedMoves :: Node Instr -> Allocator ()
    addConstrainedMoves m = do
      wlists <- get
      let newConstrainedMoves = (constrainedMoves wlists) ++ [m]
      put wlists {constrainedMoves = newConstrainedMoves}
    checkOk :: Temp -> Temp -> Allocator Bool
    checkOk v u = do
      wlists <- get
      adj <- adjacent v
      foldM (\accum a -> ok a u) True adj
    addActiveMoves :: Node Instr -> Allocator ()
    addActiveMoves m = do
      wlists <- get
      let newActiveMoves = (activeMoves wlists) ++ [m]
      put wlists {activeMoves = newActiveMoves}


addWorkList :: Temp -> Allocator ()
addWorkList u = do
  wlists <- get
  isMoveRelated <- moveRelated u
  when (S.notMember u (precolored wlists) && not (isMoveRelated) && (degree wlists M.! u) < k) $ do
    let newFreezeWorklist = L.filter (==u) (freezeWorklist wlists)
        newSimplifyWorklist = (simplifyWorklist wlists) ++ [u]
    put wlists {freezeWorklist = newFreezeWorklist, simplifyWorklist = newSimplifyWorklist}

ok :: Temp -> Temp -> Allocator Bool
ok t r = do
  wlists <- get
  let tDegree = (degree wlists) M.! t
      isPrecolored = S.member t (precolored wlists)
      isAdjSet = S.member (t, r) (adjSet wlists)
  return $ (tDegree < k) || isPrecolored || isAdjSet

conservative :: [Temp] -> Allocator Bool
conservative nodes = do
  wlists <- get
  let i = L.foldl (\a n -> if (degree wlists) M.! n >= k then a+1 else a) 0 (nodes)
  return (i < k)

getAlias :: Temp -> Allocator Temp
getAlias n = do
  wlists <- get
  let nAlias = (alias wlists) M.! n
  nAlias' <- getAlias nAlias
  return $ if L.elem n (coalescedNodes wlists) then  nAlias' else n

combine :: Temp -> Temp -> Allocator ()
combine u v = do
  wlists <- get
  adj <- adjacent v
  let 
    newFreezeWorklist = if L.elem v (freezeWorklist wlists) then L.filter (==v) (freezeWorklist wlists) else freezeWorklist (wlists)
    newSpillWorklist = if L.notElem v (freezeWorklist wlists) then L.filter (==v) (spillWorklist wlists) else spillWorklist (wlists)
    newCoalescedNodes = (coalescedNodes wlists) ++ [v]
    newAlias = M.insert v u (alias wlists)
    newMovelist = M.insert u (((moveList wlists) M.! u) ++ ((moveList wlists) M.! v)) (moveList wlists)
  mapM_ (\t -> do addEdge t v >> decrementDegree t) adj
  when (degree wlists M.! u >= k && L.elem u (freezeWorklist wlists)) $ do
    let newFreezeWorklist' = L.filter (==u) newFreezeWorklist
        newSpillWorklist' = newSpillWorklist ++ [u]
    put wlists {freezeWorklist = newFreezeWorklist', spillWorklist = newSpillWorklist',
                coalescedNodes = newCoalescedNodes, alias = newAlias, moveList = newMovelist}

freeze :: Allocator ()
freeze = do
  wlists <- get
  let u = head $ freezeWorklist wlists
  let newFreezeWorklist = tail $ freezeWorklist wlists
  let newSimplifyWorklist = simplifyWorklist wlists ++ [u]
  put wlists {freezeWorklist = newFreezeWorklist, simplifyWorklist = newSimplifyWorklist}
  freezeMoves u

freezeMoves :: Temp -> Allocator ()
freezeMoves u = do
  nMoves <- nodeMoves u
  mapM_ doFreezeMoves nMoves
    where
      doFreezeMoves :: Node Instr -> Allocator ()
      doFreezeMoves m@(Node _ (move)) = do
        wlists <- get
        let x = msrc move
            y = mdst move
        xAlias <- getAlias x
        yAlias <- getAlias y
        uAlias <- getAlias u
        let v = if yAlias == uAlias then xAlias else yAlias
        vMoves <- nodeMoves v
        let newActiveMoves = L.filter (==m) (activeMoves wlists)
        let newFrozenMoves = (frozenMoves wlists) ++ [m]
        when (S.null vMoves && ((degree wlists) M.! v) < k) $ do
          let newFreezeWorklist = L.filter (==v) (freezeWorklist wlists)
          let newSimplifyWorklist = (simplifyWorklist wlists) ++ [v]
          put wlists {activeMoves = newActiveMoves, frozenMoves = newFrozenMoves,
                      simplifyWorklist = newSimplifyWorklist, freezeWorklist = newFreezeWorklist}

selectSpill :: Allocator ()
selectSpill = do
  wlists <- get
  let m = head $ spillWorklist wlists -- heuristica??
      newSpillWorklist = tail $ spillWorklist wlists
      newSimplifyWorklist = (simplifyWorklist wlists) ++ [m]
  freezeMoves m

-- work in progress de ak pa abajo
assignColors :: Allocator ()
assignColors = do
  wlists <- get
  doAssignColors (selectStack wlists)
  mapM_ doColor (coalescedNodes wlists)
    where
      doColor :: Temp -> Allocator ()
      doColor n = do
        wlists <- get
        nAlias <- getAlias n
        let newColor = M.insert n ((color wlists) M.! nAlias) (color wlists)
        put wlists {color = newColor}

-- este seria el while del libro
doAssignColors :: [Temp] -> Allocator ()
doAssignColors [] = return ()
doAssignColors sStack = do
  wlists <- get
  let n = head $ sStack
      newSelectStack = tail $ sStack
  put wlists {selectStack = newSelectStack}
  setOkColors
  mapM_ updateColors (S.toList $ (adjList wlists) M.! n)
  wlists' <- get
  if (S.null $ okColors wlists') then do
    let newSpilledNodes = (spilledNodes wlists') ++ [n]
    put wlists' {spilledNodes = newSpilledNodes}
    doAssignColors (selectStack wlists')
  else do
    let newColoredNodes = (coloredNodes wlists) ++ [n]
    let c = S.findMin $ okColors wlists'
    let newColor = M.insert n c (color wlists')
    put wlists' {coloredNodes = newColoredNodes, color = newColor}
    doAssignColors (selectStack wlists')
  where
    updateColors :: Temp -> Allocator ()
    updateColors w = do
      wlists <- get
      wAlias <- getAlias w
      when (S.member wAlias (S.union (precolored wlists) (S.fromList $ coloredNodes wlists))) $ do
        let newOkColors = (okColors wlists) S.\\ (S.singleton $ (color wlists) M.! wAlias)
        put wlists {okColors = newOkColors}

rewriteProgram :: Allocator ()
rewriteProgram = undefined
  