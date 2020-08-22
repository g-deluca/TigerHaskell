-- module TigerAllocation where

-- import Assem
-- import TigerTemp
-- import TigerGraph
-- import TigerMakeGraph
-- import TigerLiveness
-- import TigerFrame
-- import TigerUnique
-- import TigerSymbol

-- import qualified Data.Map as M
-- import Data.Set as S
-- import Data.List as L
-- import Control.Monad.State
-- import Debug.Trace

-- data Worklists = Worklists {

--     -- TODO: ya que en build() tenemos flowGraph y LivenessMap, armamos el interferencaGraph y lo tenemos acá?

--     -- Custom
--     -- lista de colores disponibles
--     okColors :: S.Set Temp,
--     -- Node worklists, sets and stacks. The following
--     -- lists and sets are always mutually disjoint and every node is always
--     -- in exactly on of the sets or lists.

--     -- machine registers, preassigned a color
--     precolored :: S.Set Temp,  -- TODO: Leer un poco más sobre esto xd
--     -- temporary registers, not precolored and not yet processed
--     initial :: S.Set Temp,
--     -- list of low-degree non-move-related nodes
--     simplifyWorklist :: [Temp],
--     -- low-degree move-related nodes
--     freezeWorklist :: [Temp],
--     -- high-degree nodes
--     spillWorklist :: [Temp],
--     -- nodes marked for spilling during this round: initially empty
--     spilledNodes :: [Temp],
--     -- registers that have been coalesced
--     -- when u <- v is coalesced, v is added to this set and u put back in some work-list (or viceversa)
--     coalescedNodes :: [Temp],
--     -- nodes suceessfully colored
--     coloredNodes :: [Temp],
--     -- stack containing temporaries removed from the graph
--     selectStack :: [Temp],

--     -- Move sets. 5 sets of move instructions, and every Move is in exactly one of these
--     -- (after Build through the end of Main)

--     -- moves that have been coalesced
--     coalescedMoves :: [Node Instr],
--     -- moves whose source and target interfere
--     constrainedMoves :: [Node Instr],
--     -- moves that will no longer be considered for coalescing
--     frozenMoves :: [Node Instr],
--     -- moves enabled for possible coalescing
--     worklistMoves :: [Node Instr],
--     -- moves not yet ready for coalescing
--     activeMoves :: [Node Instr],

--     -- the set of interference edges (u,v) in the graph. if (u,v) \in adjSet => (v,u) \in adjSet
--     adjSet :: S.Set (Temp, Temp),
--     -- adjacency list representation of the graph.
--     -- for each non-precolored temporary u, adjList[u] is the set of nodes that interfere with u.
--     adjList :: M.Map Temp (S.Set Temp),
--     -- the current degree of each node
--     degree :: M.Map Temp Int,
--     -- a mapping from a node to the list of moves it is associated with
--     moveList :: M.Map Temp [Node Instr],
--     -- when a move (u, v) has been coalesced, and v put in coalescedNodes,
--     -- alias[v] = u
--     alias :: M.Map Temp Temp,
--     -- the color chosen by the algorithm for a node;
--     -- for precolored nodes this is initialized to the given color
--     color :: M.Map Temp Temp

-- }

-- instance Show (Worklists) where
--   show (Worklists _ precolored initial _ _ _ _ _ _ _ _ _ _ _ _ adjSet _ _ _ _ color) =
--     "Precolored: " ++ show precolored  ++ "\n Initial: " ++ show initial

-- (!) :: (Ord k, Show k, Show v) => M.Map k v -> k -> v
-- m ! k =
--   maybe (error $ "No encontrada la clave: " ++ show k ++ " in: " ++ show m) id
--     $ M.lookup k m

-- colors :: [Temp]
-- colors = callersaves ++ calleesaves ++ argregs

-- precolors :: [Temp]
-- precolors = allRegs

-- k :: Int
-- k = L.length colors


-- setOkColors :: Allocator ()
-- setOkColors = do
--   wlists <- get
--   put wlists { okColors = S.fromList colors }

-- build :: FlowGraph Instr -> LivenessMap -> Allocator ()
-- build flowGraph livenessMap =
--     let fnodes = TigerGraph.nodes $ graph flowGraph
--         moveNodes = ismove flowGraph
--         instrs = L.map (\(Node _ instr) -> instr) $ S.toList fnodes
--         buildNode :: Node Instr -> Allocator ()
--         buildNode node = do
--             let live = liveOut $ (lookUp livenessMap node "buildNode 1")
--                 live' = if (lookUp moveNodes node "buildNode 1.5")
--                             then S.difference live (S.fromList (lookUp (use flowGraph) node "buildNode 2"))
--                             else live
--             when (lookUp moveNodes node "buildNode 3") $ do
--                 -- trace ("when moveNodes " ++ show moveNodes)
--                 -- forall n \in def(I) \union use(I)
--                 let defAndUse = (lookUp (def flowGraph) node "buildNode 4") ++ (lookUp (use flowGraph) node "buildNode 5")
--                 mapM_ (flip buildMoveList node) defAndUse

--                 wlists <- get
--                 put $ wlists { worklistMoves = node:worklistMoves wlists}
--             let live'' = S.union live' (S.fromList (lookUp (def flowGraph) node "buildNode 6"))
--             -- mapM_ (\d -> mapM_ (\l -> insertDegree d l) live'') (S.fromList ((def flowGraph) M.! node))
--             mapM_ (\d -> mapM_ (\l -> addEdge d l) live'') (S.fromList (lookUp (def flowGraph) node "buildNode 7"))


--         buildMoveList :: Temp -> Node Instr -> Allocator ()
--         buildMoveList t node = do
--           wlists <- get
--           let newMoveList = M.insertWith (++) t [node] (moveList wlists)
--           -- trace ("buildMoveList " ++ show t ++ " - " ++ show node)
--           put $ wlists { moveList = newMoveList}
--     in insertDegrees (getTemps instrs) >> mapM_ buildNode fnodes

-- insertDegrees :: [Temp] -> Allocator ()
-- insertDegrees temps = 
--   mapM_ insertDegree temps

-- insertDegree :: Temp -> Allocator ()
-- insertDegree t = do
--   wlists <- get
--   let newDegree = M.insert t 0 (degree wlists)
--   put wlists {degree = newDegree}

-- addEdge :: Temp -> Temp -> Allocator ()
-- addEdge d l = do
--     trace (">>> addEdge " ++ show d ++ " -- " ++ show l) (return ())
--     wlists <- get
--     when (S.notMember (d, l) (adjSet wlists) && d /= l) $ do
--       let newAdjSet = S.insert (d, l) $ S.insert (l, d) (adjSet wlists)

--           newAdjList = if (S.notMember d (precolored wlists))
--             then M.insertWith S.union d (S.singleton l) (adjList wlists)
--             else adjList wlists
--           newDegree = if (S.notMember d (precolored wlists))
--             then M.insertWith (+) d 1 (degree wlists)
--             else degree wlists

--           newAdjList' = if (S.notMember l (precolored wlists))
--             then M.insertWith S.union l (S.singleton d) newAdjList
--             else newAdjList
--           newDegree' = if (S.notMember l (precolored wlists))
--             then M.insertWith (+) l 1 newDegree
--             else newDegree
--       put $ wlists { adjSet = newAdjSet, adjList = newAdjList', degree = newDegree' }

-- makeWorklist :: Allocator ()
-- makeWorklist = do
--   wlists <- get
--   trace ("makeWorklist initial: " ++ show (initial wlists)) $ return ()
--   trace ("makeWorklist degree: " ++ show (degree wlists)) $ return ()
--   mapM_ makeWorklistStep (initial wlists)
--   where
--     makeWorklistStep :: Temp -> Allocator ()
--     makeWorklistStep n = do
--       wlists <- get
--       moveRelatedBool <- moveRelated n
--       let newInitial = initial wlists `S.difference` S.singleton n
--       if degree wlists ! n >= k
--         then do
--           let newSpillWorklist = spillWorklist wlists ++ [n]
--           put $ wlists { initial = newInitial, spillWorklist = newSpillWorklist }
--         else if moveRelatedBool
--           then do
--             let newFreezeWorklist = freezeWorklist wlists ++ [n]
--             put $ wlists { initial = newInitial, freezeWorklist = newFreezeWorklist }
--           else do
--             let newSimplifyWorklist = simplifyWorklist wlists ++ [n]
--             put $ wlists { initial = newInitial, simplifyWorklist = newSimplifyWorklist }


-- moveRelated :: Temp -> Allocator Bool
-- moveRelated t = do
--   nodeSet <- nodeMoves t
--   return (nodeSet /= S.empty)

-- nodeMoves :: Temp -> Allocator (S.Set (Node Instr))
-- nodeMoves t = do
--   wlists <- get
--   return $
--     S.fromList (maybe [] id (moveList wlists M.!? t)) `S.intersection` S.fromList (activeMoves wlists ++ worklistMoves wlists)

-- adjacent :: Temp -> Allocator [Temp]
-- adjacent t = do
--   wlists <- get
--   trace ("adjacent 1: " ++ (show $ adjList wlists)) (return ())
--   trace ("adjacent 2: " ++ (show $ selectStack wlists)) (return ())
--   trace ("adjacent 1: " ++ (show $ adjList wlists)) $ return $ S.toList $ (adjList wlists ! t) `S.difference` (S.fromList (selectStack wlists) `S.union` S.fromList (coalescedNodes wlists))

-- simplify :: Allocator ()
-- simplify = do
--   wlists <- get
--   let n = head $ simplifyWorklist wlists
--       newSimplifyWorklist = tail $ simplifyWorklist wlists
--       newSelectStack = n:(selectStack wlists)
--   put $ wlists {simplifyWorklist = newSimplifyWorklist, selectStack = newSelectStack}
--   adj <- adjacent n
--   mapM_ decrementDegree adj

-- decrementDegree :: Temp -> Allocator ()
-- decrementDegree t = do
--   wlists <- get
--   adj <- adjacent t
--   isMoveRelated <- moveRelated t
--   let d = degree wlists ! t
--       newDegree = M.insert t (d-1) (degree wlists)
--       newSpillWorklist = if (d == k) then L.delete t (spillWorklist wlists) else spillWorklist wlists
--   when (d == k) $ do
--     enableMoves $ adj ++ [t]
--     if isMoveRelated then
--       let newFreezeWorklist = (freezeWorklist wlists) ++ [t]
--       in put wlists { degree = newDegree, spillWorklist = newSpillWorklist, freezeWorklist = newFreezeWorklist}
--     else
--       let newSimplifyWorklist = (simplifyWorklist wlists) ++ [t]
--       in put wlists { degree = newDegree, spillWorklist = newSpillWorklist, simplifyWorklist = newSimplifyWorklist}
--   return ()

-- enableMoves :: [Temp] -> Allocator ()
-- enableMoves temps = do
--   mapM_ doEnableMoves temps
--   where
--     doEnableMoves :: Temp -> Allocator ()
--     doEnableMoves t = do
--       nMoves <- nodeMoves t
--       mapM_ doUpdate (S.toList nMoves)
--     doUpdate :: Node Instr -> Allocator ()
--     doUpdate n = do
--       wlists <- get
--       when (n `L.elem` (activeMoves wlists)) $ do
--         let newActiveMoves = n `L.delete` (activeMoves wlists)
--             newWorklistMoves = (worklistMoves wlists) ++ [n]
--         put wlists {activeMoves = newActiveMoves, worklistMoves = newWorklistMoves}

-- coalesce :: Allocator ()
-- coalesce = do
--   wlists <- get
--   let m@(Node _ (move)) = head $ worklistMoves wlists
--   trace ("coalesce 0: " ++ show m) (return ())
--   x <- getAlias $ msrc move
--   y <- getAlias $ mdst move
--   trace ("coalesce 1: " ++ show x ++ " - " ++ show y) (return ())
--   let (u, v) = if L.elem y (precolored wlists) then (y,x) else (x,y)
--   let newWorklistMoves = L.filter (/=m) (worklistMoves wlists)
--   adjU <- adjacent u
--   adjV <- adjacent v
--   isConservative <- conservative (adjU ++ adjV)
--   isOk <- checkOk v u
--   trace ("coalesce 2: " ++ show isOk) (return ())
--   if u == v then do
--     addCoalescedMoves m
--     addWorkList u
--   else  if L.elem v (precolored wlists) && S.member (u,v) (adjSet wlists) then do
--           addConstrainedMoves m
--           addWorkList u
--           addWorkList v
--         else  if  L.elem u (precolored wlists) && isOk ||
--                   L.notElem u (precolored wlists) && isConservative then do
--                 addCoalescedMoves m
--                 combine u v
--                 addWorkList u
--               else do
--                 addActiveMoves m
--   where
--     addCoalescedMoves :: Node Instr -> Allocator ()
--     addCoalescedMoves m = do
--       wlists <- get
--       let newCoalescedMoves = (coalescedMoves wlists) ++ [m]
--       put wlists {coalescedMoves = newCoalescedMoves}
--     addConstrainedMoves :: Node Instr -> Allocator ()
--     addConstrainedMoves m = do
--       wlists <- get
--       let newConstrainedMoves = (constrainedMoves wlists) ++ [m]
--       put wlists {constrainedMoves = newConstrainedMoves}
--     checkOk :: Temp -> Temp -> Allocator Bool
--     checkOk v u = do
--       wlists <- get
--       adj <- adjacent v
--       foldM (\accum a -> ok a u) True adj
--     addActiveMoves :: Node Instr -> Allocator ()
--     addActiveMoves m = do
--       wlists <- get
--       let newActiveMoves = (activeMoves wlists) ++ [m]
--       put wlists {activeMoves = newActiveMoves}


-- addWorkList :: Temp -> Allocator ()
-- addWorkList u = do
--   wlists <- get
--   isMoveRelated <- moveRelated u
--   when (S.notMember u (precolored wlists) && not (isMoveRelated) && (degree wlists ! u) < k) $ do
--     let newFreezeWorklist = L.filter (/=u) (freezeWorklist wlists)
--         newSimplifyWorklist = (simplifyWorklist wlists) ++ [u]
--     put wlists {freezeWorklist = newFreezeWorklist, simplifyWorklist = newSimplifyWorklist}

-- ok :: Temp -> Temp -> Allocator Bool
-- ok t r = do
--   wlists <- get
--   let tDegree = (degree wlists) ! t
--       isPrecolored = S.member t (precolored wlists)
--       isAdjSet = S.member (t, r) (adjSet wlists)
--   return $ (tDegree < k) || isPrecolored || isAdjSet

-- conservative :: [Temp] -> Allocator Bool
-- conservative nodes = do
--   wlists <- get
--   let i = L.foldl (\a n -> if (degree wlists) ! n >= k then a+1 else a) 0 (nodes)
--   return (i < k)

-- getAlias :: Temp -> Allocator Temp
-- getAlias n = do
--   wlists <- get
--   if L.elem n (coalescedNodes wlists) then do
--     let nAlias = (alias wlists) ! n
--     getAlias nAlias
--   else
--     return n

-- combine :: Temp -> Temp -> Allocator ()
-- combine u v = do
--   wlists <- get
--   adj <- adjacent v
--   let
--     newFreezeWorklist = if L.elem v (freezeWorklist wlists) then L.filter (/=v) (freezeWorklist wlists) else freezeWorklist (wlists)
--     newSpillWorklist = if L.notElem v (freezeWorklist wlists) then L.filter (/=v) (spillWorklist wlists) else spillWorklist (wlists)
--     newCoalescedNodes = (coalescedNodes wlists) ++ [v]
--     newAlias = M.insert v u (alias wlists)
--     newMovelist = M.insert u (((moveList wlists) ! u) ++ ((moveList wlists) ! v)) (moveList wlists)
--   mapM_ (\t -> do addEdge t v >> decrementDegree t) adj
--   when (degree wlists ! u >= k && L.elem u (freezeWorklist wlists)) $ do
--     let newFreezeWorklist' = L.filter (/=u) newFreezeWorklist
--         newSpillWorklist' = newSpillWorklist ++ [u]
--     put wlists {freezeWorklist = newFreezeWorklist', spillWorklist = newSpillWorklist',
--                 coalescedNodes = newCoalescedNodes, alias = newAlias, moveList = newMovelist}

-- freeze :: Allocator ()
-- freeze = do
--   wlists <- get
--   let u = head $ freezeWorklist wlists
--   let newFreezeWorklist = tail $ freezeWorklist wlists
--   let newSimplifyWorklist = simplifyWorklist wlists ++ [u]
--   put wlists {freezeWorklist = newFreezeWorklist, simplifyWorklist = newSimplifyWorklist}
--   freezeMoves u

-- freezeMoves :: Temp -> Allocator ()
-- freezeMoves u = do
--   nMoves <- nodeMoves u
--   mapM_ doFreezeMoves nMoves
--     where
--       doFreezeMoves :: Node Instr -> Allocator ()
--       doFreezeMoves m@(Node _ (move)) = do
--         wlists <- get
--         let x = msrc move
--             y = mdst move
--         xAlias <- getAlias x
--         yAlias <- getAlias y
--         uAlias <- getAlias u
--         let v = if yAlias == uAlias then xAlias else yAlias
--         vMoves <- nodeMoves v
--         let newActiveMoves = L.filter (/=m) (activeMoves wlists)
--         let newFrozenMoves = (frozenMoves wlists) ++ [m]
--         when (S.null vMoves && ((degree wlists) ! v) < k) $ do
--           let newFreezeWorklist = L.filter (/=v) (freezeWorklist wlists)
--           let newSimplifyWorklist = (simplifyWorklist wlists) ++ [v]
--           put wlists {activeMoves = newActiveMoves, frozenMoves = newFrozenMoves,
--                       simplifyWorklist = newSimplifyWorklist, freezeWorklist = newFreezeWorklist}

-- selectSpill :: Allocator ()
-- selectSpill = do
--   wlists <- get
--   let m = head $ spillWorklist wlists -- heuristica??
--       newSpillWorklist = tail $ spillWorklist wlists
--       newSimplifyWorklist = (simplifyWorklist wlists) ++ [m]
--   freezeMoves m

-- assignColors :: Allocator ()
-- assignColors = do
--   wlists <- get
--   doAssignColors (selectStack wlists)
--   mapM_ doColor (coalescedNodes wlists)
--     where
--       doColor :: Temp -> Allocator ()
--       doColor n = do
--         wlists <- get
--         nAlias <- getAlias n
--         let newColor = M.insert n ((color wlists) ! nAlias) (color wlists)
--         put wlists {color = newColor}

-- -- este seria el while del libro
-- doAssignColors :: [Temp] -> Allocator ()
-- doAssignColors [] = return ()
-- doAssignColors sStack = do
--   wlists <- get
--   let n = head $ sStack
--       newSelectStack = tail $ sStack
--   put wlists {selectStack = newSelectStack}
--   setOkColors
--   mapM_ updateColors (S.toList $ (adjList wlists) ! n)
--   wlists' <- get
--   if (S.null $ okColors wlists') then do
--     let newSpilledNodes = (spilledNodes wlists') ++ [n]
--     put wlists' {spilledNodes = newSpilledNodes}
--     doAssignColors (selectStack wlists')
--   else do
--     let newColoredNodes = (coloredNodes wlists) ++ [n]
--     let c = S.findMin $ okColors wlists'
--     let newColor = M.insert n c (color wlists')
--     put wlists' {coloredNodes = newColoredNodes, color = newColor}
--     doAssignColors (selectStack wlists')
--   where
--     updateColors :: Temp -> Allocator ()
--     updateColors w = do
--       wlists <- get
--       wAlias <- getAlias w
--       when (S.member wAlias (S.union (precolored wlists) (S.fromList $ coloredNodes wlists))) $ do
--         let newOkColors = (okColors wlists) S.\\ (S.singleton $ (color wlists) ! wAlias)
--         put wlists {okColors = newOkColors}

-- rewriteProgram :: [Instr] -> Frame -> Allocator ([Instr],Frame)
-- rewriteProgram instr frame = do
--   wlists <- get
--   -- Allocate memory locations for each spilledNodes
--   (newInstr, newFrame) <- doSpill (spilledNodes wlists) instr frame
--   let newInitial = S.fromList $ (coloredNodes wlists) ++ (coalescedNodes wlists) ++ (spilledNodes wlists)
--   put wlists {spilledNodes = [], initial = newInitial, coloredNodes = [], coalescedNodes = []}
--   return (newInstr, newFrame)
--     where
--       doSpill :: [Temp] -> [Instr] -> Frame -> Allocator ([Instr], Frame)
--       doSpill [] instr frame = return (instr, frame)
--       doSpill (t:temps) instr frame = do
--         (newInstr, newFrame) <- spillOneTemp t instr frame
--         doSpill temps newInstr newFrame


--       spillOneTemp :: Temp -> [Instr] -> Frame -> Allocator ([Instr], Frame)
--       spillOneTemp t [] frame = return ([], frame {actualReg = actualReg frame + 1})
--       spillOneTemp t ((Move assem dst src):instr) frame = do
--         let def = t == dst
--             use = t == src
--         (spilledInstr, newFrame) <- spillOneTemp t instr frame
--         if def
--           then do
--             freshTemp <- newTemp
--             return ([Move assem freshTemp src] ++ [(push freshTemp)] ++ spilledInstr, newFrame)
--           else if use
--             then do
--               freshTemp <- newTemp
--               return ([(pop freshTemp)] ++ [Move assem dst freshTemp] ++  spilledInstr, newFrame)
--             else return ([Move assem dst src] ++ spilledInstr, newFrame)

--       spillOneTemp t ((Oper assem dst src jmp):instr) frame = do
--         let def = elem t dst
--             use = elem t src
--         (spilledInstr, newFrame) <- spillOneTemp t instr frame
--         if def
--           then do
--             freshTemp <- newTemp
--             let newDst = freshTemp : (L.filter (/=t) dst)
--             return ([Oper assem newDst src jmp] ++ [(push freshTemp)] ++ spilledInstr, newFrame)
--           else if use
--             then do
--               freshTemp <- newTemp
--               let newSrc = freshTemp : (L.filter (/=t) src)
--               return ([(pop freshTemp)] ++ [Oper assem dst newSrc jmp] ++  spilledInstr, newFrame)
--             else return ([Oper assem dst src jmp] ++ spilledInstr, newFrame)

--       spillOneTemp t (label:instr) frame = do
--         (spilledInstr, newFrame) <- spillOneTemp t instr frame
--         return (label:spilledInstr, newFrame)

--       pop :: Temp -> Instr
--       pop t = Oper {
--         oassem = "popq d0\n",
--         osrc = [],
--         odst = [t, sp],
--         ojump = Nothing
--       }

--       push :: Temp -> Instr
--       push t = Oper {
--           oassem = "pushq s0\n",
--           osrc = [t],
--           odst = [sp],
--           ojump = Nothing
--       }

-- allocate :: [Instr] -> Frame -> Allocator ()
-- allocate instr frame = do
--   let flowGraph = instrs2graph instr
--       livenessMap = calculateLiveness flowGraph
--   build flowGraph livenessMap
--   wlists <- get
--   -- trace ("$$$ After build: " ++ show (moveList wlists)) (return ())
--   makeWorklist
--   repeatAllocate
--   -- assignColors
--   -- wlists <- get
--   -- when (spilledNodes wlists /= []) $ do
--   --   (newInstr, newFrame) <- rewriteProgram instr frame
--   --   allocate newInstr newFrame
--     where
--       repeatAllocate :: Allocator ()
--       repeatAllocate = do
--         step
--         condition <- check
--         if condition then return () else repeatAllocate
--       step :: Allocator ()
--       step = do
--         wlists <- get
--         if trace ("simplify test " ++ show ((simplifyWorklist wlists) /= []) ++"\nOHSI--->" ++ show wlists) (simplifyWorklist wlists) /= [] then trace ("simplify\n") simplify
--               else  if trace ("worklistMoves test " ++ show ((worklistMoves wlists) /= []) ) (worklistMoves wlists) /= [] then trace ("coalesce") coalesce
--                     else  if trace ("freezeWorklist test " ++ show ((freezeWorklist wlists) /= [])) (freezeWorklist wlists) /= [] then trace ("freeze\n")  freeze
--                           else  if trace ("spillWorklist test") (spillWorklist wlists) /= [] then trace ("selectSpill\n")  selectSpill
--                                 else trace ("Termine\n")  return ()
--       check :: Allocator Bool
--       check = do
--         wlists <- get
--         return ((simplifyWorklist wlists) == [] && (worklistMoves wlists) == [] &&
--                 (freezeWorklist wlists) == [] && (spillWorklist wlists) == [])


-- replaceTemps :: [Instr] -> Allocator [Instr]
-- replaceTemps instrs = do
--   wlists <- get
--   let mapColors = (color wlists)
--       newInstr = L.map (\instr -> replaceTemps' instr mapColors) instrs
--   return newInstr

-- replaceTemps' :: Instr -> M.Map Temp Temp -> Instr
-- replaceTemps' (Move str src dst) color =
--   let newStr = applyTemps "s" 0 str [src] color
--       newStr' = applyTemps "d" 0 newStr [dst] color
--   in (Move newStr' src dst)
-- replaceTemps' (Oper str src dst jump) color =
--   let newStr = applyTemps "s" 0 str src color
--       newStr' = applyTemps "d" 0 newStr dst color
--   in (Oper newStr' src dst jump)
-- replaceTemps' label@(Label _ _) _ = label

-- applyTemps :: String -> Integer -> String -> [Temp] -> M.Map Temp Temp -> String
-- applyTemps prefijo indice instruccion [] colores = instruccion
-- applyTemps prefijo indice instruccion (temp:temps) colores =
--   let toReplace = pack (prefijo ++ show indice)
--       replaceWith = trace ("buscando clave " ++ unpack temp ++ " en el mapa " ++ (show colores))  (colores ! temp)
--       recursive = applyTemps prefijo (indice + 1) instruccion temps colores

--   in unpack $ replace toReplace replaceWith (pack recursive)

-- doAllocate :: [Instr] -> Frame -> Allocator [Instr]
-- doAllocate instrs frame = do
--   allocate instrs frame
--   replaceTemps instrs

-- type Allocator a = StateT Worklists StGen a

-- runAllocator :: [Instr] -> Frame -> StGen [Instr]
-- runAllocator instrs frame = flip evalStateT (initState $ getInitialTemps instrs) (doAllocate instrs frame)

-- getInitialTemps :: [Instr] -> S.Set Temp
-- getInitialTemps instr = getInitialTemps' instr S.empty

-- getInitialTemps' :: [Instr] -> S.Set Temp -> S.Set Temp
-- getInitialTemps' [] temps = temps
-- getInitialTemps' ((Move _ src dst):instrs) temps =
--   getTemp [src] `S.union` getTemp [dst] `S.union` (getInitialTemps' instrs temps) `S.union` temps
-- getInitialTemps' ((Oper _ src dst _):instrs) temps =
--   getTemp src `S.union` getTemp dst `S.union` (getInitialTemps' instrs temps) `S.union` temps
-- getInitialTemps' ((Label _ _):instrs) temps = S.union (getInitialTemps' instrs temps) temps

-- getTemp :: [Temp] -> S.Set Temp
-- getTemp [] = S.empty
-- getTemp (t:ts) =
--   case unpack t of
--     'T':rest -> (S.singleton t) `S.union` (getTemp ts)
--     _        -> getTemp ts


-- initState :: S.Set Temp -> Worklists
-- initState initialRegs = Worklists {
--     okColors = S.empty,
--     precolored = S.fromList precolors,
--     -- initial :: S.Set Temp,
--     initial = initialRegs,
--     -- simplifyWorklist :: [Temp],
--     simplifyWorklist = [],
--     -- freezeWorklist :: [Temp],
--     freezeWorklist = [],
--     -- spillWorklist :: [Temp],
--     spillWorklist = [],
--     -- spilledNodes :: [Temp],
--     spilledNodes = [],
--     -- coalescedNodes :: [Temp],
--     coalescedNodes = [],
--     -- coloredNodes :: [Temp],
--     coloredNodes = [],
--     -- selectStack :: [Temp],
--     selectStack = [],
--     -- coalescedMoves :: [Node Instr],
--     coalescedMoves = [],
--     -- constrainedMoves :: [Node Instr],
--     constrainedMoves = [],
--     -- frozenMoves :: [Node Instr],
--     frozenMoves = [],
--     -- worklistMoves :: [Node Instr],
--     worklistMoves = [],
--     -- activeMoves :: [Node Instr],
--     activeMoves = [],
--     -- adjSet :: S.Set (Temp, Temp),
--     adjSet = S.empty,
--     -- adjList :: M.Map Temp (S.Set Temp),
--     adjList = M.empty,
--     -- degree :: M.Map Temp Int,
--     degree = M.empty,
--     -- moveList :: M.Map Temp [Node Instr],
--     moveList = M.empty,
--     alias = M.empty,
--     color = M.fromList (zip precolors precolors)
-- }
