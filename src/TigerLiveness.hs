module TigerLiveness where

  import Assem
  import TigerGraph as G
  import TigerMakeGraph
  import TigerTemp
  import Prelude hiding (succ)
  import qualified Data.Map.Strict as Map
  import qualified Data.Set as Set

  import Control.Monad.State
  import Data.Maybe

  data LivenessNode a = LivenessNode {
    liveIn :: Set.Set a,
    liveOut :: Set.Set a
  } deriving (Eq, Ord)

  emptyLivenessNode :: LivenessNode Temp
  emptyLivenessNode = LivenessNode { liveIn = Set.empty, liveOut = Set.empty}

  type LivenessMap = Map.Map (Node Instr) (LivenessNode Temp)

  mkLivenessMap :: FlowGraph Instr -> State LivenessMap ()
  mkLivenessMap flowGraph@(FlowGraph graph def use ismove) = do
    let
      mkLiveIn :: (Node Instr) -> (LivenessMap) -> (Set.Set Temp)
      mkLiveIn node allNodes =
        let currentLiveness = fromMaybe emptyLivenessNode (Map.lookup node allNodes)
        in Set.union (Set.fromList (use Map.! node)) (Set.difference (liveOut currentLiveness) (Set.fromList (def Map.! node)))

      mkLiveOut :: (Node Instr) -> LivenessMap -> (Set.Set Temp)
      mkLiveOut node allNodes =
        let
          nodeSucc = Set.map (\succesor -> fromMaybe emptyLivenessNode (Map.lookup succesor allNodes)) (succ node graph)
        in Set.unions $ Set.toList $ Set.map liveIn nodeSucc

      mkLivenessNode :: Node Instr -> State LivenessMap Bool
      mkLivenessNode node = do
        allNodes <- get
        let newLiveIn = mkLiveIn node allNodes
            newLiveOut = mkLiveOut node allNodes
            oldLiveIn = liveIn $ fromMaybe emptyLivenessNode $ Map.lookup node allNodes
            oldLiveOut = liveOut $ fromMaybe emptyLivenessNode $ Map.lookup node allNodes
        if (newLiveIn == oldLiveIn && newLiveOut == oldLiveOut)
          then return True
          else  let updatedNodes = Map.insert node (LivenessNode {liveIn = newLiveIn, liveOut = newLiveOut}) allNodes
                in put updatedNodes >> return False
    
    keepUpdating <- mapM mkLivenessNode $ Set.toList (nodes graph)
    if foldl (&&) True keepUpdating then return () else mkLivenessMap flowGraph
    
  calculateLiveness :: FlowGraph Instr -> LivenessMap
  calculateLiveness flowGraph = execState (mkLivenessMap flowGraph) Map.empty


  data InterferenceGraph a = InterferenceGraph {
    igraph :: Graph a,
    tnode :: Map.Map Temp (Node Temp),
    gtemp :: Map.Map (Node Temp) Temp,
    moves :: [Instr]
  }

  emptyInterferenceGraph :: InterferenceGraph Temp
  emptyInterferenceGraph = InterferenceGraph { igraph = emptyGraph, tnode = Map.empty, gtemp = Map.empty, moves = []}

  interferenceGraph :: FlowGraph Instr -> State LivenessMap (InterferenceGraph Temp)
  interferenceGraph (FlowGraph graph def use ismove) = do
    livenessMap <- get
    let graphNodes = (nodes graph)
        addInterferenceNodes :: (Node Instr) -> InterferenceGraph Temp -> InterferenceGraph Temp
        addInterferenceNodes node@(Node index item) iGraph =
          let defs = def Map.! node
              livenessNode = livenessMap Map.! node
              temps = Set.toList $ Set.union (liveIn livenessNode) (liveOut livenessNode)
              iGraph' = maybeNodeIsMove node iGraph -- lo hago acá porque después pierdo `node`
          in loopDefs defs temps iGraph'
        loopDefs :: [Temp] -> [Temp] -> InterferenceGraph Temp -> InterferenceGraph Temp
        loopDefs [] _ iGraph = iGraph
        loopDefs (def:xs) temps iGraph = loopDefs xs temps (loopTemps def temps iGraph)
        loopTemps :: Temp -> [Temp] -> InterferenceGraph Temp -> InterferenceGraph Temp
        loopTemps _ [] iGraph = iGraph
        loopTemps def (temp:temps) iGraph =
          let graph = igraph iGraph
              tNode = tnode iGraph
              gTemp = gtemp iGraph
              nodeDef = mkNode def graph
              nodeTemp = mkNode temp graph
              newGraph = addNode def graph
              newGraph' = addNode temp newGraph
              newGraph'' = mkEdge nodeDef nodeTemp newGraph'
              newTNode = Map.insert def nodeTemp (Map.insert def nodeDef tNode)
              newGTemp = Map.insert nodeDef def (Map.insert nodeTemp def gTemp)
              newIGraph = iGraph { igraph = newGraph'', tnode = newTNode, gtemp = newGTemp}
          in loopTemps def temps newIGraph
        loopNodes :: [Node Instr] -> InterferenceGraph Temp -> InterferenceGraph Temp
        loopNodes [] iGraph = iGraph
        loopNodes (n:nodes) iGraph =
          loopNodes nodes (addInterferenceNodes n iGraph)
    return (loopNodes (Set.toList graphNodes) emptyInterferenceGraph)
          

  -- Auxiliar xD
  maybeNodeIsMove :: Node Instr  -> InterferenceGraph Temp -> InterferenceGraph Temp
  maybeNodeIsMove (Node _ (Move a b c)) iGraph =
    let oldMoves = moves iGraph
    in iGraph { moves = (Move a b c):oldMoves}