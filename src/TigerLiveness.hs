module TigerLiveness where

  import Assem
  import TigerSymbol
  import TigerGraph as G
  import TigerMakeGraph
  import TigerTemp
  import Prelude as P hiding (succ)
  import qualified Data.Map.Strict as Map
  import qualified Data.Set as Set

  import Control.Monad.State
  import Data.Maybe
  import Debug.Trace

  data LivenessNode a = LivenessNode {
    liveIn :: Set.Set a,
    liveOut :: Set.Set a
  } deriving (Eq, Ord, Show)

  lookUp :: (Ord k, Show k, Show v) => Map.Map k v -> k -> String -> v
  lookUp m k errorMsg =
    maybe (error $ ">>> lookUp " ++ errorMsg ++ " --> " ++ show k ++ " in: " ++ show m) id
      $ Map.lookup k m

  emptyLivenessNode :: LivenessNode Temp
  emptyLivenessNode = LivenessNode { liveIn = Set.empty, liveOut = Set.empty}

  type LivenessMap = Map.Map (Node Instr) (LivenessNode Temp)
  
  mkEmptyLivenessMap :: FlowGraph Instr -> LivenessMap
  mkEmptyLivenessMap flowGraph@(FlowGraph graph def use ismove) = 
    let nGraph = nodes graph -- los nodos del grafo
        emptySets = map (\x -> emptyLivenessNode) [0..(P.length nGraph)] -- [{}, ... , {}]
    in Map.fromList $ zip (Set.toList (nodes graph)) emptySets
  
  mkLivenessMap :: FlowGraph Instr -> State LivenessMap ()
  mkLivenessMap flowGraph@(FlowGraph graph def use ismove) = do
    let
      mkLiveIn :: (Node Instr) -> (LivenessMap) -> (Set.Set Temp)
      mkLiveIn node allNodes =
        let currentLiveness = (allNodes Map.! node)
        in Set.union (Set.fromList $ use Map.! node) (Set.difference (liveOut currentLiveness) (Set.fromList (def Map.! node)))

      mkLiveOut :: (Node Instr) -> LivenessMap -> (Set.Set Temp)
      mkLiveOut node allNodes =
        let
          nodeSucc = Set.map (\succesor -> (allNodes Map.! succesor)) (succ node graph)
        in Set.unions $ Set.toList $ Set.map liveIn nodeSucc

      mkLivenessNode :: Node Instr -> State LivenessMap Bool
      mkLivenessNode node = do 
        allNodes <- get
        let newLiveIn = mkLiveIn node allNodes
            newLiveOut = mkLiveOut node allNodes
            oldLiveIn = liveIn $ allNodes  Map.! node
            oldLiveOut = liveOut $ allNodes Map.! node
        if (newLiveIn == oldLiveIn && newLiveOut == oldLiveOut)
          then return True
          else  let updatedNodes = Map.insert node (LivenessNode {liveIn = newLiveIn, liveOut = newLiveOut}) allNodes
                in put updatedNodes >> return False
    
    keepUpdating <- mapM mkLivenessNode $ Set.toList (nodes graph)
    if foldl (&&) True keepUpdating then return () else mkLivenessMap flowGraph
    
  calculateLiveness :: FlowGraph Instr -> LivenessMap
  calculateLiveness flowGraph = execState (mkLivenessMap flowGraph) (mkEmptyLivenessMap flowGraph)


  data InterferenceGraph a = InterferenceGraph {
    igraph :: Graph a,
    tnode :: Map.Map Temp (Node Temp),
    gtemp :: Map.Map (Node Temp) Temp,
    moves :: [Instr]
  }

  emptyInterferenceGraph :: InterferenceGraph Temp
  emptyInterferenceGraph = InterferenceGraph { igraph = emptyGraph, tnode = Map.empty, gtemp = Map.empty, moves = []}

  interferenceGraph :: FlowGraph Instr -> LivenessMap -> (InterferenceGraph Temp)
  interferenceGraph (FlowGraph graph def use ismove) livenessMap =
    let graphNodes = (nodes graph)
        addInterferenceNodes :: (Node Instr) -> InterferenceGraph Temp -> InterferenceGraph Temp
        addInterferenceNodes node@(Node index item) iGraph =
          let defs = lookUp def node "fun: interferenceGraph-1"
              livenessNode = lookUp livenessMap node "fun: interferenceGraph-2"
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
              newTNode = Map.insert temp nodeTemp (Map.insert def nodeDef tNode)
              newGTemp = Map.insert nodeDef def (Map.insert nodeTemp temp gTemp)
              newIGraph = iGraph { igraph = newGraph'', tnode = newTNode, gtemp = newGTemp}
          in loopTemps def temps newIGraph
        loopNodes :: [Node Instr] -> InterferenceGraph Temp -> InterferenceGraph Temp
        loopNodes [] iGraph = iGraph
        loopNodes (n:nodes) iGraph =
          loopNodes nodes (addInterferenceNodes n iGraph)
    in (loopNodes (Set.toList graphNodes) emptyInterferenceGraph)

  calculateInterferenceGraph :: FlowGraph Instr -> InterferenceGraph Temp
  calculateInterferenceGraph flowGraph = interferenceGraph flowGraph (calculateLiveness flowGraph)

  -- Auxiliar xD
  maybeNodeIsMove :: Node Instr  -> InterferenceGraph Temp -> InterferenceGraph Temp
  maybeNodeIsMove (Node _ (Move a b c)) iGraph =
    let oldMoves = moves iGraph
    in iGraph { moves = (Move a b c):oldMoves}
  maybeNodeIsMove _ iGraph = iGraph