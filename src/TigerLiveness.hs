module TigerLiveness where

import           Assem
import qualified Data.Map.Strict     as Map
import qualified Data.Set            as Set
import           Prelude             as P hiding (succ)
import           TigerGraph          as G
import           TigerMakeGraph
import           TigerSymbol
import           TigerTemp

import           Control.Monad.State
import           Data.Maybe
import           Debug.Trace

data LivenessNode a =
  LivenessNode
    { liveIn  :: Set.Set a
    , liveOut :: Set.Set a
    }
  deriving (Eq, Ord, Show)

lookUp :: (Ord k, Show k, Show v) => Map.Map k v -> k -> String -> v
lookUp m k errorMsg =
  maybe
    (error $ ">>> lookUp " ++ errorMsg ++ " --> " ++ show k ++ " in: " ++ show m)
    id $
  Map.lookup k m

emptyLivenessNode :: LivenessNode Temp
emptyLivenessNode = LivenessNode {liveIn = Set.empty, liveOut = Set.empty}

type LivenessMap = Map.Map (Node Instr) (LivenessNode Temp)

mkEmptyLivenessMap :: FlowGraph Instr -> LivenessMap
mkEmptyLivenessMap flowGraph@(FlowGraph graph def use ismove) =
  let nGraph = nodes graph -- los nodos del grafo
      emptySets = map (\x -> emptyLivenessNode) [0 .. (P.length nGraph)] -- [{}, ... , {}]
   in Map.fromList $ zip (Set.toList (nodes graph)) emptySets

mkLivenessMap :: FlowGraph Instr -> State LivenessMap ()
mkLivenessMap flowGraph@(FlowGraph graph def use ismove) = do
  let mkLiveIn :: (Node Instr) -> (LivenessMap) -> (Set.Set Temp)
      mkLiveIn node allNodes =
        let currentLiveness = (allNodes Map.! node)
         in Set.union
              (Set.fromList $ use Map.! node)
              (Set.difference
                 (liveOut currentLiveness)
                 (Set.fromList (def Map.! node)))
      mkLiveOut :: (Node Instr) -> LivenessMap -> (Set.Set Temp)
      mkLiveOut node allNodes =
        let nodeSucc =
              Set.map (\succesor -> (allNodes Map.! succesor)) (succ node graph)
         in Set.unions $ Set.toList $ Set.map liveIn nodeSucc
      mkLivenessNode :: Node Instr -> State LivenessMap Bool
      mkLivenessNode node = do
        allNodes <- get
        let newLiveIn = mkLiveIn node allNodes
            newLiveOut = mkLiveOut node allNodes
            oldLiveIn = liveIn $ allNodes Map.! node
            oldLiveOut = liveOut $ allNodes Map.! node
        if (newLiveIn == oldLiveIn && newLiveOut == oldLiveOut)
          then return True
          else let updatedNodes =
                     Map.insert
                       node
                       (LivenessNode {liveIn = newLiveIn, liveOut = newLiveOut})
                       allNodes
                in put updatedNodes >> return False
  keepUpdating <- mapM mkLivenessNode $ Set.toList (nodes graph)
  if foldl (&&) True keepUpdating
    then return ()
    else mkLivenessMap flowGraph

calculateLiveness :: FlowGraph Instr -> LivenessMap
calculateLiveness flowGraph =
  execState (mkLivenessMap flowGraph) (mkEmptyLivenessMap flowGraph)

type InterferenceGraph = Graph Temp

buildIGraph :: FlowGraph Instr -> LivenessMap -> InterferenceGraph
buildIGraph fcg livenessMap =
  let fcgNodes = Set.elems $ nodes $ graph fcg
      unprocessedEdges = buildIGraphEdges fcgNodes fcg livenessMap
      -- Acá tenemos las aristas medio peladas, falta darle estructura
      allTemps =
        Set.elems $
        foldl
          (\accum (tA, tB) -> Set.insert tA (Set.insert tB accum))
          Set.empty
          unprocessedEdges
      -- Me armo un set con los temps cosa de poder agregar los nodos sin repetir ...
      (graphWithNodes, _) = addNodes allTemps (emptyGraph, [])
      -- ... y los agrego
      processedEdges =
        map
          (\(a, b) ->
             ( findNodeByContent a graphWithNodes
             , findNodeByContent b graphWithNodes))
          unprocessedEdges
   in addEdges processedEdges graphWithNodes

buildIGraphEdges ::
     [Node Instr] -> FlowGraph Instr -> LivenessMap -> [(Temp, Temp)]
buildIGraphEdges [] _ _ = []
buildIGraphEdges (thisNode:otherNodes) fcg livenessMap =
  let livenessNode = livenessMap Map.! thisNode
      out = Set.elems $ liveOut $ livenessNode
      defs = (def fcg) Map.! thisNode
      uses = (use fcg) Map.! thisNode
      isMove = (ismove fcg) Map.! thisNode
      aristas =
        if isMove
          then [(x, y) | x <- defs, y <- filter (/= (head $ uses)) out]
          else [(x, y) | x <- defs, y <- out]
   in aristas ++ buildIGraphEdges otherNodes fcg livenessMap

calculateInterferenceGraph :: FlowGraph Instr -> InterferenceGraph
calculateInterferenceGraph fcg =
  let livenessMap = calculateLiveness fcg
   in buildIGraph fcg livenessMap
