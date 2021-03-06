module TigerLiveness where

import           Assem
import qualified Data.Map.Strict     as Map
import qualified Data.Set            as Set
import           Prelude             as P hiding (succ)
import qualified TigerGraph          as G
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

type LivenessMap = Map.Map (G.Node Instr) (LivenessNode Temp)

mkEmptyLivenessMap :: FlowGraph Instr -> LivenessMap
mkEmptyLivenessMap flowGraph@(FlowGraph graph def use ismove) =
  let nGraph = G.nodes graph -- los nodos del grafo
      emptySets = map (\x -> emptyLivenessNode) [0 .. (P.length nGraph)] -- [{}, ... , {}]
   in Map.fromList $ zip (Set.toList (G.nodes graph)) emptySets

mkLivenessMap :: FlowGraph Instr -> State LivenessMap ()
mkLivenessMap flowGraph@(FlowGraph graph def use ismove) = do
  let mkLiveIn :: (G.Node Instr) -> (LivenessMap) -> (Set.Set Temp)
      mkLiveIn node allNodes =
        let currentLiveness = (allNodes Map.! node)
         in Set.union
              (Set.fromList $ use Map.! node)
              (Set.difference
                 (liveOut currentLiveness)
                 (Set.fromList (def Map.! node)))
      mkLiveOut :: (G.Node Instr) -> LivenessMap -> (Set.Set Temp)
      mkLiveOut node allNodes =
        let nodeSucc =
              P.map
                (\succesor -> (allNodes Map.! succesor))
                (Set.toList (G.succ node graph))
         in Set.unions $ P.map liveIn nodeSucc
      mkLivenessNode :: G.Node Instr -> State LivenessMap Bool
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
  keepUpdating <- mapM mkLivenessNode $ Set.toList (G.nodes graph)
  if foldl (&&) True keepUpdating
    then return ()
    else mkLivenessMap flowGraph

calculateLiveness :: FlowGraph Instr -> LivenessMap
calculateLiveness flowGraph =
  execState (mkLivenessMap flowGraph) (mkEmptyLivenessMap flowGraph)

-- TODO: De acá para abajo debería ser un módulo nuevo
--
--
-- Simplifico el grafo de esta forma para que sea más fácil usarlo en la etapa siguiente
-- Podría modificarse directamente el grafo, porque la interfaz que estamos usando solamente
-- mete ruido y no nos dio ventajas; pero parto desde acá para no tener que refactorizar tanto
-- código.
data InterferenceGraph =
  IGraph
    { nodes :: Set.Set Temp
    , edges :: Set.Set (Temp, Temp)
    }

-- Esto es medio garrón porque queda repetida la interfaz pero ya fue
pred, succ, adj :: Temp -> InterferenceGraph -> Set.Set Temp
pred t igraph =
  Set.foldr
    (\(src, dst) accum ->
       if dst == t
         then Set.insert src accum
         else accum)
    Set.empty
    (edges igraph)

-- No bueno, Maxi se mata si ve este código
succ t igraph =
  Set.foldr
    (\(src, dst) accum ->
       if src == t
         then Set.insert dst accum
         else accum)
    Set.empty
    (edges igraph)

adj t igraph = Set.union (TigerLiveness.pred t igraph) (succ t igraph)

removeNode :: Temp -> InterferenceGraph -> InterferenceGraph
removeNode t igraph =
  let oldNodes = nodes igraph
      newNodes = oldNodes `Set.difference` (Set.singleton t)
      oldEdges = edges igraph
      newEdges = Set.filter (\(src, dst) -> t /= src && t /= dst) oldEdges
   in IGraph {nodes = newNodes, edges = newEdges}

instance Show InterferenceGraph where
  show (IGraph _ edges) = show $ Set.elems edges

buildIGraph :: FlowGraph Instr -> LivenessMap -> InterferenceGraph
buildIGraph fcg livenessMap =
  let fcgNodes = Set.elems $ G.nodes $ graph fcg
      newEdges = buildIGraphEdges fcgNodes fcg livenessMap
      newNodes =
        Set.fromList $ concat $ Map.elems (def fcg) ++ Map.elems (use fcg)
   in IGraph {nodes = newNodes, edges = Set.fromList newEdges}

buildIGraphEdges ::
     [G.Node Instr] -> FlowGraph Instr -> LivenessMap -> [(Temp, Temp)]
buildIGraphEdges [] _ _ = []
buildIGraphEdges (thisNode:otherNodes) fcg livenessMap =
  let livenessNode = livenessMap Map.! thisNode
      out = Set.elems $ liveOut $ livenessNode
      defs = (def fcg) Map.! thisNode
      uses = (use fcg) Map.! thisNode
      isMove = (ismove fcg) Map.! thisNode
      aristas =
        if isMove
          -- Since this is a move instruction, we now that uses is a list of one element
          then [(x, y) | x <- defs, y <- filter (/= (head $ uses)) out]
          else [(x, y) | x <- defs, y <- out]
   in aristas ++ buildIGraphEdges otherNodes fcg livenessMap

calculateInterferenceGraph :: FlowGraph Instr -> InterferenceGraph
calculateInterferenceGraph fcg =
  let livenessMap = calculateLiveness fcg
   in buildIGraph fcg livenessMap
