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
