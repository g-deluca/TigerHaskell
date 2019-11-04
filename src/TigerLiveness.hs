module TigerLiveness where

  import Assem
  import TigerGraph 
  import TigerMakeGraph
  import TigerTemp
  import Prelude hiding (succ)
  import qualified Data.Map.Strict as Map
  import qualified Data.Set as Set

  -- data InterferenceGraph a = InterferenceGraph {
  --   igraph :: Graph a,
  --   tnode :: Map.Map Temp (Node Instr),
  --   gtemp :: Map.Map (Node Instr) Temp,
  --   moves :: [(Node Instr, Node Instr)]
  -- }

  data NodeLiveness a = NodeLiveness {
    liveIn :: Set.Set a,
    liveOut :: Set.Set a
  }

  type LivenessMap a = Map.Map (Node a) (NodeLiveness a)


  mkLivenessMap :: FlowGraph a -> LivenessMap a
  mkLivenessMap (FlowGraph graph def use ismove) = 
    undefined    
    where 
      mkLiveIn :: (Node a) -> Set.Set Temp
      mkLiveIn node = Set.union (Set.fromList (use Map.! node)) (Set.difference (mkLiveOut node) (Set.fromList (def Map.! node)))
      mkLiveOut :: (Node a) -> Set.Set Temp
      mkLiveOut node = 
        Set.unions (Set.map liveIn (succ node graph))
      mkLivenessNode :: (Node a) -> (NodeLiveness a)
      mkLivenessNode node = NodeLiveness {liveIn = mkLiveIn node, liveOut = mkLiveOut node}
  