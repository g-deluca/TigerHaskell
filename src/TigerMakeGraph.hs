module TigerMakeGraph where

import           Assem
import           Data.Maybe
import           Debug.Trace
  -- http://hackage.haskell.org/package/containers-0.6.2.1/docs/Data-Map-Strict.html

import           TigerGraph
import           TigerTemp

import qualified Data.Map.Strict as Map
import           Data.Set        as S

data FlowGraph a =
  FlowGraph
    { graph  :: Graph a
    -- a table of the temporaries defined at each node (destination registers of the instruction)
    , def    :: Map.Map (Node Instr) [Temp] -- src
    -- a table of the temporaries used at each node (source registers of the instruction)
    , use    :: Map.Map (Node Instr) [Temp] -- dst
    -- tells wether each instruction is a MOVE
    , ismove :: Map.Map (Node Instr) Bool
    } deriving Show

emptyFCG :: FlowGraph a
emptyFCG =
  FlowGraph
    {graph = emptyGraph, def = Map.empty, use = Map.empty, ismove = Map.empty}

insertNodesToFCG ::
     [Instr] -> FlowGraph Instr -> (FlowGraph Instr, [Node Instr])
insertNodesToFCG instrs fcg = (fcg {graph = newGraph}, insertedNodes)
  where
    oldGraph = graph fcg
    (newGraph, insertedNodes) = addNodes instrs (oldGraph, [])

findNodeWithLabel :: Label -> [Node Instr] -> Node Instr
findNodeWithLabel label [] = error "Error en la busqueda del nodo"
findNodeWithLabel label (node@(Node _ (Label _ llab)):xs) =
  if label == llab
    then node
    else findNodeWithLabel label xs
findNodeWithLabel label (_:xs) = findNodeWithLabel label xs

processNodes :: [Node Instr] -> FlowGraph Instr -> FlowGraph Instr
processNodes [] fcg = fcg
processNodes [lastNode] fcg = processLastNode lastNode fcg
processNodes (thisNode@(Node _ (Label _ _)):instrs) fcg =
  let nextNode = head instrs
      graphWithEdge = mkEdge thisNode nextNode (graph fcg)
      newDef = Map.insert thisNode [] (def fcg)
      newUse = Map.insert thisNode [] (use fcg)
      newIsmove = Map.insert thisNode False (ismove fcg)
   in processNodes instrs
        fcg {graph = graphWithEdge, def = newDef, use = newUse, ismove = newIsmove}
processNodes (thisNode@(Node _ (Move _ src dst)):instrs) fcg =
  let nextNode = head instrs
      graphWithEdge = mkEdge thisNode nextNode (graph fcg)
      newDef = Map.insert thisNode [dst] (def fcg)
      newUse = Map.insert thisNode [src] (use fcg)
      newIsmove = Map.insert thisNode True (ismove fcg)
   in processNodes instrs
        fcg {graph = graphWithEdge, def = newDef, use = newUse, ismove = newIsmove}
processNodes (thisNode@(Node _ (Oper _ src dst (Nothing))):instrs) fcg =
  let nextNode = head instrs
      graphWithEdge = mkEdge thisNode nextNode (graph fcg)
      newDef = Map.insert thisNode dst (def fcg)
      newUse = Map.insert thisNode src (use fcg)
      newIsmove = Map.insert thisNode False (ismove fcg)
   in processNodes instrs
        fcg {graph = graphWithEdge, def = newDef, use = newUse, ismove = newIsmove}
processNodes (thisNode@(Node _ (Oper _ src dst (Just label))):instrs) fcg =
  let jmpNode = findNodeWithLabel (head label) (S.toList (nodes (graph fcg)))
      graphWithEdge = mkEdge thisNode jmpNode (graph fcg)
      newDef = Map.insert thisNode dst (def fcg)
      newUse = Map.insert thisNode src (use fcg)
      newIsmove = Map.insert thisNode False (ismove fcg)
   in processNodes instrs
        fcg {graph = graphWithEdge, def = newDef, use = newUse, ismove = newIsmove}

instrs2graph :: [Instr] -> FlowGraph Instr
instrs2graph instrs
  -- Primero insertamos todos los nodos al grafo para poder armar las aristas de manera segura
 =
  let (withNodesFCG, instrsAsNodes) = insertNodesToFCG instrs emptyFCG
    -- Ahora nos falta completar el FCG con el resto de la información
   in processNodes instrsAsNodes withNodesFCG

processLastNode :: Node Instr -> FlowGraph Instr -> FlowGraph Instr
processLastNode thisNode@(Node _ (Label _ _)) fcg =
  let newDef = Map.insert thisNode [] (def fcg)
      newUse = Map.insert thisNode [] (use fcg)
      newIsmove = Map.insert thisNode False (ismove fcg)
  in fcg {def = newDef, use = newUse, ismove = newIsmove}
processLastNode thisNode@(Node _ (Move _ src dst)) fcg =
  let newDef = Map.insert thisNode [dst] (def fcg)
      newUse = Map.insert thisNode [src] (use fcg)
      newIsmove = Map.insert thisNode True (ismove fcg)
  in fcg {def = newDef, use = newUse, ismove = newIsmove}
processLastNode thisNode@(Node _ (Oper _ src dst (Nothing))) fcg =
  let newDef = Map.insert thisNode dst (def fcg)
      newUse = Map.insert thisNode src (use fcg)
      newIsmove = Map.insert thisNode False (ismove fcg)
  in fcg {def = newDef, use = newUse, ismove = newIsmove}
processLastNode thisNode@(Node _ (Oper _ _ _ (Just _))) fcg =
  error "[TigerMakeGraph] Jump in last instr"

