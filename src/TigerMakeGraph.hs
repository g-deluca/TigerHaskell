module TigerMakeGraph where

  import Assem
  import TigerTemp
  import TigerGraph
  import Data.Maybe
  -- http://hackage.haskell.org/package/containers-0.6.2.1/docs/Data-Map-Strict.html
  import qualified Data.Map.Strict as Map

  data FlowGraph a = FlowGraph {
    graph :: Graph a,
    -- a table of the temporaries defined at each node (destination registers of the instruction)
    def :: Map.Map (Node Instr) [Temp], -- src
    -- a table of the temporaries used at each node (source registers of the instruction)
    use :: Map.Map (Node Instr) [Temp], -- dst
    -- tells wether each instruction is a MOVE
    ismove :: Map.Map (Node Instr) Bool
  }

  emptyFlowGraph :: FlowGraph a
  emptyFlowGraph = FlowGraph {graph = emptyGraph, def = Map.empty, use = Map.empty, ismove = Map.empty}

  -- El algoritmo será el siguiente:
  -- Agregamos todas las Instr Label primero, porque cuando tengamos que agregar una arista (de un Jump)
  -- necesito tener el nodo ya agregado
  isLabel (Label _ _) = True
  isLabel _            = False

  instrs2graph :: [Instr] -> (FlowGraph Instr, [Node Instr])
  instrs2graph instrs =
    let labelInstrs = filter isLabel instrs
        otherInstrs = filter (not . isLabel) instrs
        first = labels2graph labelInstrs (emptyFlowGraph, [])
        second = others2graph otherInstrs first
    in second

  labels2graph :: [Instr] -> (FlowGraph Instr, [Node Instr]) -> (FlowGraph Instr, [Node Instr])
  labels2graph [] res = res
  labels2graph (ins:inss) res = labels2graph inss (oneLabel2graph ins res)

  oneLabel2graph :: Instr -> (FlowGraph Instr, [Node Instr]) -> (FlowGraph Instr, [Node Instr])
  oneLabel2graph instr@(Label _ _ ) (flowGraph, nodes) =
      let newNode = mkNode instr (graph flowGraph) -- solo el nodo
          newGraph = addNode instr (graph flowGraph) -- grafo con nuevo nodo
          newIsmove = Map.insert newNode False (ismove flowGraph)
      in (flowGraph {graph = newGraph, ismove = newIsmove}, newNode:nodes)
  oneLabel2graph _ _= error "why?"

  others2graph :: [Instr] -> (FlowGraph Instr, [Node Instr]) -> (FlowGraph Instr, [Node Instr])
  others2graph [] res = res
  others2graph (ins:inss) res = others2graph inss (oneOther2graph ins res)

  oneOther2graph :: Instr -> (FlowGraph Instr, [Node Instr]) -> (FlowGraph Instr, [Node Instr])
  oneOther2graph instr@(Move _ src dst) (flowGraph, nodes) =
      let newNode = mkNode instr (graph flowGraph)
          newGraph = addNode instr (graph flowGraph)
          newDef = Map.insert newNode [src] (def flowGraph)
          newUse = Map.insert newNode [dst] (use flowGraph)
          newIsmove = Map.insert newNode True (ismove flowGraph)
      in (flowGraph {graph = newGraph, def = newDef, use = newUse, ismove = newIsmove}, newNode:nodes)

  oneOther2graph instr@(Oper _ src dst (Just label)) (flowGraph, nodes) =
      let newNode = mkNode instr (graph flowGraph)
          newGraph = addNode instr (graph flowGraph)
          -- tengo que agregar la arista con el jmp ese
          foundNode = findNodeWithLabel (head label) nodes
          newGraph' = mkEdge newNode foundNode newGraph
          newDef = Map.insert newNode src (def flowGraph)
          newUse = Map.insert newNode dst (use flowGraph)
          newIsmove = Map.insert newNode False (ismove flowGraph)
      in (flowGraph {graph = newGraph', def = newDef, use = newUse, ismove = newIsmove}, newNode:nodes)

  oneOther2graph instr@(Oper _ src dst Nothing) (flowGraph, nodes) =
    let newNode = mkNode instr (graph flowGraph)
        newGraph = addNode instr (graph flowGraph)
        newDef = Map.insert newNode src (def flowGraph)
        newUse = Map.insert newNode dst (use flowGraph)
        newIsmove = Map.insert newNode False (ismove flowGraph)
    in (flowGraph {graph = newGraph, def = newDef, use = newUse, ismove = newIsmove}, newNode:nodes)

  oneOther2graph (Label _ _) _ = error "why? bad filter"

  findNodeWithLabel :: Label -> [Node Instr] -> Node Instr
  findNodeWithLabel label [] = error "Error en la busqueda del nodo"
  findNodeWithLabel label (node@(Node _ label'@(Label _ _)):xs) =
    if label == llab label' then node else findNodeWithLabel label xs