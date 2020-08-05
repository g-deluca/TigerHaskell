module TigerAlloc where

import           Assem
import           Control.Monad.State
import qualified Data.Map            as M
import qualified Data.Set            as S
import           TigerFrame
import           TigerLiveness       as IGraph
import           TigerMakeGraph
import           TigerTemp
import           TigerUnique

data Worklists =
  Worklists
    { simplifyWL :: [Temp]
    , freezeWL   :: [Temp]
    , spillWL    :: [Temp]
    }

data AllocState =
  AllocState
    { igraph    :: InterferenceGraph
    , worklists :: Worklists
    -- Me llevo el stack de nodos con el grafo correspondiente a ese momento para poder "restaurarlo"
    , stack     :: [(Temp, InterferenceGraph)]
    }

initialState :: AllocState
initialState =
  AllocState
    { worklists = Worklists {simplifyWL = [], freezeWL = [], spillWL = []}
    , igraph = IGraph S.empty S.empty
    , stack = []
    }

type Allocator a = StateT AllocState StGen a

-- Constantes
precolored :: S.Set Temp
precolored = S.fromList allRegs

-- Funciones auxiliares
degree :: Temp -> InterferenceGraph -> Int
degree t igraph = S.size $ IGraph.adj t igraph

-- Cantidad de colores disponibles
k :: Int
k = length allRegs - length specialregs

-- Construye el grafo de interferencia
build :: [Instr] -> Frame -> Allocator ()
build instrs frame = do
  let fcg = instrs2graph instrs
      livenessMap = calculateLiveness fcg
      igraph = calculateInterferenceGraph fcg
  oldState <- get
  put $ oldState {igraph = igraph}

-------------
-- Inicializamos las worklists
makeSimplifyWorklist :: Allocator ()
makeSimplifyWorklist = do
  oldSt <- get
  let nodos = nodes $ igraph oldSt
  let initial = S.elems $ nodos `S.difference` precolored
  let newSimplify = filter (\t -> degree t (igraph oldSt) < k) initial
  let oldLists = worklists oldSt
  put $ oldSt {worklists = oldLists {simplifyWL = newSimplify}}

makeWorklists :: Allocator ()
makeWorklists = do
  makeSimplifyWorklist

-- Simplify
-- De a uno por vez, vamos sacando los nodos de grado bajo (< K) del grafo
simplify :: Allocator ()
simplify = do
  st <- get
  let oldGraph = igraph st
  let oldWorklists = worklists st
  -- Es seguro hacer este pattern porque entramos a este caso solo cuando la worklist
  -- de simplify es distinta a vacío
  let (t:newSimplifyWL) = simplifyWL $ worklists st
  let newStack = (t, oldGraph) : (stack st)
  let newIGraph = removeNode t oldGraph
  put $
    st
      { stack = newStack
      , igraph = newIGraph
      , worklists = oldWorklists {simplifyWL = newSimplifyWL}
      }

-------------
-- Main
allocate :: [Instr] -> Frame -> Allocator ()
allocate instrs frame = do
  build instrs frame
  return ()

loop :: Allocator ()
loop = do
  st <- get
  let simplifyWorklist = simplifyWL $ worklists st
  if simplifyWorklist /= []
    then simplify
    else return ()
