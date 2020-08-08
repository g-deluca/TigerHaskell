module TigerAlloc where

import           Control.Monad
import           Control.Monad.State

import qualified Data.List           as L
import qualified Data.Map            as M
import qualified Data.Set            as S

import           Assem
import           TigerFrame
import           TigerLiveness       as IGraph
import           TigerMakeGraph
import           TigerTemp
import           TigerUnique

import           Debug.Trace

data Worklists =
  Worklists
    { simplifyWL :: [Temp]
    , freezeWL   :: [Temp]
    , spillWL    :: [Temp]
    }
  deriving (Show)

data AllocState =
  AllocState
    { igraph    :: InterferenceGraph
    , worklists :: Worklists
    , okColors  :: S.Set Temp
    -- Me llevo el stack de nodos con el grafo correspondiente a ese momento para poder "restaurarlo"
    , stack     :: [(Temp, InterferenceGraph)]
    -- Mapa de colores (coloredNodes = keys colorsMap)
    , colorsMap :: M.Map Temp Temp
    }

instance Show AllocState where
  show allocState =
    show (colorsMap allocState) ++ " >>> " ++ show (igraph allocState)

initialState :: AllocState
initialState =
  AllocState
    { worklists = Worklists {simplifyWL = [], freezeWL = [], spillWL = []}
    , igraph = IGraph S.empty S.empty
    , stack = []
    , okColors = allColors
    , colorsMap = initColorsMap
    }

type Allocator a = StateT AllocState StGen a

-------------
-- Constantes
precolored :: [Temp]
precolored = allRegs

-- Cantidad de colores disponibles
k :: Int
k = length allRegs - length specialregs

allColors :: S.Set Temp
allColors = S.fromList $ allRegs L.\\ specialregs

initColorsMap :: M.Map Temp Temp
initColorsMap = M.fromList (zip precolored precolored)

-------------
-- Funciones auxiliares
degree :: Temp -> InterferenceGraph -> Int
degree t igraph = S.size $ IGraph.adj t igraph

-- Construye el grafo de interferencia
build :: [Instr] -> Frame -> Allocator ()
build instrs frame = do
  let fcg = instrs2graph instrs
      --livenessMap = calculateLiveness fcg
      igraph = calculateInterferenceGraph fcg
  oldState <- get
  put $ oldState {igraph = igraph}

-------------
-- Inicializamos las worklists
makeSimplifyWorklist :: Allocator ()
makeSimplifyWorklist = do
  oldSt <- get
  let nodos = nodes $ igraph oldSt
  let initial = S.elems $ nodos `S.difference` (S.fromList precolored)
  let newSimplify = filter (\t -> degree t (igraph oldSt) < k) initial
  let oldLists = worklists oldSt
  put $ oldSt {worklists = oldLists {simplifyWL = newSimplify}}

makeWorklists :: Allocator ()
makeWorklists = do
  makeSimplifyWorklist

-------------
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
-- Asignar colores Acá vamos construyendo el colorsMap, asignandole un registro
-- real a cada temp.
-- TODO: Modificar cuando se agregue spilling coalescing (pag. 249)
assignColors :: Allocator ()
assignColors = do
  st <- get
  let actualStack = stack st
  let actualColorsMap = colorsMap st
  case actualStack of
    [] -> return ()
    ((t, ig):newStack) -> do
      let adjList = adj t ig
          adjListFiltered =
            S.filter
              (\adjTemp -> L.elem adjTemp (M.keys actualColorsMap))
              adjList
          notOkColors =
            S.map (\adjTemp -> actualColorsMap M.! adjTemp) adjListFiltered
          okColors = allColors `S.difference` notOkColors
      put $
        st
          { colorsMap = M.insert t (head $ S.elems okColors) actualColorsMap
          , stack = newStack
          , igraph = ig
          }
      -- Súper imperativo: quedó la recursión en un elemento que está en el estado (stack)
      assignColors

-------------
-- Main
allocate :: [Instr] -> Frame -> Allocator [Instr]
allocate instrs frame = do
  build instrs frame
  makeWorklists
  loop
  assignColors
  return []

loop :: Allocator ()
loop = do
  st <- get
  let simplifyWorklist = simplifyWL $ worklists st
  if simplifyWorklist /= []
    then simplify >> loop
    else return ()

-- Manejo de la mónada
runAllocator :: [Instr] -> Frame -> StGen AllocState
runAllocator instrs frame = flip execStateT initialState (allocate instrs frame)
