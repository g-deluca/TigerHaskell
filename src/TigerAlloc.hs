module TigerAlloc where

import           Assem
import           Control.Monad.State
import qualified Data.Map            as M
import qualified Data.Set            as S
import           TigerFrame
import           TigerLiveness
import           TigerMakeGraph
import           TigerTemp
import           TigerUnique

data Worklists =
  Worklists
    { simplify :: [Temp]
    , freeze   :: [Temp]
    , spill    :: [Temp]
    }

data AllocState =
  AllocState
    { igraph     :: InterferenceGraph
    , worklists  :: Worklists
    , precolored :: [Temp]
    }

initialState :: AllocState
initialState =
  AllocState
    { worklists = Worklists {simplify = [], freeze = [], spill = []}
    , igraph = IGraph S.empty S.empty
    , precolored = allRegs
    }

type Allocator a = StateT AllocState StGen a

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
makeWorklists :: Allocator ()
makeWorklists = undefined

-------------
-- Main loop
allocLoop :: [Instr] -> Frame -> Allocator ()
allocLoop instrs frame = do
  build instrs frame
  return ()
