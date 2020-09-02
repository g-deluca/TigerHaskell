module TigerAlloc where

import           Control.Monad
import           Control.Monad.State
import qualified Data.List           as L
import qualified Data.List.Split     as Split
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

instance Show Worklists where
  show (Worklists simp _ spill) =
    "Simplify: " ++ show simp ++ "\n" ++ "Spill: " ++ show spill

data AllocState =
  AllocState
    { igraph       :: InterferenceGraph
    , worklists    :: Worklists
    , okColors     :: S.Set Temp
    , spilledNodes :: S.Set Temp
    -- Me llevo el stack de nodos con el grafo correspondiente a ese momento para poder "restaurarlo"
    , stack        :: [(Temp, InterferenceGraph)]
    -- Mapa de colores (coloredNodes = keys colorsMap)
    , colorsMap    :: M.Map Temp Temp
    }

type ColorsMap = M.Map Temp Temp

instance Show AllocState where
  show allocState =
    show (igraph allocState) ++ "\n" ++ show (worklists allocState)

-------------
-- Initial state
initialState :: AllocState
initialState =
  AllocState
    { worklists = Worklists {simplifyWL = [], freezeWL = [], spillWL = []}
    , igraph = IGraph S.empty S.empty
    , stack = []
    , okColors = allColors
    , spilledNodes = S.empty
    , colorsMap = initColorsMap
    }

type Allocator a = StateT AllocState StGen a

-------------
-- Constantes
precolored :: [Temp]
precolored = allRegs

-- Cantidad de colores disponibles
k :: Int
k = length allRegs -- - length specialregs

allColors :: S.Set Temp
allColors = S.fromList $ allRegs L.\\ specialregs

initColorsMap :: ColorsMap
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

makeSpillWorklist :: Allocator ()
makeSpillWorklist = do
  oldSt <- get
  let nodos = nodes $ igraph oldSt
  let initial = S.elems $ nodos `S.difference` (S.fromList precolored)
  let newSpill = filter (\t -> degree t (igraph oldSt) >= k) initial
  let oldLists = worklists oldSt
  put $ oldSt {worklists = oldLists {spillWL = newSpill}}

makeWorklists :: Allocator ()
makeWorklists = do
  makeSimplifyWorklist
  makeSpillWorklist

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

selectPotentialSpill :: Allocator ()
selectPotentialSpill = do
  st <- get
  let oldWorklists = worklists st
      selected = head $ spillWL oldWorklists
      newSpillWL = tail $ spillWL oldWorklists
      newSimplifyWL = selected : (simplifyWL oldWorklists)
  put $
    st
      { worklists =
          oldWorklists {spillWL = newSpillWL, simplifyWL = newSimplifyWL}
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
      if okColors == S.empty
        then do
          let oldSpilled = spilledNodes st
          put $
            st
              { spilledNodes = S.insert t oldSpilled
              , stack = newStack
              , igraph = ig
              }
        else do
          put $
            st
              { colorsMap = M.insert t (head $ S.elems okColors) actualColorsMap
              , stack = newStack
              , igraph = ig
              }
          -- Súper imperativo: quedó la recursión en un elemento que está en el estado (stack)
          assignColors

-------------
-- Reescribir el programa
rewriteProgram :: [Instr] -> Frame -> Allocator ([Instr], Frame)
rewriteProgram instrs frame = do
  st <- get
  let spilled = S.elems $ spilledNodes st
  rewriteAllTemps spilled instrs frame

-- Custom foldM for better understanding
rewriteAllTemps :: [Temp] -> [Instr] -> Frame -> Allocator ([Instr], Frame)
rewriteAllTemps [] instrs frame = return (instrs, frame)
rewriteAllTemps (t:temps) instrs frame = do
  (newInstrs, newFrame) <- rewriteOneTemp t instrs frame
  rewriteAllTemps temps newInstrs newFrame

rewriteOneTemp :: Temp -> [Instr] -> Frame -> Allocator ([Instr], Frame)
rewriteOneTemp _ [] oldFrame = do
  let newFrame = oldFrame {actualReg = actualReg oldFrame + 1}
  return ([], newFrame)
rewriteOneTemp t (instr@(Oper _ src dst _):instrs) frame = do
  let def = elem t dst
      use = elem t src
  (newInstrs, newFrame) <- rewriteOneTemp t instrs frame
  if def
    then do
      freshTemp <- newTemp
      -- Acá hay mucho pattern matching que damos por sentado:
      -- - Sabemos que podemos usar "Just" pq estamos adentro de def
      let Just tIndexOnDst = L.elemIndex t dst
          -- Y sabemos que podemos "tirar" un elemento de la segunda mitad por
          -- el índice que le pasamos a splitAt
          (initNewDst, _:tailNewDst) = splitAt tIndexOnDst dst
          newDst = initNewDst ++ [freshTemp] ++ tailNewDst
      return
        ([instr {odst = newDst}] ++ [(push freshTemp)] ++ newInstrs, newFrame)
    else if use
           then do
             freshTemp <- newTemp
             let Just tIndexOnSrc = L.elemIndex t src
                 (initNewSrc, _:tailNewSrc) = splitAt tIndexOnSrc src
                 newSrc = initNewSrc ++ [freshTemp] ++ tailNewSrc
             return
               ( [(retrieve freshTemp newFrame)] ++
                 [instr {osrc = newSrc}] ++ newInstrs
               , newFrame)
           else return (instr : newInstrs, newFrame)
rewriteOneTemp t (instr@(Move _ src dst):instrs) frame = do
  let def = t == dst
      use = t == src
  (newInstrs, newFrame) <- rewriteOneTemp t instrs frame
  if def
    then do
      freshTemp <- newTemp
      return
        ( [instr {mdst = freshTemp}] ++ [(push freshTemp)] ++ newInstrs
        , newFrame)
    else if use
           then do
             freshTemp <- newTemp
             return
               ( [(retrieve freshTemp newFrame)] ++
                 [instr {msrc = freshTemp}] ++ newInstrs
               , newFrame)
           else return (instr : newInstrs, newFrame)
rewriteOneTemp t (label:instrs) frame = do
  (newInstrs, newFrame) <- rewriteOneTemp t instrs frame
  return (label : newInstrs, newFrame)

push :: Temp -> Instr
push t = Oper {oassem = "push s0\n", osrc = [t], odst = [sp], ojump = Nothing}

retrieve :: Temp -> Frame -> Instr
retrieve t frame =
  let offset =
        TigerFrame.wSz *
        (actualLocal frame + actualReg frame) + TigerFrame.localsGap
   in Oper
        { oassem = "mov -" ++ show offset ++ "(s0), d0\n"
        , osrc = [fp]
        , odst = [t, sp]
        , ojump = Nothing
        }

-- Una vez que tenemos el mapa de colores construidos tenemos que reemplazar los
-- temps que aparecen en las Instr por los colores (aka registros de la máquina).
--
-- No necesita del estado de Allocator más que el colorsMap ya calculado así que
-- no hace falta que sea monádica
applyColors :: [Instr] -> ColorsMap -> [Instr]
applyColors [] _ = []
applyColors ((Oper assem src dst jmp):instrs) colorsMap =
  let newSrc = map (\t -> lookUp colorsMap t "newSrc opr >>>") src
      newDst = map (\t -> lookUp colorsMap t "newDst oper >>>") dst
      newOper = replaceTemps $ Oper assem newSrc newDst jmp
   in newOper : applyColors instrs colorsMap
applyColors ((Move assem src dst):instrs) colorsMap =
  let newSrc = lookUp colorsMap src "newSrc move >>>"
      newDst = lookUp colorsMap dst "newDst move >>>"
      newMove = replaceTemps $ Move assem newSrc newDst
   in newMove : applyColors instrs colorsMap
-- En los labels no hay que plicar colores pq no hay temporales
applyColors (otherInstr:instrs) colorsMap =
  otherInstr : applyColors instrs colorsMap

replaceDst :: Instr -> Instr
replaceDst (Oper assem src dst jmp) =
  case Split.splitOn "d0" assem
    -- Caso 1: La instrucción no tiene temporales de destino
        of
    [oneInstr] -> Oper oneInstr src dst jmp
    -- Caso 2: La instrucción tiene exactamente un d0, por lo que podemos asumir que
    -- el registro correspondiente está en (head dst)
    [beforeDest, afterDest] ->
      Oper (beforeDest ++ makeStringT (head dst) ++ afterDest) src dst jmp
    -- Cas0 3: Hay alguna instrucción con más de un destino (no debería por las instrucciones que elegimos)
    _ ->
      error $
      "[replaceDst] Apparently theres more than one destination on instruction: " ++
      assem
replaceDst (Move assem src dst) =
  case Split.splitOn "d0" assem of
    [oneInstr] -> Move oneInstr src dst
    [beforeDest, afterDest] ->
      Move (beforeDest ++ makeStringT dst ++ afterDest) src dst
    _ ->
      error $
      "[replaceDst] Apparently theres more than one destination on instruction: " ++
      assem
replaceDst otherInstr = otherInstr

replaceSrc :: Instr -> Instr
replaceSrc (Oper assem src dst jmp) =
  case Split.splitOn "s0" assem
    -- Caso 1: La instrucción no tiene temporales de destino
        of
    [oneInstr] -> Oper oneInstr src dst jmp
    -- Caso 2: La instrucción tiene exactamente un s0, por lo que podemos asumir que
    -- el registro correspondiente está en (head src)
    [beforeSrc, afterSrc] ->
      case Split.splitOn "s1" afterSrc of
        [oneInstr] ->
          Oper (beforeSrc ++ makeStringT (head src) ++ afterSrc) src dst jmp
        [afterBefore, afterAfter] ->
          Oper
            (beforeSrc ++
             makeStringT (head src) ++
             afterBefore ++ makeStringT (last src) ++ afterAfter)
            src
            dst
            jmp
    -- Cas0 3: Hay alguna instrucción con más de un origen (no debería por las instrucciones que elegimos)
    _ ->
      error $
      "[replaceDst] Apparently theres more than one src on instruction: " ++
      assem
replaceSrc (Move assem src dst) =
  case Split.splitOn "s0" assem of
    [oneInstr] -> Move oneInstr src dst
    [beforeSrc, afterSrc] ->
      Move (beforeSrc ++ makeStringT src ++ afterSrc) src dst
    _ ->
      error $
      "[replaceDst] Apparently theres more than one src on instruction: " ++
      assem
replaceSrc otherInstr = otherInstr

replaceTemps :: Instr -> Instr
replaceTemps = replaceDst . replaceSrc

-------------
-- Minor optimization: removes silly moves like `mov %eax, %eax`
removeSillyMoves :: [Instr] -> [Instr]
removeSillyMoves instrs = filter notSillyMove instrs
  where
    notSillyMove (Move _ src dst) = src /= dst
    notSillyMove _                = True

-------------
-- Main
allocate :: [Instr] -> Frame -> Allocator [Instr]
allocate instrs frame = do
  build instrs frame
  makeWorklists
  loop
  assignColors
  st <- get
  if spilledNodes st == S.empty
    then do
      let newInstr = applyColors instrs (colorsMap st)
      let newInstrWithoutSillyMoves = removeSillyMoves newInstr
      return newInstrWithoutSillyMoves
    else do
      (newInstr, newFrame) <- rewriteProgram instrs frame
      -- Ahora que tenemos el nuevo programa, empezamos todo de vuelta
      put initialState
      allocate newInstr newFrame

loop :: Allocator ()
loop = do
  st <- get
  let simplifyWorklist = simplifyWL $ worklists st
      spillWorklist = spillWL $ worklists st
  if simplifyWorklist /= []
    then simplify
    else if spillWorklist /= []
           then selectPotentialSpill
           else return ()
  areEmpty <- checkAllWorklists
  if areEmpty
    then return ()
    else loop

checkAllWorklists :: Allocator Bool
checkAllWorklists = do
  st <- get
  let wl = worklists st
  return $ (simplifyWL wl) == [] && (spillWL wl) == []

-- Manejo de la mónada
runAllocator :: [Instr] -> Frame -> StGen ([Instr], AllocState)
runAllocator instrs frame = flip runStateT initialState (allocate instrs frame)
