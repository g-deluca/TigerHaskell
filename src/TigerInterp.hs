{-# Language LambdaCase #-}
{-# Language OverloadedStrings #-}
module TigerInterp where

import Prelude hiding (compare, EQ)

import TigerTree
import TigerFrame
import TigerTemp
import TigerSymbol

import Data.Map as M

import Control.Monad.State

import Debug.Trace

-- | Datos a almacenar en memoria.
data Dato
    -- | String constantes.
    = Str Symbol
    -- | Cuerpos de funciones constantes.
    | FBody (
        -- | Lista de acceso de los posibles argumentos.
        [Access]
        -- | Body de la función.
        , [Stm])
    -- | O puedo almacenar un entero.
    | DInt Int
    deriving (Show)

getInt :: Dato -> Int
getInt (DInt i) = i
getInt _ = error "NOT AN INT"

getStr :: Dato -> Symbol
getStr (Str s) = s
getStr _ = error "NOT A Symbol?"

getFBody :: Dato -> ([Access] , [Stm])
getFBody (FBody sts) = sts
getFBody _ = error "NOT A FUN"

data CPU = CPU
    { -- | Mem representa la memoria del CPU, básicamente los registros.
      mem :: M.Map Temp Int
    , wat :: M.Map Int Dato
    , dat :: M.Map Label Dato
    , output :: [Symbol]
    , input :: [Symbol]
    } deriving Show

type RC = State CPU

extCall :: Label -> Bool
extCall l = all ((== l) . pack) ["print", "flush", "getchar"]

printExec :: Int -> RC Int
printExec i = trace (show i) $ do
  env <- get
  let mblab = wat env ! i
  let mbstr = dat env ! getStr mblab
  put (env{output = output env ++ [getStr mbstr]} )
  return 1

extDispatcher :: Label -> [Int] -> RC Int
extDispatcher "print" (x : _ ) = printExec x

compute :: BOp -> Int -> Int -> Int
compute Plus = (+)
compute _ = error "TODO"

compare :: Relop -> Int -> Int -> Bool
compare EQ = (==)
compare _ = error "TODO"

-- | Exp :: TInt
iexp :: Exp -> RC Int
iexp (Const i) = return i
iexp (Name n) = trace ("NAME") $ get >>= \e -> return $ getInt (dat e ! n)
iexp (Temp t) = get >>= \e -> return $ mem e ! t
iexp (Binop op x y) = do
  x' <- iexp x
  y' <- iexp y
  return $ compute op x' y'
iexp (Mem e) = do
  e' <- iexp e
  env <- get
  return $ getInt $ wat env ! e'
iexp (Call (Name f) es) = do
  es' <- mapM iexp es
  dats <- gets dat
  if (extCall f)
     then extDispatcher f es'
  -- Acá hay que conectar los argumentos con el body de la función
     else do
        let (acc , body) = getFBody $ dats ! f
        mapM_ step $ zipWith (\a i -> Move (TigerFrame.exp a 0) (Mem (Const i))) acc es'
        mems <- gets mem
        return $ mems ! rv
iexp (Call _ _ ) = error "Puede pasar?"
iexp (Eseq s e) = step s >> iexp e

step :: Stm -> RC [Stm]
step (Label _) = return []
step (Seq l r) = return [l , r]
-- | Assm load
step (Move (Temp t) (Mem m)) = do
  dir <- iexp m
  wats <- gets wat
  modify $ \env -> env{mem = M.insert t (getInt $ wats ! dir) (mem env)}
  return []
step (Move (Temp t) src) = do
  val <- iexp src
  modify $ \env -> env{mem = M.insert t val (mem env)}
  return []
-- | Assm store
step (Move (Mem t) src) = do
  dir <- iexp t
  val <- iexp src
  modify $ \env -> env{wat = M.insert dir (DInt val) (wat env)}
  return []
step (Move dst src) = do
  src' <- iexp src
  dst' <- iexp dst
  modify (\env -> env{wat = M.insert dst' (wat env ! src') (wat env)})
  return []
step (ExpS e) = iexp e >> return []
step (Jump _ l) = gets dat >>= \dats -> return $ snd $ getFBody $ dats ! l
step (CJump bop x y tt ff) = do
  x' <- iexp x
  y' <- iexp y
  return $ if compare bop x' y' then
    [Jump (Const 0) tt]
  else
    [Jump (Const 0) ff]

runPc :: [Stm] -> RC ()
runPc [] = return ()
runPc (l@Jump{} : _) = step l >>= runPc
runPc (x : xs) = step x >>= \ys -> runPc (ys ++ xs)

emptyCPU :: CPU
emptyCPU = CPU M.empty M.empty M.empty [] []

runInitial :: CPU -> [Stm] -> CPU
runInitial cpu prog = execState (runPc prog) cpu

-- | Función que búsca los posibles labels dentro de una sequencia de stms.
splitStms :: [Stm]
          ->
             -- | Lista de stms hasta encontrar un Label.
             ([Stm]
             -- | Segmentos Lable lista de Stmts debajo de él.
             , [(Label, [Stm])])
splitStms [] = ([],[])
splitStms ((Label l) : ts) = ([], splitLbls ts (l , []))
splitStms (t : ts) = let (res, lbls) = splitStms ts in (t : res, lbls)

-- | Función auxiliar que claramente hace todo el trabajo. Básicamente va
-- acumulando hasta encontrar un Label, lo agrega al final de la lista, y pasa a
-- acumular otro label.
splitLbls :: [Stm] -> (Label, [Stm]) -> [(Label, [Stm])]
splitLbls [] ts = [ts]
splitLbls ((Label l) : ts) rs = rs : splitLbls ts (l,[])
splitLbls (t : ts) (l, rs) = splitLbls ts (l, t : rs)

-- | Preparamos la CPU para que corra desde un estado inicial.
loadCPU ::
        -- | Fragmentos de funciones ya definidas. (fuera del main)
         [(Frame, [Stm])]
        -- | Strings.
        -> [(Label , Symbol)]
        -- | Básicamente el main.
        -> [Stm]
        -> CPU
loadCPU fs ss tmain =
  let
    -- Cargamos las strings a una Cpu vacía.
    loadStrings = (Prelude.foldl (\r (l, s) ->
                                  r{dat = M.insert l (Str s) (dat r)}
                                  ) emptyCPU ss)
    -- Sobre la CPU cargada con las strings, cargamos las funciones.
    loadProcs = Prelude.foldl (\r (f, body) ->
                                  let (factBody, lbls) = splitStms body in
                                  r{dat =
                                       Prelude.foldl
                                       (\ datos (l, s)
                                        -> M.insert l (FBody ([], s)) datos)
                                       (M.insert (name f)
                                           (FBody (prepFormals f , factBody ))
                                           (dat r)) lbls
                                    }
                               )
                 loadStrings fs
    -- Separamos los primeros Statements hasta el primer label, que lo separamos.
    (main, restLabels) = splitStms tmain
    -- Agregamos los nuevos lbls que encontramos en el main.
    inCpu = Prelude.foldl (\ r (l,s) -> r{dat = M.insert l (FBody ([], s))
                                               (dat r)} ) loadProcs restLabels
  in
    runInitial inCpu (snd $ head $ restLabels)
