module TigerMunch where

import           Assem               as A

import           TigerFrame
import           TigerTemp
import           TigerTree           as T
import           TigerUnique

import           Control.Monad.State
import           Data.Text           (unpack)

class (Monad w, TLGenerator w) =>
      InstrEmitter w
    -- | Acumula la lista de instrucciones que se retornarán luego
  where
  emit :: Instr -> w ()
    -- | Función auxiliar para munchExp. La agrego porque está en el
    -- | libro pero no sé qué tan útil es en Haskell.
  result :: (Temp -> w ()) -> w Temp
  result gen = do
    res <- newTemp
    gen res
    return res

munchStm :: InstrEmitter w => Stm -> w ()
munchStm (Seq s1 s2) = munchStm s1 >> munchStm s2
munchStm (ExpS e) = void $ munchExp e
-- TODO: Agregar casos para optimizar (o nah xd)
munchStm (T.Move e1 e2) = do
  stm1 <- munchExp e1
  stm2 <- munchExp e2
  emit $ A.Move {massem = "mov d0, s0\n", msrc = stm2, mdst = stm1}
munchStm (Jump (Name _) label) = do
  emit $
    Oper
      { oassem = "jmp " ++ makeStringL label ++ "\n"
      , osrc = []
      , odst = []
      , ojump = Just [label]
      }
  return ()
munchStm (CJump relop e1 e2 l1 l2) = do
  te1 <- munchExp e1
  te2 <- munchExp e2
    -- Primero realizo la comparación
  emit $
    Oper
      {oassem = "cmp s0, s1\n", osrc = [te1, te2], odst = [], ojump = Nothing}
    -- Saltamos dependiendo del tipo de operacion fue True
  let assemRelOp = relop2assem relop
  emit $
    Oper
      { oassem = assemRelOp ++ makeStringL l1 ++ "\n"
      , osrc = []
      , odst = []
      , ojump = Just [l1]
      }
    -- Si el salto condicional "assemRelOp" no se ejecuto, se ejecuta la siguiente instruccion
    -- el Jump a la label del caso False
  emit $
    Oper
      { oassem = "jmp " ++ makeStringL l2 ++ "\n"
      , osrc = []
      , odst = []
      , ojump = Just [l2]
      }
  return ()
munchStm (T.Label label) = do
  emit $ A.Label {lassem = makeStringL label ++ ":\n", llab = label}
  return ()

munchExp :: InstrEmitter w => Exp -> w Temp
munchExp (Const i) = do
  r <- newTemp
  emit $
    Oper
      { oassem = "mov d0, " ++ show i ++ "\n"
      , osrc = []
      , odst = [r]
      , ojump = Nothing
      }
  return r
munchExp (Name l) = do
  r <- newTemp
  emit $
    Oper
        -- TODO: Googlear un poco más offset
      { oassem = "mov d0, OFFSET " ++ show l ++ "\n"
      , osrc = []
      , odst = [r]
      , ojump = Nothing
      }
  return r
munchExp (Temp t) = return t
munchExp (Eseq s e) = munchStm s >> munchExp e
munchExp (T.Mem (T.Binop T.Plus e (T.Const i))) = do
  res <- newTemp
  te <- munchExp e
  emit $
    Oper
      { oassem = "mov d0, [s0 + " ++ show i ++ "]\n"
      , osrc = [te]
      , odst = [res]
      , ojump = Nothing
      }
  return res
-- TODO: ¿Por qué agarramos puntualmente el caso de "Plus" y no
-- el resto. Así está en el libro, ¿but why?
munchExp (T.Mem (T.Binop T.Plus (T.Const i) e)) = do
  res <- newTemp
  te <- munchExp e
  emit $
    Oper
      { oassem = "mov d0, [s0 + " ++ show i ++ "]\n"
      , osrc = [te]
      , odst = [res]
      , ojump = Nothing
      }
  return res
munchExp (T.Mem (T.Const i)) = do
  res <- newTemp
  emit $
    Oper
      { oassem = "mov d0, [" ++ show i ++ "]\n"
      , osrc = []
      , odst = [res]
      , ojump = Nothing
      }
  return res
munchExp (T.Mem e) = do
  te <- munchExp e
  res <- newTemp
  emit $
    Oper {oassem = "mov d0, [s0]\n", osrc = [te], odst = [res], ojump = Nothing}
  return res
-- Arrancamo' con Binop...
munchExp (T.Binop T.Plus el er) = do
  tl <- munchExp el
  tr <- munchExp er
  res <- newTemp
    -- TODO: ¿Usar move?
  emit $ A.Move {massem = "mov d0, s0\n", msrc = tl, mdst = res}
  emit $
    Oper
      {oassem = "add d0, s0\n", osrc = [tr, res], odst = [res], ojump = Nothing}
  return res
munchExp (T.Binop T.Minus el er) = do
  tl <- munchExp el
  tr <- munchExp er
  res <- newTemp
  emit $ A.Move {massem = "mov d0, s0\n", msrc = tl, mdst = res}
  emit $
    Oper
      {oassem = "sub d0, s0\n", osrc = [tr, res], odst = [res], ojump = Nothing}
  return res
-------------
-- Pequeña optimización ;)
munchExp (T.Binop T.Mul (Const i) er) = do
  tr <- munchExp er
  res <- newTemp
  emit $
    Oper
      { oassem = "imul d0, s0, " ++ show i ++ "\n"
      , osrc = [tr]
      , odst = [res]
      , ojump = Nothing
      }
  return res
munchExp (T.Binop T.Mul el (Const i)) = do
  tl <- munchExp el
  res <- newTemp
  emit $
    Oper
      { oassem = "imul d0, s0, " ++ show i ++ "\n"
      , osrc = [tl]
      , odst = [res]
      , ojump = Nothing
      }
  return res
-------------
munchExp (T.Binop T.Mul el er) = do
  tl <- munchExp el
  tr <- munchExp er
  res <- newTemp
  emit $ A.Move {massem = "mov d0, s0\n", msrc = tl, mdst = res}
  emit $
    Oper
      { oassem = "imul d0, s0\n"
      , osrc = [tr, res]
      , odst = [res]
      , ojump = Nothing
      }
  return res
munchExp (T.Binop T.Div el er)
    -- idiv reg1 --> (eax:edx)/reg1
    -- pongo 0's en eax porque no me interesa dividir en 64bits
 = do
  tl <- munchExp el
  tr <- munchExp er
  emit $ Oper {oassem = "mov d0, 0\n", osrc = [], odst = [eax], ojump = Nothing}
  emit $ A.Move {massem = "mov d0, s0\n", msrc = tl, mdst = edx}
  emit $
    Oper
      { oassem = "idiv s0\n"
      , osrc = [tr]
      , odst = [eax, edx] -- [division, resto]
      , ojump = Nothing
      }
  return eax
munchExp (T.Binop T.And el er) = do
  tl <- munchExp el
  tr <- munchExp er
  res <- newTemp
  emit $ A.Move {massem = "mov d0, s0\n", msrc = tl, mdst = res}
  emit $
    Oper
      {oassem = "and d0, s0\n", osrc = [tr, res], odst = [res], ojump = Nothing}
  return res
munchExp (T.Binop T.Or el er) = do
  tl <- munchExp el
  tr <- munchExp er
  res <- newTemp
  emit $ A.Move {massem = "mov d0, s0\n", msrc = tl, mdst = res}
  emit $
    Oper
      {oassem = "or d0, s0\n", osrc = [tr, res], odst = [res], ojump = Nothing}
  return res
munchExp (T.Binop T.XOr el er) = do
  tl <- munchExp el
  tr <- munchExp er
  res <- newTemp
  emit $ A.Move {massem = "mov d0, s0\n", msrc = tl, mdst = res}
  emit $
    Oper
      {oassem = "xor d0, s0\n", osrc = [tr, res], odst = [res], ojump = Nothing}
  return res
munchExp (T.Call (Name n) args)
    -- Primero guardamos en stack los callersaves
  -- TODO: Creo que pushList y popList es lo que deberíamos hacer en la función procEntryExit2 o alguna de esas
  -- pushList callersaves
    -- Ahora los argumentos van a stack
 = do
  munchArgs args
    -- Llamamos a la función
  emit $
    Oper
      { oassem = "call " ++ unpack n ++ "\n"
      , osrc = []
      , odst = calldefs
      , ojump = Nothing
      }
    -- Remove the parameters from stack. This restores the stack to its state before the call was performed.
    -- Muevo el stack-pointer lo suficiente para que se "olvide" de los parametros
  emit $
    Oper
      { oassem = "add s0 " ++ (show (length args * wSz)) ++ "\n"
      , osrc = []
      , odst = [sp]
      , ojump = Nothing
      }
    -- Restore the contents of caller-saved registers (EAX, ECX, EDX) by popping them off of the stack.
    -- The caller can assume that no other registers were modified by the subroutine.
  -- popList (reverse callersaves)
  return eax
munchExp _ = error "No implementado ni con planes de hacerlo"

-- | Definimos la mónada que instanciaremos en Emitter. Le puse Mordisco
-- | porque 'munch' significa masticar (: y me pareció oportuno
type Mordisco = StateT [Instr] StGen

instance InstrEmitter Mordisco where
  emit i = do
    st <- get
        -- Sabemos que el estado es una lista de instrucciones, así que lo
        -- agregamos de cheto.
    put $ i : st

-- TODO: Dejar de poner nombres PAVOS
-- morder :: InstrEmitter w => Stm -> w ()
morder = munchStm

masticar :: [Stm] -> Mordisco [Instr]
masticar [] = get
masticar (stm:stmts) = do
  morder stm
  masticar stmts

tragar :: Mordisco [Instr] -> StGen [Instr]
tragar = flip execStateT []

runMordisco :: [Stm] -> StGen [Instr]
runMordisco stmts = do
  instrs <- tragar $ masticar stmts
  return $ reverse instrs

--------------------------
-- #######################
-- Auxiliares
-- #######################
--------------------------
-- To pass parameters to the subroutine, push them onto the stack before the call.
-- The parameters should be pushed in inverted order (i.e. last parameter first).
-- Since the stack grows down, the first parameter will be stored at the lowest address
-- (this inversion of parameters was historically used to allow functions to be passed a variable number of parameters).
munchArgs :: InstrEmitter e => [Exp] -> e ()
munchArgs [] = return ()
munchArgs args = do
  arg <- munchExp (last args)
  emit $
    Oper
      { oassem = "push d0\n"
      , osrc = [arg]
      , odst = [sp] --porque lo mueve
      , ojump = Nothing
      }
  munchArgs (init args)

-- Before calling a subroutine, the caller should save the contents of certain registers that are designated caller-saved.
-- The caller-saved registers are EAX, ECX, EDX. Since the called subroutine is allowed to modify these registers,
-- if the caller relies on their values after the subroutine returns, the caller must push the values in these registers
-- onto the stack (so they can be restore after the subroutine returns.
pushList :: InstrEmitter e => [Temp] -> e ()
pushList [] = return ()
pushList (x:xs) = do
  pushOne x
  pushList xs

-- Sera util ??
pushOne :: InstrEmitter e => Temp -> e ()
pushOne temp = do
  emit $
    Oper {oassem = "push s0\n", osrc = [temp], odst = [sp], ojump = Nothing}
  return ()

popList :: InstrEmitter e => [Temp] -> e ()
popList [] = return ()
popList (x:xs) = do
  emit $ Oper {oassem = "pop d0\n", osrc = [], odst = [x, sp], ojump = Nothing}
  popList xs

-- data Relop = EQ | NE | LT | GT | LE | GE | ULT | ULE | UGT | UGE
-- Syntax
-- je <label> (jump when equal)
-- jne <label> (jump when not equal)
-- jz <label> (jump when last result was zero)
-- jg <label> (jump when greater than)
-- jge <label> (jump when greater than or equal to)
-- jl <label> (jump when less than)
-- jle <label> (jump when less than or equal to)
relop2assem :: Relop -> String
relop2assem T.EQ = "je"
relop2assem T.NE = "jne"
relop2assem T.LT = "jl"
relop2assem T.GT = "jg"
relop2assem T.LE = "jle"
relop2assem T.GE = "jge"
