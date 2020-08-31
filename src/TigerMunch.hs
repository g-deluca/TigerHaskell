module TigerMunch where

import           Assem               as A

import           TigerFrame
import           TigerTemp
import           TigerTree           as T
import           TigerUnique

import           Control.Monad.State
import           Data.Text           (unpack)
import           Debug.Trace

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
munchStm (T.Move (Temp t) e2) = do
  t2 <- munchExp e2
  emit $ A.Move {massem = "mov s0, d0\n", msrc = t2, mdst = t}
munchStm (T.Move (Mem e1) e2) = do
  t1 <- munchExp e1
  t2 <- munchExp e2
  emit $
    A.Oper
      {oassem = "mov s0, (s1)\n", osrc = [t2, t1], odst = [], ojump = Nothing}
munchStm (T.Move (Name l) e2) = do
  t2 <- munchExp e2
  let label = makeStringL l
  emit $
    A.Oper
      { oassem = "mov s0, " ++ label ++ "\n"
      , osrc = [t2]
      , odst = []
      , ojump = Nothing
      }
munchStm (T.Move e1 e2) -- T.Move e1 e2 => e1 <- e2
 = do
  stm1 <- munchExp e1
  stm2 <- munchExp e2
  emit $ A.Move {massem = "mov s0, d0\n", msrc = stm2, mdst = stm1}
munchStm (Jump (Name _) label) = do
  emit $
    Oper
      { oassem = "jmp " ++ makeStringL label ++ "\n"
      , osrc = []
      , odst = []
      , ojump = Just [label]
      }
  -- return ()
munchStm (CJump relop e1 e2 l1 l2) = do
  te1 <- munchExp e1
  te2 <- munchExp e2
    -- Primero realizo la comparación
  emit $
    Oper
      -- osrc = [te2, te1] por la arq. hace la comparacion al revés
      -- cmp $5, %rax
      -- je  equals   # jmp equals if RAX == 5
      {oassem = "cmp s0, s1\n", osrc = [te2, te1], odst = [], ojump = Nothing}
    -- Saltamos dependiendo del tipo de operacion fue True
  let assemRelOp = relop2assem relop
  emit $
    Oper
      { oassem = assemRelOp ++ " " ++ makeStringL l1 ++ "\n"
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
  -- return ()
munchStm (T.Label label) = do
  emit $ A.Label {lassem = makeStringL label ++ ":\n", llab = label}
  -- return ()

munchExp :: InstrEmitter w => Exp -> w Temp
munchExp (Const i) = do
  r <- newTemp
  emit $
    Oper
      { oassem = "mov $" ++ show i ++ ", d0\n"
      , osrc = []
      , odst = [r]
      , ojump = Nothing
      }
  return r
munchExp (Name l) = do
  r <- newTemp
  emit $
    Oper
      { oassem = "mov $" ++ makeStringL l ++ ", d0\n"
      , osrc = []
      , odst = [r]
      , ojump = Nothing
      }
  return r
munchExp (Temp t) = return t
munchExp (Eseq s e) = munchStm s >> munchExp e
-- munchExp (T.Mem (T.Binop T.Plus e (T.Const i))) = do
--   res <- newTemp
--   te <- munchExp e
--   emit $
--     Oper
--       { oassem = "mov (s0, $" ++ show i ++ "), d0\n" -- mov (s0, $i), d0 => d0 = Mem[s0 + $i]
--       , osrc = [te]
--       , odst = [res]
--       , ojump = Nothing
--       }
--   return res
munchExp (T.Mem (T.Binop T.Plus (T.Const i) e)) = do
  res <- newTemp
  te <- munchExp e
  emit $
    Oper
      { oassem = "mov " ++ show i ++ "(s0), d0\n"
      , osrc = [te]
      , odst = [res]
      , ojump = Nothing
      }
  return res
munchExp (T.Mem e) = do
  te <- munchExp e
  res <- newTemp
  emit $
    Oper {oassem = "mov (s0), d0\n", osrc = [te], odst = [res], ojump = Nothing}
  return res
-- Arrancamo' con Binop...
munchExp (T.Binop T.Plus el er) = do
  tl <- munchExp el
  tr <- munchExp er
  res <- newTemp
  -- en d0 pongo el valor de s0, res = tl
  emit $ A.Move {massem = "mov s0, d0\n", msrc = tl, mdst = res}
  -- en d0 = (d0) + (s0)
  emit $
    Oper
      {oassem = "add s0, d0\n", osrc = [tr, res], odst = [res], ojump = Nothing}
  return res
munchExp (T.Binop T.Minus el er) = do
  tl <- munchExp el
  tr <- munchExp er
  res <- newTemp
  emit $ A.Move {massem = "mov s0, d0\n", msrc = tl, mdst = res}
  emit $
    Oper
      {oassem = "sub s0, d0\n", osrc = [tr, res], odst = [res], ojump = Nothing}
  return res
munchExp (T.Binop T.Mul el er) = do
  tl <- munchExp el
  tr <- munchExp er
  res <- newTemp
  emit $ A.Move {massem = "mov s0, d0\n", msrc = tl, mdst = res}
  emit $
    Oper
      { oassem = "imul s0, d0\n"
      , osrc = [tr, res]
      , odst = [res]
      , ojump = Nothing
      }
  return res
munchExp (T.Binop T.Div el er)
  -- idivq divisor --> (edx:eax) / divisor => eax resultado, edx resto
 = do
  tl <- munchExp el
  tr <- munchExp er
  -- pongo 0's en eax porque no me interesa dividir en 128bits
  emit $
    Oper {oassem = "mov $0, d0\n", osrc = [], odst = [eax], ojump = Nothing}
  emit $ A.Move {massem = "mov s0, d0\n", msrc = tl, mdst = edx}
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
  emit $ A.Move {massem = "mov s0, d0\n", msrc = tl, mdst = res}
  emit $
    Oper
      {oassem = "and s0, d0\n", osrc = [tr, res], odst = [res], ojump = Nothing}
  return res
munchExp (T.Binop T.Or el er) = do
  tl <- munchExp el
  tr <- munchExp er
  res <- newTemp
  emit $ A.Move {massem = "mov s0, d0\n", msrc = tl, mdst = res}
  emit $
    Oper
      {oassem = "or s0, d0\n", osrc = [tr, res], odst = [res], ojump = Nothing}
  return res
munchExp (T.Binop T.XOr el er) = do
  tl <- munchExp el
  tr <- munchExp er
  res <- newTemp
  emit $ A.Move {massem = "mov s0, d0\n", msrc = tl, mdst = res}
  emit $
    Oper
      {oassem = "xor s0, d0\n", osrc = [tr, res], odst = [res], ojump = Nothing}
  return res
-- Esta es la parte callee (ya entre a la funcion que fue llamada, que hago antes y despues?)
munchExp (T.Call (Name n) args)
  -- Llamada a procedimiento -- devuelve algo
  -- TODO: Esto lo debería hacer el caller, si lo hago primero acá no hay problema?
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
      { oassem = "add $" ++ (show (length args * wSz)) ++ ", d0\n"
      , osrc = []
      , odst = [sp]
      , ojump = Nothing
      }
  -- Restore the contents of caller-saved registers (EAX, ECX, EDX) by popping them off of the stack.
  -- The caller can assume that no other registers were modified by the subroutine.
  return eax
munchExp _ = error "No implementado"

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
munchArgs :: InstrEmitter e => [Exp] -> e ()
munchArgs [] = return ()
munchArgs args = do
  arg <- munchExp (last args)
  emit $ Oper {oassem = "push s0\n", osrc = [arg], odst = [sp], ojump = Nothing}
  munchArgs (init args)

-- Before calling a subroutine, the caller should save the contents of certain registers that are designated caller-saved.
emitPushList :: InstrEmitter e => [Temp] -> e ()
emitPushList [] = return ()
emitPushList (reg:regs) = do
  emit $ Oper {oassem = "push s0\n", osrc = [reg], odst = [sp], ojump = Nothing}
  emitPushList regs

emitPopList :: InstrEmitter e => [Temp] -> e ()
emitPopList [] = return ()
emitPopList (reg:regs) = do
  emit $
    Oper {oassem = "pop d0\n", osrc = [], odst = [reg, sp], ojump = Nothing}
  emitPopList regs

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
