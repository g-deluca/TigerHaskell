module TigerMunch where

import Assem

import TigerTemp
import TigerUnique
import TigerTree as T

import Control.Monad.State

class (Monad w, TLGenerator w) => InstrEmitter w where
    -- | Acumula la lista de instrucciones que se retornarán luego
    emit :: Instr -> w ()
    -- | Función auxiliar para munchExp. La agrego porque está en el
    -- | libro pero no sé qué tan útil es en Haskell.
    result :: (Temp -> w ()) -> w Temp
    result gen = do
        res <- newTemp
        gen res
        return res

munchStm :: InstrEmitter w => Stm -> w ()
munchStm = undefined

munchExp :: InstrEmitter w => Exp -> w Temp
munchExp (Const i) = do
    r <- newTemp
    emit $ Oper {
        oassem = "mov d0, " ++ show i ++ "\n",
        osrc = [],
        odst = [r],
        ojump = Nothing
    }
    return r
munchExp (Name l) = do
    r <- newTemp
    emit $ Oper {
        -- TODO: Googlear un poco más offset
        oassem = "mov d0, OFFSET " ++ show l ++ "\n",
        osrc = [],
        odst = [r],
        ojump = Nothing
    }
    return r
munchExp (Temp t) = return t
munchExp (Eseq s e) = munchStm s >> munchExp e
munchExp (T.Mem (T.Binop T.Plus e (T.Const i))) = do
-- TODO: ¿Por qué agarramos puntualmente el caso de "Plus" y no
-- el resto. Así está en el libro, ¿but why?
    res <- newTemp
    te <- munchExp e
    emit $ Oper {
        oassem = "mov d0, [s0 + " ++ show i ++ "]\n",
        osrc = [te],
        odst = [res],
        ojump = Nothing
    }
    return res
munchExp (T.Mem (T.Binop T.Plus (T.Const i) e)) = do
    res <- newTemp
    te <- munchExp e
    emit $ Oper {
        oassem = "mov d0, [s0 + " ++ show i ++ "]\n",
        osrc = [te],
        odst = [res],
        ojump = Nothing
    }
    return res
munchExp (T.Mem (T.Const i)) = do
    res <- newTemp
    emit $ Oper {
        oassem = "mov d0, [" ++ show i ++ "]\n",
        osrc = [],
        odst = [res],
        ojump = Nothing
    }
    return res
munchExp (T.Mem e) = do
    te <- munchExp e
    res <- newTemp
    emit $ Oper {
        oassem = "mov d0, [s0]\n",
        osrc = [te],
        odst = [res],
        ojump = Nothing
    }
    return res

-- Arrancamo' con Binop...
munchExp (T.Binop T.Plus el er) = do
    tl <- munchExp el
    tr <- munchExp er
    res <- newTemp
    -- TODO: ¿Usar move?
    emit $ Oper {
        oassem = "mov d0, s0\n",
        osrc = [tl],
        odst = [res],
        ojump = Nothing
    }
    emit $ Oper {
        oassem = "add d0, s0\n",
        osrc = [tr, res],
        odst = [res],
        ojump = Nothing
    }
    return res
munchExp (T.Binop T.Minus el er) = do
    tl <- munchExp el
    tr <- munchExp er
    res <- newTemp
    emit $ Oper {
        oassem = "mov d0, s0\n",
        osrc = [tl],
        odst = [res],
        ojump = Nothing
    }
    emit $ Oper {
        oassem = "sub d0, s0\n",
        osrc = [tr, res],
        odst = [res],
        ojump = Nothing
    }
    return res
-------------
-- Pequeña optimización ;)
munchExp (T.Binop T.Mul (Const i) er) = do
    tr <- munchExp er
    res <- newTemp
    emit $ Oper {
        oassem = "imul d0, s0, " ++ show i ++ "\n",
        osrc = [tr],
        odst = [res],
        ojump = Nothing
    }
    return res
munchExp (T.Binop T.Mul el (Const i)) = do
    tl <- munchExp el
    res <- newTemp
    emit $ Oper {
        oassem = "imul d0, s0, " ++ show i ++ "\n",
        osrc = [tl],
        odst = [res],
        ojump = Nothing
    }
    return res
-------------
munchExp (T.Binop T.Mul el er) = do
    tl <- munchExp el
    tr <- munchExp er
    res <- newTemp
    emit $ Oper {
        oassem = "mov d0, s0\n",
        osrc = [tl],
        odst = [res],
        ojump = Nothing
    }
    emit $ Oper {
        oassem = "imul d0, s0\n",
        osrc = [tr, res],
        odst = [res],
        ojump = Nothing
    }
    return res
munchExp (T.Binop T.Div el er) = error "COMPLETAR"
munchExp (T.Binop T.And el er) = do
    tl <- munchExp el
    tr <- munchExp er
    res <- newTemp
    emit $ Oper {
        oassem = "mov d0, s0\n",
        osrc = [tl],
        odst = [res],
        ojump = Nothing
    }
    emit $ Oper {
        oassem = "and d0, s0\n",
        osrc = [tr, res],
        odst = [res],
        ojump = Nothing
    }
    return res
munchExp (T.Binop T.Or el er) = do
    tl <- munchExp el
    tr <- munchExp er
    res <- newTemp
    emit $ Oper {
        oassem = "mov d0, s0\n",
        osrc = [tl],
        odst = [res],
        ojump = Nothing
    }
    emit $ Oper {
        oassem = "or d0, s0\n",
        osrc = [tr, res],
        odst = [res],
        ojump = Nothing
    }
    return res
munchExp (T.Binop T.XOr el er) = do
    tl <- munchExp el
    tr <- munchExp er
    res <- newTemp
    emit $ Oper {
        oassem = "mov d0, s0\n",
        osrc = [tl],
        odst = [res],
        ojump = Nothing
    }
    emit $ Oper {
        oassem = "xor d0, s0\n",
        osrc = [tr, res],
        odst = [res],
        ojump = Nothing
    }
    return res
munchExp (T.Binop T.LShift el er) = error "JAJAJAJAJ"
munchExp (T.Binop T.RShift el er) = error "FUck off"
munchExp (T.Binop T.ARShift el er) = error "Seriously fUck off"
munchExp (T.Call e args) = do
    -- Primero guardamos en stack los callersaves
    pushList callersaves
    -- Ahora los argumentos en los registros de params
    munchArgs args
    -- Llamamos a la función
    emit $ Oper {
        oassem = "call " ++ show e
        osrc = [],
        odst = calldefs
        ojump = Nothing
    }
    -- Remove the parameters from stack. This restores the stack to its state before the call was performed.
    emit $ Oper {
        oassem = "add s0 " ++ (show (length args * wSz))
        osrc = [],
        odst = [esp]
        ojump = Nothing
    }
    -- Restore the contents of caller-saved registers (EAX, ECX, EDX) by popping them off of the stack.
    -- The caller can assume that no other registers were modified by the subroutine.
    popList (reverse callersaves)
    return eax

-- To pass parameters to the subroutine, push them onto the stack before the call.
-- The parameters should be pushed in inverted order (i.e. last parameter first).
-- Since the stack grows down, the first parameter will be stored at the lowest address
-- (this inversion of parameters was historically used to allow functions to be passed a variable number of parameters).
munchArgs :: InstrEmitter e => [Exp] -> e ()
munchArgs [] = return ()
munchArgs args = do
    arg <- munchExp (last args)
    emit $ Oper {
        oassem = "push d0\n",
        osrc = [arg],
        odst = [sp], --porque lo mueve
        ojump = Nothing
    }
    munchArgs (init args)

-- Before calling a subroutine, the caller should save the contents of certain registers that are designated caller-saved.
-- The caller-saved registers are EAX, ECX, EDX. Since the called subroutine is allowed to modify these registers,
-- if the caller relies on their values after the subroutine returns, the caller must push the values in these registers
-- onto the stack (so they can be restore after the subroutine returns.
pushList :: InstrEmitter e => [Temp] -> e ()
pushList [] = return ()
pushList (x:xs) = do
    emit $ Oper {
        oassem = "push d0\n",
        osrc = [x],
        odst = [sp],
        ojump = Nothing
    }
    pushList xs

-- Sera util ??
pushOne :: InstrEmitter e => Temp -> e ()
pushOne temp = do
    emit $ {
        oassem = "push d0\n",
        osrc = [temp],
        odst = [sp],
        ojump = Nothing
    }
    return ()

popList :: InstrEmitter e => [Temp] -> e ()
popList [] = return ()
popList (x:xs) = do
    emit $ Oper {
        oassem = "pop " ++ show x
        osrc = [],
        odst = [x],
        ojump = Nothing
    }
    popList xs

-- | Definimos la mónada que instanciaremos en Emitter. Le puse Mordisco
-- | porque 'munch' significa masticar (: y me pareció oportuno
type Mordisco = StateT [Instr] StGen

instance InstrEmitter Mordisco where
    emit i = do
        st <- get
        -- Sabemos que el estado es una lista de instrucciones, así que lo
        -- agregamos de cheto.
        put $ i:st
