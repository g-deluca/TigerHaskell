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
munchExp (T.Call e args) = error "COMPLETAR"



-- | Definimos la mónada que instanciaremos en Emitter. Le puse Mordisco
-- | porque 'munch' significa masticar (: y me pareció oportuno
type Mordisco = StateT [Instr] StGen

instance InstrEmitter Mordisco where
    emit i = do
        st <- get
        -- Sabemos que el estado es una lista de instrucciones, así que lo
        -- agregamos de cheto.
        put $ i:st
