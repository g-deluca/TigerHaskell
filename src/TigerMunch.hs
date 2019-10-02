module TigerMunch where

import Assem

import TigerTemp
import TigerUnique
import TigerTree

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
munchExp = undefined


-- | Definimos la mónada que instanciaremos en Emitter. Le puse Mordisco
-- | porque 'munch' significa masticar (: y me pareció oportuno
type Mordisco = StateT [Instr] StGen

instance InstrEmitter Mordisco where
    emit i = do
        st <- get
        -- Sabemos que el estado es una lista de instrucciones, así que lo
        -- agregamos de cheto.
        put $ i:st
