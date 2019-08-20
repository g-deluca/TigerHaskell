module TigerSres where

import           TigerTips
import           TigerTemp
import           TigerUnique
import TigerTrans
import TigerFrame

-- | 'Externa' representa la idea si una función pertenece al /runtime/ o no.
data Externa = Runtime | Propia
    deriving Show

-- type FunEntry = (Unique, Label, [Tipo], Tipo, Externa)
-- TODO: Cambiamos el Unique por Level a propósito?
type FunEntry = (Level, Label, [Tipo], Tipo, Externa)


type ValEntry = (Tipo, Access, Int)

data EnvEntry =
    Var ValEntry | Func FunEntry
    deriving Show
