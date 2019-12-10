module TigerSres where

import           TigerTips
import           TigerTemp
import           TigerUnique
import TigerTrans
import TigerFrame

-- | 'Externa' representa la idea si una función pertenece al /runtime/ o no.
-- data Externa = Runtime | Propia
--     deriving Show

-- type FunEntry = (Unique, Label, [Tipo], Tipo, Externa)
-- Now our "Unique" is the level where the function is defined
-- since it can't be that two functions are declared at the same
-- Level with the same Label
type FunEntry = (Level, Label, [Tipo], Tipo, Externa)


type ValEntry = (Tipo, Access, Int)

data EnvEntry =
    Var ValEntry | Func FunEntry
    deriving Show
