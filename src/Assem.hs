module Assem where

import TigerTemp (Label, Temp)

data Instr =
    Oper {
        oassem :: String,
        odst :: [Temp],
        osrc :: [Temp],
        ojump :: Maybe [Label]
    }
 |  Label {
        lassem :: String,
        llab :: Label
    }
 |  Move {
        massem :: String,
        mdst :: Temp,
        msrc :: Temp
    }

instance Show Instr where
    show (Oper oassem _ _ _) = oassem
    show (Label lassem _) = lassem
    show (Move massem _ _) = massem