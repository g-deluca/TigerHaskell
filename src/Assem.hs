module Assem where

-- Algunos links con doc. x86
-- https://www.cs.virginia.edu/~evans/cs216/guides/x86.html
-- https://www.felixcloutier.com/x86/

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