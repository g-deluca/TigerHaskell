module Assem where

-- Algunos links con doc. x86-64
-- https://cs.brown.edu/courses/cs033/docs/guides/x64_cheatsheet.pdf
-- http://6.s081.scripts.mit.edu/sp18/x86-64-architecture-guide.html
import           TigerTemp (Label, Temp, makeStringL)

data Instr
  = Oper
      { oassem :: String
      , osrc   :: [Temp]
      , odst   :: [Temp]
      , ojump  :: Maybe [Label]
      }
  | Label
      { lassem :: String
      , llab   :: Label
      }
  | Move
      { massem :: String
      , msrc   :: Temp
      , mdst   :: Temp
      }

instance Show Instr where
  show (Oper oassem src dst jump) = oassem
  show (Label lassem llab)        = lassem
  show (Move massem src dst)      = massem

getTemps' :: Instr -> [Temp]
getTemps' (Oper _ dsts srcs _) = dsts ++ srcs
getTemps' (Label _ _)          = []
getTemps' (Move _ d s)         = [d] ++ [s]

getTemps :: [Instr] -> [Temp]
getTemps body = foldr (\instr acc -> (getTemps' instr) ++ acc) [] body
