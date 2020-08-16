module Main
  ( main
  ) where

import           Control.Monad
import           Control.Monad.State   hiding (evalState)
import           Data.Either
import           Data.Maybe
import           System.Console.GetOpt
import qualified System.Environment    as Env
import           System.Exit

import           Assem

import           Text.Parsec           (runParser)
import           TigerAbs
import           TigerAlloc
import           TigerCanon
import           TigerEscap
import           TigerFrame
import           TigerLiveness
import           TigerMakeGraph
import           TigerMunch
import           TigerParser
import           TigerPretty
import           TigerPrettyIr
import           TigerSeman
import           TigerSymbol
import           TigerTemp
import           TigerTree             (Stm)
import           TigerUnique

data Options =
  Options
    { optArbol    :: Bool
    , optDebEscap :: Bool
    , optFrags    :: Bool
    , optMunch    :: Bool
    }
  deriving (Show)

defaultOptions :: Options
defaultOptions =
  Options
    {optArbol = False, optDebEscap = False, optFrags = False, optMunch = False}

options :: [OptDescr (Options -> Options)]
options =
  [ Option
      ['a']
      ["arbol"]
      (NoArg (\opts -> opts {optArbol = True}))
      "Muestra el AST luego de haber realizado el cálculo de escapes"
  , Option
      ['e']
      ["escapada"]
      (NoArg (\opts -> opts {optDebEscap = True}))
      "Stepper escapadas"
  , Option
      ['f']
      ["fragmentos"]
      (NoArg (\opts -> opts {optFrags = True}))
      "Muestra los fragmentos obtenidos luego de la traducción a código intermedio"
  , Option
      ['m']
      ["munch"]
      (NoArg (\opts -> opts {optMunch = True}))
      "Muestra el resultado de Munch"
  ]

compilerOptions :: [String] -> IO (Options, [String])
compilerOptions argv =
  case getOpt Permute options argv of
    (o, n, []) -> return (foldl (flip id) defaultOptions o, n)
    (_, _, errs) ->
      ioError (userError (concat errs ++ usageInfo header options))
  where
    header = "Se usa: tiger fileName [OPTIONS] "

showExp :: Exp -> IO ()
showExp e = do
  putStrLn "Mostramos el AST (PP Gracias a Emilio Lopez Junior)"
  putStrLn $ renderExp e

showFrags :: [Frag] -> IO ()
showFrags f = putStrLn $ unlines $ map renderFrag f

calculoEscapadas :: Exp -> Options -> IO Exp
calculoEscapadas rawAST opts =
  if (optDebEscap opts)
    then fail "NO DEBBUGING!"
    else either
           (\err -> putStrLn "Error de Escap:" >> fail (show err))
           return
           (calcularEEsc rawAST)

parserStep :: Options -> String -> String -> IO Exp
parserStep opts nm sc =
  either (\perr -> error $ "Parser error" ++ show perr) return $
  runParser expression () nm sc

translateStep :: Options -> Exp -> StGen [Frag]
translateStep _ exp = do
  eitherFrags <- runFrags exp
  case eitherFrags of
    Left s      -> fail $ show s
    Right frags -> return frags

canonStep :: Options -> [Stm] -> StGen [[Stm]]
canonStep _ stmts = mapM tankCanonizer stmts

munchStep :: Options -> [[Stm]] -> StGen [[Instr]]
munchStep _ stmtss = mapM runMordisco stmtss

allocStep :: Options -> [([Instr], Frame)] -> StGen [([Instr], AllocState)]
allocStep _ instrs = mapM (\(i, f) -> runAllocator i f) instrs

-- tiger :: Options -> Exp -> StGen [[Instr]]
tiger opt exp = do
  frags <- translateStep opt exp
  let (labels, stmtsWithFrames) = sepFrag frags
  let (stmts, frames) = unzip stmtsWithFrames
  canonStmts <- canonStep opt stmts
  munchInstr <- munchStep opt canonStmts
  let instrWithSink =
        zipWith (\f bd -> (procEntryExit2 f bd, f)) frames munchInstr
  -- Instrucciones "coloreadas" (con el estado pq todavía necesitamos debugging)
  newInstrsWithState <- allocStep opt instrWithSink
  let newInstrs = map fst newInstrsWithState
  let instrsWithEpiAndPrologue = map procEntryExit3 $ zip newInstrs frames
  let instrsWithReplacedTemps
        -- A pesar de que al aplicar los colores ya cambiamos el string de la
        -- instrucción, lo volvemos a hacer acá por el epílogo y prólogo
       = map replaceTemps (concat instrsWithEpiAndPrologue)
  let assembly = instrs2Strings instrsWithReplacedTemps
  let labelsAssembly = labels2Strings labels
  return $ labelsAssembly ++ "\n" ++ assembly

-- runTiger :: Options -> Exp -> IO [[Instr]]
runTiger opt = return . fst . flip TigerUnique.evalState 0 . tiger opt

main :: IO ()
main = do
  s:opts <- Env.getArgs
  (opts', _) <- compilerOptions opts
  sourceCode <- readFile s
  rawAst <- parserStep opts' s sourceCode
  ast <- calculoEscapadas rawAst opts'
  when (optArbol opts') (showExp ast)
  assembly <- runTiger opts' ast
  putStrLn assembly

labels2Strings :: [Frag] -> String
labels2Strings [] = ""
labels2Strings ((AString label strings):frags) =
  let first =
        unpack label ++
        ":" ++
        foldl
          (\declaracion linea -> declaracion ++ "\n" ++ unpack linea)
          ""
          strings ++
        "\n"
   in first ++ labels2Strings frags
labels2Strings ((Proc _ _):_) = error "Que hace esto acá?"

instrs2Strings :: [Instr] -> String
instrs2Strings instrs =
  foldl (\programa instr -> programa ++ show instr ++ "\n") "" instrs
