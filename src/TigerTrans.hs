{-# LANGUAGE UndecidableInstances #-}
module TigerTrans where

import qualified Control.Monad.State           as ST
import           Prelude                 hiding ( EQ
                                                , GT
                                                , LT
                                                , error
                                                , exp
                                                , seq
                                                )
import qualified Prelude                       as P
                                                ( error )
import           TigerAbs                       ( Escapa(..) )
import qualified TigerAbs                      as Abs
import           TigerErrores
import           TigerFrame                    as F
-- import           TigerSres                      ( Externa(..) )
import           TigerSymbol                   as T
import           TigerTemp
import           TigerTree

import           Control.Monad
import qualified Data.Foldable                 as Fold
import           Data.List                     as List
import           Data.Ord                hiding ( EQ
                                                , GT
                                                , LT
                                                )


import           Debug.Trace

-- | Reexportamos el tipo de Fragmentos provenientes de TigerFrame.
type TransFrag = Frag

data Externa = Runtime | Propia
    deriving Show

-- | Tipo de datos representando si es un procedimiento o una función
data IsProc = IsProc | IsFun

-- | Empaquetadores de expresiones
-- Esto pasa ya que la información de contexto, es decir, donde están cada
-- una de las expresiones, statements y/o condicionales, lo sabemos
-- en el otro modulo, en [TigerSeman].
data BExp where
  -- | Representa una expresión. Es decir que se espera que devuelva
  -- algún resultado.
  Ex :: Exp -> BExp
  -- | Representan las computaciones que no dan resultados, es decir
  -- un /statement/
  Nx :: Stm -> BExp
  -- | Representan a expresiones que representan condicionales.
  Cx  :: ( -- | Dadas las etiquetas a donde saltar en caso de verdadero
           -- o falso.
          (Label, Label)
         -- | Y devolvemos un Statement formado correctamente.
          -> Stm)
      -> BExp

instance Show BExp where
    show (Ex e)  = "Ex " ++ show e
    show (Nx e)  = "Nx " ++ show e
    show (Cx _ ) = "Cx "

-- | Función helper /seq/ que nos permite escribir
-- fácilmente una secuencia de [Stm] usando listas.
seq :: [Stm] -> Stm
seq []       = ExpS $ Const 0
seq [s     ] = s
seq (x : xs) = Seq x (seq xs)

-- | Eventualmente vamos a querer obtener nuevamente las expresiones
-- empaquetadas por este nuevo tipo [BExp]. Para eso damos las siguientes
-- funciones des-empaquetadoras. Definidas en [7.3] del libro.

-- | Des-empaquetador de expresiones
-- Es mónadico ya que deberá crear labels, y temps
-- para des-empaquetar una condición.
unEx :: (Monad w, TLGenerator w) => BExp -> w Exp
unEx (Ex e ) = return e
unEx (Nx s ) = return $ Eseq s (Const 0)
unEx (Cx cf) = do
  r <- newTemp
  t <- newLabel
  f <- newLabel
  return $ Eseq
    (seq
      [ Move (Temp r) (Const 1)
      , cf (t, f)
      , Label f
      , Move (Temp r) (Const 0)
      , Label t
      ]
    )
    (Temp r)


-- | Des-empaquetador de statements
unNx :: (Monad w, TLGenerator w) => BExp -> w Stm
unNx (Ex e ) = return $ ExpS e
unNx (Nx s ) = return s
unNx (Cx cf) = do
  t <- newLabel
  return $ seq [cf (t, t), Label t]

-- | Des-empaquetador de condiciones
unCx :: (Monad w, TLGenerator w, Demon w) => BExp -> w ((Label, Label) -> Stm)
unCx (Nx _        ) = internal $ pack "unCx(Nx...)"
unCx (Cx cf       ) = return cf
-- Pequeña optimización boluda
unCx (Ex (Const 0)) = return (\(_, f) -> Jump (Name f) f)
unCx (Ex (Const _)) = return (\(t, _) -> Jump (Name t) t)
unCx (Ex e        ) = return (uncurry (CJump NE e (Const 0)))

-- | Los niveles son un stack de (Frame, Int)
-- Recordar que Frame es una representación del Marco Virtual.
data LevelI = MkLI {getFrame' :: Frame, getNlvl' :: Int}
  deriving Show

type Level = [LevelI]

-- | Helpers de niveles.
getFrame :: Level -> Frame
getFrame = getFrame' . head

getNlvl :: Level -> Int
getNlvl = getNlvl' . head

setFrame :: Frame -> Level -> Level
setFrame f (MkLI _ l : xs) = MkLI f l : xs
setFrame _ _               = P.error "setFrame"

newLevel :: Level -> Symbol -> [Escapa] -> Level
newLevel []                  s bs = [MkLI (newFrame s bs) 0]
newLevel ls@(MkLI _ lvl : _) s bs = MkLI (newFrame s bs) (lvl + 1) : ls

getParent :: Level -> Level
getParent []       = P.error "No fuimos del outermost level"
getParent (_ : xs) = xs

outermost :: Level
outermost = [MkLI (newFrame (pack "_tigermain") []) (-1)]

-- | Clase encargada del manejo de memoria y niveles.
-- Esta etapa va a consumir el AST y construir un nuevo lenguaje llamado Código
-- Intermedio. En este proceso vamos tomando nota cuantas variables define una
-- función o let, para eventualmente crear los marcos necesarios para le
-- ejecución de código assembler.
class (Monad w, TLGenerator w, Demon w) => MemM w where
    -- | Level management
    -- Es un entero que nos indica en qué nivel estamos actualmente.
    getActualLevel :: w Int
    upLvl :: w ()
    downLvl :: w ()
    -- | Salida management.
    -- Esta etiqueta la necesitamos porque es la que nos va permitir saltar a la
    -- salida de un while (Ver código intermedio de While). Usada en el break.
    pushSalida :: Maybe Label -> w ()
    topSalida :: w (Maybe Label)
    popSalida :: w ()
    -- | Level management Cont. El nivel en esta etapa es lo que llamamos el
    -- marco de activación virtual o dinámico (no me acuerdo). Pero es lo que
    -- eventualmente va a ser el marco de activación
    pushLevel :: Level -> w ()
    popLevel  :: w ()
    topLevel  :: w Level
    -- | Manejo de /pedido/ de memoria para variables locales.
    -- Esto básicamente debería aumentar en uno la cantidad de variables locales
    -- usadas. Es lo que se usará eventualmente para toquetear el stack o lo que
    -- sea que use la arquitectura deseada.
    allocLocal :: Escapa -> w Access
    allocLocal b = do
      -- | Dame el nivel actual
        t <- topLevel
        popLevel
      -- dame una versión modificada según lo dicte
      -- el módulo de manejo de Frame (que simula la arquitectura)
        (f,acc) <- F.allocLocal (getFrame t) b
      -- este nuevo frame es lo que vamos a usar.
        let nt = setFrame f t
        pushLevel nt
      -- y devolvemos el acceso creado. Si está en un temporal (registro) o en
      -- memoria (y en qué /offset/ del /fp/).
        return  acc
    -- | Manejo de /pedido/ de memoria para argumentos.
    -- ver lo que hicimos en /allocLocal/
    allocArg :: Escapa -> w Access
    allocArg b = do
        t <- topLevel
        popLevel
        (f,a) <- F.allocArg (getFrame t) b
        pushLevel (setFrame f t)
        return a
    -- | Frag management
    -- Básicamente los fragmentos van a ser un efecto lateral de la computación.
    -- Recuerden que los fragmentos son pedazos de código intermedio que se van
    -- a ejecutar. Y estos son un efecto lateral porque todavía no sabemos bien
    -- cómo van a ser ejecutados (eso se decide más adelante)
    pushFrag  :: Frag -> w ()
    getFrags  :: w [Frag]

-- | Generación de código intermedio.
-- Cada construcción del (AST)[src/TigerAbs.hs] la consumiremos
-- y construiremos un fragmento de código intermedio que eventualmente
--  se traducirá en código de máquina y ejecutará.
-- Algunas funciones se especializan más para conseguir un mejor código intermedio.
class IrGen w where
    -- | Esta función mágica prepara la máquina para comenzar a traducir una función o procedimiento.
    -- básicamente es la que va a agregar el Fragmento que es generado por la
    -- función y ponerlo como el efecto secundario mencionado más arriba
    procEntryExit :: Level -> BExp -> w ()
    unitExp :: w BExp
    nilExp :: w BExp
    intExp :: Int -> w BExp
    stringExp :: Symbol -> w BExp
    simpleVar :: Access -> Int -> w BExp
    varDec :: Access -> w BExp
    fieldVar :: BExp -> Int -> w BExp
    subscriptVar :: BExp -> BExp -> w BExp
    recordExp :: [(BExp,Int)]  -> w BExp
    callExp :: Label -> Externa -> IsProc -> Level -> [BExp] -> w BExp
    letExp :: [BExp] -> BExp -> w BExp
    breakExp :: w BExp
    seqExp :: [BExp] -> w BExp
    preWhileforExp :: w ()
    posWhileforExp :: w ()
    whileExp :: BExp -> BExp -> w BExp
    forExp :: BExp -> BExp -> BExp -> BExp -> w BExp
    ifThenExp :: BExp -> BExp -> w BExp
    ifThenElseExp :: BExp -> BExp -> BExp -> w BExp
    ifThenElseExpUnit :: BExp -> BExp -> BExp -> w BExp
    assignExp :: BExp -> BExp -> w BExp
    -- preFunctionDec :: Level -> w ()
    -- posFunctionDec :: w ()
    -- Esto fuerza a que haya menos opciones... ver bien con los que lleguen a
    -- este lugar..
    envFunctionDec :: Level -> w BExp -> w BExp
    functionDec :: BExp -> Level -> IsProc -> w BExp
    binOpIntExp :: BExp -> Abs.Oper -> BExp -> w BExp
    binOpIntRelExp :: BExp -> Abs.Oper -> BExp -> w BExp
    binOpStrExp :: BExp -> Abs.Oper -> BExp -> w BExp
    arrayExp :: BExp -> BExp -> w BExp

instance (MemM w) => IrGen w where
    procEntryExit lvl bd =  do
        bd' <- unNx bd
        let res = Proc bd' (getFrame lvl)
        pushFrag res
    stringExp t = do
      -- | Esto debería ser dependiente de la arquitectura...
      -- No estoy seguro que tenga que estar esto acá.
        l <- newLabel
        let ln = T.append (pack ".long ")  (pack $ show $ T.length t)
        let str = T.append (T.append (pack ".string \"") t) (pack "\"")
        pushFrag $ AString l [ln,str]
        return $ Ex $ Name l
    -- | Función utilizada para la declaración de una función.
    envFunctionDec lvl funDec = do
        -- preFunctionDec
        -- mandamos un nada al stack, por si un /break/ aparece en algún lado que
        -- no tenga un while y detectamos el error. Ver [breakExp]
        pushSalida Nothing
        upLvl
        pushLevel lvl
        fun <- funDec
        -- posFunctionDec
        -- | Cuando salimos de la función sacamos el 'Nothing' que agregamos en 'preFunctionDec'.
        popSalida
        downLvl
        -- devolvemos el código en el entorno donde fue computada.
        return fun
    -- functionDec :: BExp -> Level -> Bool -> w BExp
    functionDec bd lvl proc = do
        body <- case proc of
                  IsProc -> unNx bd
                  IsFun  -> Move (Temp rv) <$> unEx bd
        procEntryExit lvl (Nx body)
        -- TODO: Pregunar qué onda esto. Devolvemos 0 porque no hay que generar
        -- código intermedio, no?
        return $ Ex $ Const 0
    -- simpleVar :: Access -> Int -> w BExp
    simpleVar acc level = do
      actual_level <- getActualLevel
      return $ Ex (F.exp acc (actual_level - level))
    varDec acc = do { i <- getActualLevel; simpleVar acc i}
    unitExp = return $ Ex (Const 0)
    nilExp = return $ Ex (Const 0)
    intExp i = return $ Ex (Const i)
    -- fieldVar :: BExp -> Int -> w BExp
    fieldVar var i = do
      evar <- unEx var
      let offset = i * wSz
      return $ Ex $
        Mem (Binop Plus evar (Const offset))
    -- subscriptVar :: BExp -> BExp -> w BExp
    subscriptVar var ind = do
        evar <- unEx var
        eind <- unEx ind
        return $ Ex $ Mem (Binop Plus evar (Binop Mul eind (Const wSz)))
    -- recordExp :: [(BExp,Int)]  -> w BExp
    recordExp flds = do
        let size = Const $ List.length flds
        let ordered = map fst $ List.sortBy (comparing snd) flds
        inis <- mapM unEx ordered
        t <- newTemp
        return $ Ex $ Eseq (seq
          [ExpS $ externalCall "_allocRecord" (size:inis)
          , Move (Temp t) (Temp rv)
          ]) (Temp t)
    -- callExp :: Label -> Externa -> Bool -> Level -> [BExp] -> w BExp
    callExp name external isproc lvl bargs = do
        args <- mapM unEx bargs
        -- Obtenemos el nivel del llamante (caller)
        callerLevel  <- getActualLevel
        let
          -- Obtenemos el nivel de "name" (callee)
          calleeLevel   = getNlvl lvl
          -- Como llamar a la función
          call  = case external of
                       Runtime -> externalCall $ unpack name -- TODO: What's the difference with Call (Name name) ???
                       Propia  -> Call (Name name)
          -- Calculamos el static link
          -- "So on each procedure call or variable access, a chain of zero
          -- or more fetches is required; the length of the chain is just
          -- the difference in static nesting depth between the two functions involved."
          -- Pag. 134
          slink = if calleeLevel > callerLevel
                  then Temp fp --  TODO: ¿Está bien esto? ¿P.error "Error en static link"?
                  else F.auxexp (callerLevel - calleeLevel)
          args' = case external of
            Runtime -> args
            Propia -> (slink:args)
        case isproc of
            IsProc ->
                return $ Nx $
                   ExpS $ call args'
            IsFun -> do
                res <- newTemp
                return $ Ex $ call args'
    -- letExp :: [BExp] -> BExp -> w BExp
    letExp [] e = do
      -- Des-empaquetar y empaquetar como un |Ex| puede generar
      -- la creación de nuevo temporales, etc. Es decir, hay efectos que necesitamos contemplar.
      -- Ver la def de |unEx|
            e' <- unEx e
            return $ Ex e'
    letExp bs body = do
        bes <- mapM unNx bs
        be <- unEx body
        return $ Ex $ Eseq (seq bes) be
    -- breakExp :: w BExp
    -- | JA! No está implementado
    breakExp = do
      mlabel <- topSalida
      case mlabel of
        Just label -> return $ Nx $ Jump (Name label) label
        _ -> derror $ pack "Break fuera de lugar"
    -- seqExp :: [BExp] -> w BExp
    seqExp [] = return $ Nx $ ExpS $ Const 0
    seqExp bes = case last bes of
            Nx _ -> Nx . seq <$> mapM unNx bes
            Ex e' -> do
                    let bfront = init bes
                    ess <- mapM unNx bfront
                    return $ Ex $ Eseq (seq ess) e'
            _ -> internal $ pack "WAT!123"
    -- preWhileforExp :: w ()
    preWhileforExp = newLabel >>= pushSalida . Just
    -- posWhileforExp :: w ()
    posWhileforExp = popSalida
    -- whileExp :: BExp -> BExp -> Level -> w BExp
    -- | While Loop.
    -- ```
    --   test:
    --        if (condition) goto body else done
    --        body:
    --             body (Un break acá se traduce como un salto a done)
    --        goto test
    --   done:
    -- ```
    whileExp cond body = do
        -- | Desempaquetamos la condición como un condicional
        ccond <- unCx cond
        -- | Desempaquetamos el body como un statement
        cbody <- unNx body
        -- | Creamos dos etiquetas para los saltos del if
        -- una correspondiente al test
        test <- newLabel
        -- | otra correspondiente al cuerpo
        body <- newLabel
        -- | buscamos en el stackla etiqueta de salida (done).
        lastM <- topSalida
        case lastM of
            Just done ->
                return $ Nx $ seq
                    [Label test
                    , ccond (body,done)
                    , Label body
                    , cbody
                    , Jump (Name test) test
                    , Label done]
            _ -> internal $ pack "no label in salida"
    -- forExp :: BExp -> BExp -> BExp -> BExp -> w BExp
    forExp lo hi var body = do
      -- | Asignamos el valor de lo a la variable var (valor inicial)
      elo <- unEx lo
      evar <- unEx var
      ehi <- unEx hi
      -- | Desempaquetamos el body como un statement
      sbody <- unNx body
      -- | Creamos los labels necesarios
      test <- newLabel
      body <- newLabel
      lastM <- topSalida
      case lastM of
        Just done ->
            return $ Nx $ seq
                [Move evar elo
                ,Label test
                ,CJump LE evar ehi body done
                ,Label body
                ,sbody
                ,Move evar (Binop Plus evar (Const 1))
                ,Jump (Name test) test
                ,Label done]
        _ -> internal $ pack "no label in salida"
    -- ifThenExp :: BExp -> BExp -> w BExp
    ifThenExp cond body = do
      -- | Desempaquetamos la condición como un condicional
      ccond <- unCx cond
      -- | Desempaquetamos el body como un statement
      cbody <- unNx body
      -- | creamos dos etiquetas para los saltos del if
      -- | una correspondiente a la salida
      done <- newLabel
      -- | otra correspondiente al cuerpo
      body <- newLabel
      return $ Nx $ seq
          [ ccond (body,done)
          , Label body
          , cbody
          , Label done]

    -- ifThenElseExp :: BExp -> BExp -> BExp -> w BExp
    ifThenElseExp cond body els = do
      -- | Desempaquetamos la condición como un condicional
      ccond <- unCx cond
      -- | Desempaquetamos el body como un statement
      cbody <- unEx body
      celse <- unEx els
      -- | creamos dos etiquetas para los saltos del if
      -- | una correspondiente al caso verdadero
      ltrue <- newLabel
      -- | otra correspondiente al caso falso
      lfalse <- newLabel
      -- | una ultima para la salida del true
      ldone <- newLabel
      -- | Creamos un registro para devolver el resultado
      ret <- newTemp
      return $ Ex $ Eseq (seq
          [ ccond (ltrue,lfalse)
          , Label ltrue
          , Move (Temp ret) cbody
          , Jump (Name ldone) ldone
          , Label lfalse
          , Move (Temp ret) celse
          , Label ldone])
          (Temp ret)

    -- ifThenElseExpUnit :: BExp -> BExp -> BExp -> w BExp
    ifThenElseExpUnit cond body els = do
      -- | Desempaquetamos la condición como un condicional
      ccond <- unCx cond
      -- | Desempaquetamos el body como un statement
      cbody <- unNx body
      celse <- unNx els
      -- | creamos dos etiquetas para los saltos del if
      -- | una correspondiente al caso verdadero
      ltrue <- newLabel
      -- | otra correspondiente al caso falso
      lfalse <- newLabel
      -- | una ultima para la salida del true
      ldone <- newLabel
      return $ Nx $ seq
          [ ccond (ltrue,lfalse)
          , Label ltrue
          , cbody
          , Jump (Name ldone) ldone
          , Label lfalse
          , celse
          , Label ldone]

    -- assignExp :: BExp -> BExp -> w BExp
    assignExp cvar cinit = do
        cvara <- unEx cvar
        cin <- unEx cinit
        case cvara of
            Mem v' ->  do
                t <- newTemp
                return $ Nx $ seq [Move (Temp t) cin, Move cvara (Temp t)]
            Temp _ -> return $ Nx $ Move cvara cin
            a -> fail $ show a
    -- binOpIntExp :: BExp -> Abs.Oper -> BExp -> w BExp
    binOpIntExp le op re = do
      ele <- unEx le
      ere <- unEx re
      case op of
        Abs.PlusOp -> return $ Ex $ Binop Plus ele ere
        Abs.MinusOp -> return $ Ex $ Binop Minus ele ere
        Abs.TimesOp -> return $ Ex $ Binop Mul ele ere
        Abs.DivideOp -> return $ Ex $ Binop Div ele ere
        _ ->  internal $ pack "deberia llamarse a binOpIntRelExp"

    binOpIntRelExp le op re = do
      ele <- unEx le
      ere <- unEx re
      t <- newLabel
      f <- newLabel
      done <- newLabel

      ret <- newTemp

      -- TODO: Esta bien devolver Ex? Deberíamos devolver Cx? Diferencia?
      return $ Ex $ (Eseq (seq
        [CJump (abs2Tree op) ele ere t f
        , Label t
        , Move (Temp ret) (Const 1)
        , Jump (Name done) done
        , Label f
        , Move (Temp ret) (Const 0)
        , Label done
        ])
        (Temp ret))

    -- binOpStrExp :: BExp -> Abs.Oper -> BExp -> w BExp
    -- TODO: Se puede comparar un string con otros operadores que no sean estos?
    binOpStrExp strl op strr = do
      estrl <- unEx strl
      estrr <- unEx strr
      t <- newTemp
      case op of
        Abs.EqOp ->
          return $ Ex $ Eseq (seq
            [ExpS $ externalCall "_stringCompare" [estrl, estrr]
            ,Move (Temp t) (Temp rv)
            ]) (Temp t)
        Abs.NeqOp ->
          return $ Ex $ Eseq (seq
            [ExpS $ externalCall "_stringCompare" [estrl, estrr]
            , Move (Temp t) (Temp rv)
            ]) (Temp t)
        _ -> internal $ pack "estas haciendo algo raro con binOpStrExp"

    -- arrayExp :: BExp -> BExp -> w BExp
    arrayExp size init = do
        sz <- unEx size
        ini <- unEx init
        t <- newTemp
        return $ Ex $ Eseq (seq
                [ExpS $ externalCall "_allocArray" [sz,ini]
                , Move (Temp t) (Temp rv)
                ]) (Temp t)


-- Funcion auxiliar
abs2Tree :: Abs.Oper -> Relop
abs2Tree Abs.EqOp = EQ
abs2Tree Abs.NeqOp = NE
abs2Tree Abs.LtOp = LT
abs2Tree Abs.LeOp = LE
abs2Tree Abs.GtOp = GT
abs2Tree Abs.GeOp = GE
abs2Tree _ = P.error "[abs2Tree] ups! falta implementar"