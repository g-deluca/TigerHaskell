module TigerSeman where

import           TigerAbs
import           TigerErrores               as E
import           TigerSres
import           TigerSymbol
import           TigerTips
import           TigerUnique
import           TigerTopSort

-- Segunda parte imports:
import           TigerTemp
-- import           TigerTrans

-- Monads
import qualified Control.Conditional        as C
import           Control.Monad
import           Control.Monad.State
import           Control.Monad.Trans.Except

-- Data
import           Data.List                  as List
import           Data.Map                   as M
import           Data.Ord                   as Ord

-- Le doy nombre al Preludio.
import           Prelude                    as P

-- Debugging. 'trace :: String -> a -> a'
-- imprime en pantalla la string cuando se ejecuta.
import           Debug.Trace                (trace)

-- * Análisis Semántico, aka Inferidor de Tipos

-- ** Notas :

-- [1] No deberían fallar las búsquedas de variables. Recuerden que
-- el calculo de variables escapadas debería detectar las variables
-- no definidas.

-- [2] En la siguiente etapa vamos a ir generando el código intermedio
-- mezclado con esta etapa por lo que es muy posible que tengan que revisar
-- este modulo. Mi consejo es que sean /lo más ordenados posible/ teniendo en cuenta
-- que van a tener que reescribir bastante.

class (Demon w, Monad w, UniqueGenerator w) => Manticore w where
  -- | Inserta una Variable al entorno
    insertValV :: Symbol -> ValEntry -> w a -> w a
  -- | Inserta una Función al entorno
    insertFunV :: Symbol -> FunEntry -> w a -> w a
  -- | Inserta una Variable de sólo lectura al entorno
    insertVRO :: Symbol -> w a -> w a
  -- | Inserta una variable de tipo al entorno
    insertTipoT :: Symbol -> Tipo -> w a -> w a
  -- | Busca una función en el entorno
    getTipoFunV :: Symbol -> w FunEntry
  -- | Busca una variable en el entorno. Ver [1]
    getTipoValV :: Symbol -> w ValEntry
  -- | Busca un tipo en el entorno
    getTipoT :: Symbol -> w Tipo
  -- | Funciones de Debugging!
    showVEnv :: w a -> w a
    showTEnv :: w a -> w a
    --
    -- | Función monadica que determina si dos tipos son iguales.
    -- El catch está en que tenemos una especie de referencia entre los
    -- nombres de los tipos, ya que cuando estamos analizando la existencia de bucles
    -- en la definición permitimos cierto alias hasta que los linearizamos con el
    -- sort topológico.
    tiposIguales :: Tipo -> Tipo -> w Bool
    tiposIguales (RefRecord s) l@(TRecord _ u) = do
        st <- getTipoT s
        case st of
            TRecord _ u1 -> return (u1 == u)
            ls@RefRecord{} -> tiposIguales ls l
            _ -> E.internal $ pack "No son tipos iguales... 123+1"
    tiposIguales l@(TRecord _ u) (RefRecord s) = do
        st <- getTipoT s
        case st of
            TRecord _ u1 -> return (u1 == u)
            ls@RefRecord{} -> tiposIguales l ls
            _ -> E.internal $ pack "No son tipos iguales... 123+2"
    tiposIguales (RefRecord s) (RefRecord s') = do
        s1 <- getTipoT s
        s2 <- getTipoT s'
        tiposIguales s1 s2
    tiposIguales TNil  (RefRecord _) = return True
    tiposIguales (RefRecord _) TNil = return True
    tiposIguales (RefRecord _) _ = E.internal $ pack "No son tipos iguales... 123+3"
    tiposIguales  e (RefRecord s) = E.internal $ pack $ "No son tipos iguales... 123+4" ++ (show e ++ show s)
    tiposIguales a b = return (equivTipo a b)

-- | Definimos algunos helpers

-- | `addpos` nos permite agregar información al error.
addpos :: (Demon w, Show b) => w a -> b -> w a
addpos t p = E.adder t (pack $ show p)

-- | Patrón de errores...
errorTiposMsg :: (Demon w, Show p)
              => p -> String -> Tipo -> Tipo -> w a
errorTiposMsg p msg t1 t2 = flip addpos p
    $ flip adder (pack msg)
    $ errorTipos t1 t2

depend :: Ty -> [Symbol]
depend (NameTy s)    = [s]
depend (ArrayTy s)   = [s]
depend (RecordTy ts) = concatMap (depend . snd) ts


-- | Función auxiliar que chequea cuales son los tipos
-- comparables.
-- Por ejemplo, ` if nil = nil then ...` es una expresión ilegal
-- ya que no se puede determinar el tipo de cada uno de los nils.
-- Referencia: [A.3.Expressions.Nil]
tiposComparables :: Tipo -> Tipo -> Oper -> Bool
tiposComparables TNil TNil EqOp  = False
tiposComparables TUnit _ EqOp    = False
tiposComparables _ _ EqOp        = True
tiposComparables TNil TNil NeqOp = False
tiposComparables TUnit _ NeqOp   = False
tiposComparables _ _ NeqOp       = True
tiposComparables _ _ _           = True

-- | Función que chequea que los tipos de los campos sean los mismos
-- Ver 'transExp (RecordExp ...)'
-- Ver 'transExp (CallExp ...)'
cmpZip :: (Demon m, Monad m) => [(Symbol, Tipo)] -> [(Symbol, Tipo, Int)] -> m () --Bool
cmpZip [] [] = return ()
cmpZip [] _ = derror $ pack "Diferencia en la cantidad. 1"
cmpZip _ [] = derror $ pack "Diferencia en la cantidad. 2"
cmpZip ((sl,tl):xs) ((sr,tr,p):ys) =
        if (equivTipo tl tr && sl == sr)
        then cmpZip xs ys
        else errorTipos tl tr

-- Función auxiliar que utilizaremos para separar una lista utilizando una
-- función que separe los elementos en pares.
splitWith :: (a -> Either b c) -> [a] -> ([b], [c])
addIzq :: ([a], [b]) -> a -> ([a],[b])
addDer :: ([a], [b]) -> b -> ([a],[b])
splitWith f = P.foldr (\x rs -> either (addIzq rs) (addDer rs) (f x)) ([] , [])
addIzq (as,bs) a = (a : as, bs)
addDer (as,bs) b = (as, b : bs)

buscarM :: Symbol -> [(Symbol, Tipo, Int)] -> Maybe Tipo
buscarM s [] = Nothing
buscarM s ((s',t,_):xs) | s == s' = Just t
                        | otherwise = buscarM s xs

-- | __Completar__ 'transVar'.
-- El objetivo de esta función es obtener el tipo
-- de la variable a la que se está __accediendo__.
-- ** transVar :: (MemM w, Manticore w) => Var -> w (BExp, Tipo)
transVar :: (Manticore w) => Var -> w ( () , Tipo)
transVar (SimpleVar s)      = do
  tipo_s <- getTipoValV s -- Nota [1]
  return ((), tipo_s)
transVar (FieldVar v s)     = do
  ((), tipo_v) <- transVar v
  -- Chequeamos que tipo_v es TRecord
  case tipo_v of
    TRecord fields _ ->
      maybe
        -- Puede ocurrir que se intente acceder a un record mediante un campo inexistente
        (derror (pack ("El campo " ++ show s ++ " no pertenece al record " ++ show v)))
        (\tipo_s -> return ((), tipo_s))
        (buscarM s fields)
    _ -> derror (pack ("La variable " ++ show v ++ " no es un record"))
transVar (SubscriptVar v e) = do
  ((), tipo_v) <- transVar v
  case tipo_v of
    -- Los arreglos son todos del mismo tipo
    TArray tipo_s _ -> return ((), tipo_s)
    _ -> derror (pack ("La variable " ++ show v ++ " no es un array"))

-- | __Completar__ 'TransTy'
-- El objetivo de esta función es dado un tipo
-- que proviene de la gramática, dar una representación
-- de tipo interna del compilador

-- | Nota para cuando se generarte código intermedio
-- que 'TransTy ' no necesita ni 'MemM ' ni devuelve 'BExp'
-- porque no se genera código intermedio en la definición de un tipo.
transTy :: (Manticore w) => Ty -> w Tipo
transTy (NameTy s)      = getTipoT s
transTy (RecordTy flds) = do
  -- Tomo los nombres de los campos del record en el orden que los declararon
  let symbols =  P.map fst flds
  -- Traduzco los tipos de cada uno de ellos en el mismo orden. Uso \mapM\ porque
  -- hay que "liftear" la mónada de transTy
  tipos <- mapM (transTy . snd) flds
  -- Por último, junto todo y le agrego la posición
  let fields = zip3 symbols tipos [0..]
  -- TODO: Revisar que Manticore sea una instancia de Unique
  unique <- mkUnique
  return (TRecord fields unique)
transTy (ArrayTy s) = do
  unique <- mkUnique
  tipo_s <- getTipoT s
  return (TArray tipo_s unique)


fromTy :: (Manticore w) => Ty -> w Tipo
fromTy (NameTy s) = getTipoT s
fromTy _ = P.error "no debería haber una definición de tipos en los args..."

-- | transDecs es la encargada de tipar las definiciones y posteriormente
-- generar código intermedio solamente para las declaraciones de variables.
----------------------------------------
-- Aquí se encontraran con la parte más difícil de esta etapa,
-- que es la detección de bucles y correcta inserción de tipos
-- en el entorno.
-- + Para realizar correctamente la detección de cíclos se utiliza el algoritmo
--   de sort topologico. Pueden encontrar una simple implementación en el
--   archivo [TigerTopSort](src/TigerTopSort.hs).
-- + Para generar los representantes correspondientes de tipo |Tipo|, vamos a
--   necesitar generar valores potencialmente infinitos y para esto usaremos una
--   técnica conocida en la literatura de Haskell conocida como [Tying the
--   Knot](https://wiki.haskell.org/Tying_the_Knot)
----------------------------------------
-- ** transDecs :: (MemM w, Manticore w) => [Dec] -> w a -> w a
transDecs :: Manticore w => [Dec] -> w a -> w a
--
----------------------------------------
-- Caso base.
transDecs [] m                               = m
----------------------------------------
-- Aquí veremos brillar la abstracción que tomamos en |insertValV|
transDecs ((VarDec nm escap t init p): xs) m = do
  ((), tipo_init) <- transExp init
  -- Si tipo_init es TUnit deberíamos fallar: No se admiten procedimientos en
  -- las declaraciones.
  when (equivTipo tipo_init TUnit)
       (flip addpos p $ derror (pack ("No se admiten procedimientos en: " ++ show nm)))
  case t of
    Just ty_t -> do
      tipo_t <- getTipoT ty_t
      -- TODO: Revisar bien el caso de los records
      if (equivTipo tipo_init tipo_t)
        then (insertValV nm tipo_init (transDecs xs m))
        else (flip addpos p $ derror (pack ("Tipos distintos en: " ++ show nm)))
    Nothing -> do
      -- Si tipo_init es nil deberíamos fallar: ver página 118 del libro.
      when (equivTipo tipo_init TNil)
           (flip addpos p $ derror (pack ("Debe explicitarse el tipo: " ++ show nm)))
      insertValV nm tipo_init (transDecs xs m)
----------------------------------------
-- Aquí veremos brillar la abstracción que tomamos en |insertFunV| Recuerden
-- viene una lista de declaración de funciones, todas se toman como mutuamente
-- recursivas así que tendrán que hacer un poco más de trabajo.
-- fs ::[(Nombre [(Symbol, Escapa, Ty) -- Tipo escrito], Maybe Symbol -- Tipo de retorno, Exp -- Body, Pos)]
transDecs (FunctionDec fs : xs) m =
  -- TODO: Revisar que no haya nombres repetidos
  -- insertFunV :: Symbol -> FunEntry -> w a -> w a
  -- type FunEntry = (Unique, Label, [Tipo], Tipo, Externa)
  let
    insert_headers [] m = m
    insert_headers ((nm, args, mty, _body, p):fs) m = do
      uniq <- mkUnique
      tipo_args <- mapM (\(_, _, ty) -> transTy ty) args
      case mty of
        Just s -> do
          tipo_s <- getTipoT s
          insertFunV nm (uniq, nm, tipo_args, tipo_s, Propia) (insert_headers fs m)
        Nothing ->
          insertFunV nm (uniq, nm, tipo_args, TUnit, Propia) (insert_headers fs m)

    insert_args [] m = m
    insert_args ((nm, tipo):args) m = do
      insertValV nm tipo (insert_args args m)

    insert_bodies [] m = m
    insert_bodies ((nm, args, _mty, body, p):fs) m = do
      tipo_args <- mapM (\(arg_nm, _, ty) -> transTy ty >>= (\ tipo -> return (arg_nm, tipo))) args
      ((), tipo_body) <- insert_args tipo_args (transExp body)
      -- Chequeamos que tipo_body coincida con el declarado
      (_, _, _, tipo_nm, _) <- getTipoFunV nm
      unless (equivTipo tipo_nm tipo_body) $ errorTiposMsg p "En la declaracion " tipo_nm tipo_body
      insert_bodies fs m

  in insert_headers fs (insert_bodies fs (transDecs xs m))



----------------------------------------
transDecs ((TypeDec xs) : xss)              m =
  let
    -- (0) Nos quedamos con todos los nombres. Nos va venir bien para cuando
    -- tengamos que actualizar a todos los tipos del batch
    -- __en este esqueleto voy a tirar las posiciones__.
    xs' = fmap (\(x,y,_) -> (x,y)) xs
    tyNames =  fst $ unzip xs'
    -- (1)
    (recordsTy, nrTy) = splitWith (\(s , t) -> either (Left . (s,)) (Right . (s,)) (splitRecordTy t)) xs'
    -- (2)
    sortedTys = kahnSort nrTy
  in
    -- (3)
    insertRecordsAsRef recordsTy $
    -- (4)
    insertSortedTys sortedTys $
----------------------------------------
    undefined -- Completar el algoritmo.
----------------------------------------
-- Las declaraciones de tipos al igual que las funciones vendrán en batch de
-- tipos mutuamente recursivos.
-- Para esto pueden seguir el siguiente camino:
-- + 1) Separar la lista xs en dos, por un lado los Records y por otro los tipos
-- NoRecords.
-- + 2) Ordenar usando el sort topologico las definiciones que __no__ son
--   records . Ya que estos __no__ generan conflictos (ver Teoría)
-- + 3) Ya que los records no generan dependencias, pero si podríamos
--   necesitarlos para definir los otros tipos. Los metemos directamente como
--   (rName, rTy) -> insertTipoT rName (TRecordRef rName)
insertRecordsAsRef  :: Manticore w => [(Symbol, Ty)] -> w a -> w a
insertRecordsAsRef [] m = m
insertRecordsAsRef ((rSym, rTy) : rs) m = undefined -- Completar el algoritmo.
-- + 4) Luego siguiendo el resultado del Sort Topológico insertamos el resto de
--   los tipos, para esto van a necesitar una función |transTy :: Ty -> Tipo|
--   que sólo analizará |Ty| que __no__ son records.
insertSortedTys :: Manticore w => [(Symbol, Ty)] -> w a -> w a
insertSortedTys [] m = m
insertSortedTys ((tSym, tTy) : ts) m = undefined -- Completar el algoritmo.
----------------------------------------
-- Pequeño parate acá, en el entorno tenemos los records definidos como
-- referencias a sí mismo y a los demás tipos con sus estructuras pero con
-- posibles |TRefRecords| que __tenemos que sacar__.

-- Lo que nos falta hacer es darle estructura a los Records (algo que todavía no
-- hicimos), pero claramente para esto vamos a tener que resolver posibles
-- dependencias mutuas que podrían llegar a aparecer (algo que vamos a tener que
-- evitar).

-- Pensar en cómo deberíamos terminar con las siguientes definiciones:

-- ```Tiger
-- type List = Record {hd : int, tl : List}
-- ```
-- En Haskell debemos tener un poco más de cuidado.
-- Para List vamos a generar el siguiente código:
--   ```Haskell
--   insertTValV "List" t
--     where t = TRecord [ ("hd", TInt RW) , ("tl", t) ]
--   ```
-- Notar que el en el |where| el |t| se usa para continuar definiendose a sí
-- mismo, pero Haskell al ser lazy anda todo bien.

-- ```Tiger
-- type A = Record {a : B , b : C}
-- type B = Record {b : B}
-- type C = Record {b : B , a : A}
-- ```
-- Pensar cómo deberían quedar estos...
----------------------------------------
-- + Para analizar y construir los records, lo vamos a hacer de a poco.
-- Volveremos a utilizar la lista de los records, construiremos parcialmente el
-- cuerpo de este record y propagar el cambio a __todos__ los tipos del batch.
-- Por ejemplo, si tenemos 5 definiciones de tipos, de las cuales 2 son records.
-- Tendremos dos listas una |rs| con la información de los records, de longitud
-- 2, y una lista |ts| con los nombres e información de cada uno de los tipos
-- que __no__ son records. Para cada uno de los records (son dos) vamos a
-- actualizar todos los tipos que son (4).
----------------------------------------
--
-- Para esto vamos a necesitar principalmente la ayuda de una función
--  |autoRef :: Symbol -> Tipo -> Tipo -> Tipo|
--   que toma un |s :: Symbol|, |t : Tipo|, |r : Tipo| y retorna un |r' : Tipo|,
--   lo que hace es recorrer a |r| y cada vez que encuentre una referencia de
--   record a |s| ponga en su lugar a |t|. Esta la vamos a usar para generar
--   correctamente el tipo recursivo de los records recursivos. Básicamente es
--   lo que va a generar el |t| del ejemplo de |List| (ver más arriba).
autoRef :: Symbol -> Tipo -> Tipo -> Tipo
autoRef s t t'@(RefRecord s') | s == s' = t | otherwise = t'
autoRef _ _ t = t
----------------------------------------
-- Siguiendo el ejemplo de |List| tendríamos un |insertValV "List" t| donde
-- ahora |t| no tiene mención a si mismo, pero podría tener otras referencias.
-- Al propagar esta información al resto de los tipos eliminamos las posibles
-- referencias al record que acabamos de definir.
--
-- Es decir, tenemos un nombre menos!!! Si hacemos esto con todos los records
-- estamos listos!!!
----------------------------------------
-- Para propagar la info, podemos tener una funcion auxiliar que sea
-- |updateT :: Manticore w => Symbol -> Tipo -> Symbol -> w a -> w a| que para
--  dados |(s : Symbol)|, |(t : Tipo)|, y un simbolo |w|, busque en la tabla el
--  tipo que tiene asignado |w| y lko recorra buscando referencias al record |s|
--  y si la encuentra la reemplaze por el tipo |t|. (ver |autoRef|).
----------------------------------------
updateRefs :: Manticore w => Symbol -> Tipo -> Symbol -> w a -> w a
updateRefs  s t s' m = do
  -- | Buscamos el tipo de s'
  t'  <- getTipoT s'
  -- | Insertamos el nuevo tipo eliminando si la tiene, las referencias a |s|
  insertTipoT s' (autoRef s t t') m

-- ** transExp :: (MemM w, Manticore w) => Exp -> w (BExp , Tipo)
transExp :: (Manticore w) => Exp -> w (() , Tipo)
transExp (VarExp v p) = addpos (transVar v) p
transExp UnitExp{} = return ((), TUnit) -- ** fmap (,TUnit) unitExp
transExp NilExp{} = return ((), TNil) -- ** fmap (,TNil) nilExp
transExp (IntExp i _) = return ((), TInt RW) -- ** fmap (,TInt RW) (intExp i)
transExp (StringExp s _) = return (() , TString) -- ** fmap (,TString) (stringExp (pack s))
transExp (CallExp nm args p) = do
  (_, _, tipos_params, tipo_nm, _) <- getTipoFunV nm
  tipos_args <- mapM transExp args

  -- Comparamos que los tipos declarados coincidan con el tipo de los argumentos recibidos.
  -- Nos inventamos unas tuplas para usar la función dada 'cmpZip'
  cmpZip
    (P.map (\tipo -> (TigerSymbol.empty, tipo)) tipos_params)
    (P.map (\ ((), tipo) -> (TigerSymbol.empty, tipo, 0)) tipos_args)

  -- Si llegamos a este punto, no hay errores de tipo, entonces devolvemos 'tipo_nm'
  return ((), tipo_nm)


transExp (OpExp el' oper er' p) = do -- Esta va /gratis/
        (_ , el) <- transExp el'
        (_ , er) <- transExp er'
        case oper of
          EqOp -> if tiposComparables el er EqOp then oOps el er
                  else addpos (derror (pack "Error de Tipos. Tipos no comparables")) p
          NeqOp ->if tiposComparables el er EqOp then oOps el er
                  else addpos (derror (pack "Error de Tipos. Tipos no comparables")) p
          -- Los unifico en esta etapa porque solo chequeamos los tipos, en la próxima
          -- tendrán que hacer algo más interesante.
          PlusOp -> oOps el er
          MinusOp -> oOps el er
          TimesOp -> oOps el er
          DivideOp -> oOps el er
          LtOp -> oOps el er
          LeOp -> oOps el er
          GtOp -> oOps el er
          GeOp -> oOps el er
          where oOps l r = if equivTipo l r -- Chequeamos que son el mismo tipo
                              && equivTipo l (TInt RO) -- y que además es Entero. [Equiv Tipo es una rel de equiv]
                           then return ((), TInt RO)
                           else addpos (derror (pack "Error en el chequeo de una comparación.")) p
-- | Recordemos que 'RecordExp :: [(Symbol, Exp)] -> Symbol -> Pos -> Exp'
-- Donde el primer argumento son los campos del records, y el segundo es
-- el texto plano de un tipo (que ya debería estar definido). Una expresión
-- de este tipo está creando un nuevo record.
transExp(RecordExp flds rt p) =
  addpos (getTipoT rt) p >>= \case -- Buscamos en la tabla que tipo es 'rt', y hacemos un análisis por casos.
    trec@(TRecord fldsTy _) -> -- ':: TRecord [(Symbol, Tipo, Int)] Unique'
      do
        -- Especial atención acá.
        -- Tenemos una lista de expresiones con efectos
        -- y estos efectos tiene producirse en orden! 'mapM' viene a mano.
        fldsTys <- mapM (\(nm, cod) -> (nm,) <$> transExp cod) flds -- Buscamos los tipos de cada una de los campos.
        -- como resultado tenemos 'fldsTys :: (Symbol, ( CIr , Tipo))'
        -- Lo que resta es chequear que los tipos  sean los mismos, entre los que el programador dio
        -- y los que tienen que ser según la definición del record.
        let ordered = List.sortBy (Ord.comparing fst) fldsTys
        -- asumiendo que no nos interesan como el usuario ingresa los campos los ordenamos.
        _ <- flip addpos p $ cmpZip ( (\(s,(c,t)) -> (s,t)) <$> ordered) fldsTy -- Demon corta la ejecución.
        return ((), trec) -- Si todo fue bien devolvemos trec.
    _ -> flip addpos p $ derror (pack "Error de tipos.")
transExp(SeqExp es p) = fmap last (mapM transExp es)
  -- last <$> mapM transExp es
-- ^ Notar que esto queda así porque no nos interesan los
-- units intermedios. Eventualmente vamos a coleccionar los códigos intermedios y se verá algo similar a:
-- do
--       es' <- mapM transExp es
--       return ( () , snd $ last es')
transExp(AssignExp var val p) = do
  (_, tipo_var) <- transVar var
  -- Primero, revisamos que la variable no sea de sólo lectura
  -- TODO: Revisar. Creo que equivTipo no distingue entre RO y RW
  when (equivTipo tipo_var (TInt RO)) $ addpos (derror (pack "Variable de solo lectura")) p
  (_, tipo_val) <- transExp val
  -- Y después, nos fijamos que el tipo declarado para la variable 'var' coincida con el valor
  -- de la expresión 'val'
  C.unlessM (tiposIguales tipo_var tipo_val) $ errorTiposMsg p "En la asignación ->" tipo_var tipo_val
  -- Si son iguales devolvemos cualquiera de los dos
  return ((), tipo_val)
transExp(IfExp co th Nothing p) = do
        -- ** (ccond , co') <- transExp co
  -- Analizamos el tipo de la condición
        (_ , co') <- transExp co
  -- chequeamos que sea un entero.
        unless (equivTipo co' TBool) $ errorTiposMsg p "En la condición del if->" co' TBool -- Claramente acá se puede dar un mejor error.
        -- ** (cth , th') <- transExp th
  -- Analizamos el tipo del branch.
        (() , th') <- transExp th
  -- chequeamos que sea de tipo Unit.
        unless (equivTipo th' TUnit) $ errorTiposMsg p "En el branch del if->" th' TUnit
  -- Si todo fue bien, devolvemos que el tipo de todo el 'if' es de tipo Unit.
        return (() , TUnit)
transExp(IfExp co th (Just el) p) = do
  (_ , condType) <- transExp co
  unless (equivTipo condType TBool) $ errorTiposMsg p "En la condición del if ->" condType TBool
  (_, ttType) <- transExp th
  (_, ffType) <- transExp el
  C.unlessM (tiposIguales ttType ffType) $ errorTiposMsg p "En los branches." ttType ffType
  -- Si todo fue bien devolvemos el tipo de una de las branches.
  return ((), ttType)
transExp(WhileExp co body p) = do
  (_ , coTy) <- transExp co
  unless (equivTipo coTy TBool) $ errorTiposMsg p "Error en la condición del While" coTy TBool
  (_ , boTy) <- transExp body
  unless (equivTipo boTy TUnit) $ errorTiposMsg p "Error en el cuerpo del While" boTy TUnit
  return ((), TUnit)
transExp(ForExp nv mb lo hi bo p) = do
  -- nv es el nombre de la variable que declara el for, mb si escapa o no
  -- lo y hi son lower y upper bound, bo es el body y p es la posición

  -- Chequeamos que el límite inferior de la variable sea una expresión de tipo entero
  (_, tipo_lo) <- transExp lo
  unless (equivTipo tipo_lo (TInt RW)) $ errorTiposMsg p "Error en la expresión 'lo' del For" tipo_lo (TInt RW)
  -- Ahora lo mismo por con límite superior
  (_, tipo_hi) <- transExp hi
  unless (equivTipo tipo_hi (TInt RW)) $ errorTiposMsg p "Error en la expresión 'hi' del For" tipo_hi (TInt RW)
  -- Acá deberíamos chequear que lo < hi. Pero para eso necesitamos el código intermedio.
  -- TODO: En la próxima etapa ^. (Ver Tiger Language Reference Manual)
  -- Chequeamos que el cuerpo del for no produzca valor. (Ver Tiger Language Reference Manual)
  (_ , tipo_bo) <- insertVRO nv $ transExp bo
  unless (equivTipo tipo_bo TUnit) $ errorTiposMsg p "Error en el cuerpo del For" tipo_bo TUnit
  -- Si llegamos hasta acá está todo bien. Como el for no produce valores, devolvemos TUnit.
  return ((), TUnit)

transExp(LetExp dcs body p) = transDecs dcs (transExp body)
transExp(BreakExp p) = return ((), TUnit)
transExp(ArrayExp sn cant init p) = do
  tipo_sn <- getTipoT sn
  -- Primero, miramos que sn sea efectivamente un arreglo
  case tipo_sn of
    TArray tipo_elem _ -> do
      (_, tipo_cant) <- transExp cant
      -- Después me fijo que el valor ingresado para indicar la longitud sea de tipo
      -- entero. Comparo con 'RO' porque \equivTipo\ no distingue entre RO y RW.
      unless (equivTipo tipo_cant (TInt RO))
             (errorTiposMsg p "En la longitud del array" tipo_cant (TInt RO))
      (_, tipo_init) <- transExp init
      -- Por último nos fijamos que el tipo de los elementos de arrego y el de la
      -- expresión inicial coincidan
      C.unlessM (tiposIguales tipo_elem tipo_init) $ errorTiposMsg p "En el array" tipo_elem tipo_init
      -- Si llegamo' a esta punto este punto está todo bien
      return ((), tipo_elem)
    _ -> flip addpos p $ derror (pack "Error de tipos.")



-- Un ejemplo de estado que alcanzaría para realizar todas la funciones es:
data Estado = Est {vEnv :: M.Map Symbol EnvEntry, tEnv :: M.Map Symbol Tipo}
    deriving Show
-- data EstadoG = G {vEnv :: [M.Map Symbol EnvEntry], tEnv :: [M.Map Symbol Tipo]}
--     deriving Show
--
-- Estado Inicial con los entornos
-- * int y string como tipos básicos. -> tEnv
-- * todas las funciones del *runtime* disponibles. -> vEnv
initConf :: Estado
initConf = Est
           { tEnv = M.insert (pack "int") (TInt RW) (M.singleton (pack "string") TString)
           , vEnv = M.fromList
                    [(pack "print", Func (1,pack "print",[TString], TUnit, Runtime))
                    ,(pack "flush", Func (1,pack "flush",[],TUnit, Runtime))
                    ,(pack "getchar",Func (1,pack "getchar",[],TString,Runtime))
                    ,(pack "ord",Func (1,pack "ord",[TString],TInt RW,Runtime))
                    ,(pack "chr",Func (1,pack "chr",[TInt RW],TString,Runtime))
                    ,(pack "size",Func (1,pack "size",[TString],TInt RW,Runtime))
                    ,(pack "substring",Func (1,pack "substring",[TString,TInt RW, TInt RW],TString,Runtime))
                    ,(pack "concat",Func (1,pack "concat",[TString,TString],TString,Runtime))
                    ,(pack "not",Func (1,pack "not",[TBool],TBool,Runtime))
                    ,(pack "exit",Func (1,pack "exit",[TInt RW],TUnit,Runtime))
                    ]
           }

-- Utilizando alguna especie de run de la monada definida, obtenemos algo así
type Monada = ExceptT Symbol (StateT Estado StGen)
  -- StateT Estado (ExceptT Symbol StGen)

instance Demon Monada where
  -- | 'throwE' de la mónada de excepciones.
  derror =  throwE
  -- TODO: Parte del estudiante
  -- adder :: w a -> Symbol -> w a
instance Manticore Monada where
  -- | A modo de ejemplo esta es una opción de ejemplo de 'insertValV :: Symbol -> ValEntry -> w a -> w'
    insertValV sym ventry m = do
      -- | Guardamos el estado actual
      oldEst <- get
      -- | Insertamos la variable al entorno (sobrescribiéndolo)
      put (oldEst{ vEnv = M.insert sym (Var ventry) (vEnv oldEst) })
      -- | ejecutamos la computación que tomamos como argumentos una vez que expandimos el entorno
      a <- m
      -- | Volvemos a poner el entorno viejo
      put oldEst
      -- | retornamos el valor que resultó de ejecutar la monada en el entorno expandido.
      return a
  -- TODO: Parte del estudiante

runMonada :: Monada ((), Tipo)-> StGen (Either Symbol ((), Tipo))
runMonada =  flip evalStateT initConf . runExceptT

runSeman :: Exp -> StGen (Either Symbol ((), Tipo))
runSeman = runMonada . transExp
