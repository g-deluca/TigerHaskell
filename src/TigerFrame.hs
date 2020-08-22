module TigerFrame where

import           TigerTemp
import           TigerTree

import           TigerAbs                       ( Escapa(..) )

import           TigerSymbol

import           Prelude                 hiding ( exp )

import           Assem                   

-- There are sixteen 64-bit registers in x86-64: ​
-- %rax​, ​%rbx​, ​%rcx​, ​%rdx​, ​%rdi​, ​%rsi​, ​%rbp​, %rsp​, and ​%r8-r15​.
-- Of these, ​%rax​, ​%rcx​, ​%rdx​, ​%rdi​, ​%rsi​, ​%rsp​, and ​%r8-r11​ are considered caller-save
-- registers, meaning that they are not necessarily saved across function calls.
-- By convention, ​%rax​ is used to store a function’s return value,
-- if it exists and is no more than 64 bits long. (Larger return types like structs are returned using the stack.)
-- Registers ​%rbx​, %rbp​, and ​%r12-r15​ are callee-save registers,
-- meaning that they are saved across function calls. Register ​​%rsp​ is used as the​ stack pointer,​ a pointer
-- to the top most element in the stack.
-- Additionally, ​%rdi​,​ %rsi​, ​%rdx​, ​%rcx​, ​%r8​, and ​%r9​ are used to pass the first six integer or
-- pointer parameters to called functions. Additional parameters (or large parameters such as structs passed by value)
-- are passed on the stack.

-- | Caller-save registers

eax = pack "%eax"
ecx = pack "%ecx"
edx = pack "%edx"

-- | Callee-save registers
edi = pack "%edi"
esi = pack "%esi"
esp = pack "%esp"
ebx = pack "%ebx"
ebp = pack "%ebp"

rv = eax
sp = esp
fp = ebp
-- -- | Registros muy usados.
-- rv, fp, sp, bp :: Temp

-- -- | Return value
-- -- | Es 'eax', ver calling convention. Lo dejo por si lo usamos sin querer.
-- rv = pack "eax"
-- --------------

-- -- | Stack pointer
-- sp = pack "esp"
-- -- | Base (frame) pointer
-- bp = pack "ebp"
-- fp = pack "ebp"

-- -- | Registros de x86
-- eax, ebx, ecx, edx, esi, edi :: Temp
-- eax = pack "eax"
-- ebx = pack "ebx"
-- ecx = pack "ecx"
-- edx = pack "edx"
-- esi = pack "esi"
-- edi = pack "edi"

-- | Word size in bytes
-- “quadword” refers to an eight-byte value (suffix​ ​q​).
wSz :: Int
wSz = 4

-- | Base two logarithm of word size in bytes
log2WSz :: Int
log2WSz = 2

-- Estos offsets se utilizan para el calculo de acceso de variables que escapan
-- (principalmente)
-- | Offset
fpPrev :: Int
fpPrev = 0
-- | Donde se encuentra el FP del nivel anterior (no necesariamente el llamante?)
fpPrevLev :: Int
fpPrevLev = 0

-- | Esto es un offset previo a al lugar donde se encuentra el lugar de las variables
-- o de los argumentos.
argsGap, localsGap :: Int
argsGap = wSz
localsGap = 4

-- | Dan inicio a los contadores de argumentos, variables y registros usados.
-- Ver |defaultFrame|
argsInicial, regInicial, localsInicial :: Int
argsInicial = 0
regInicial = 1
localsInicial = 0

-- | Listas de registros que define la llamada y registros especiales
specialregs :: [Temp]
-- Los registros que una llamada pisa, deberian ser destino de un call
calldefs = [rv]
specialregs = [sp, fp]

argregs, calleesaves, callersaves :: [Temp]
argregs = error "Intentamos acceder a argregs"
-- Preservados por la subrutina que se llama
calleesaves = [edi, esi, esp, ebx, ebp]
-- Preservados por el que llama a la subrutina
callersaves = [eax, ecx, edx]

allRegs = calleesaves ++ callersaves

-- | Tipo de dato que define el acceso a variables.
data Access =
  -- | En memoria, acompañada de una dirección
  InFrame Int
  -- | En un registro
  | InReg Temp
    deriving Show

-- | Definición de fragmento usado en en la traducción.
-- Son los bloques que van al assembler de formal individual.
data Frag =
  -- | Es un procedimiento (recordar que ahora todo es un procedimiento)
  -- ya que el resultado viene como un efecto lateral en el |rv|
  Proc Stm Frame
  -- | Es una cadena de caracteres literal, en general esto va en el segmento de datos del assembler.
  | AString Label [Symbol]

-- | Función que nos permite separar los procedimientos y las cadenas de caracteres.
sepFrag :: [Frag] -> ([Frag], [(Stm, Frame)])
sepFrag xs = (reverse ass, reverse stmss)
 where
  (ass, stmss) = foldl
    (\(lbls, stms) x -> case x of
      Proc st fr -> (lbls, (st, fr) : stms)
      AString{}  -> (x : lbls, stms)
    )
    ([], [])
    xs

instance Show Frag where
    show (Proc s f) = "Frame:" ++ show f ++ '\n': show s
    show (AString l ts) = show l ++ ":\n" ++ (foldr (\t ts -> ("\n\t" ++ unpack t) ++ ts) "" ts)

-- | |Frame| es lo que representa el marco de activación dinámico, es la
-- información que vamos a utilizar eventualmente para construir el marco de
-- activación real al momento de efectuar las llamadas a funciones. Que consiste en:
data Frame = Frame {
        -- | Nombre que lleva en el assembler.
        name        :: Symbol,
        -- | Argumentos, si escapan o no.
        formals     :: [Escapa],
        -- | Variables Locales , si escapan o no.
        locals      :: [Escapa],
        -- | Contadores de cantidad de argumentos, variables y registros.
        actualArg   :: Int,
        actualStackArg :: Int,
        actualLocal :: Int,
        actualReg   :: Int
    }
    deriving Show
-- Nota: claramente pueden no llevar contadores y calcularlos en base a la longitud de
-- las listas |formals| y |locals|.

defaultFrame :: Frame
defaultFrame = Frame
  { name        = empty
  , formals     = []
  , locals      = []
  , actualArg   = argsInicial
  , actualStackArg = argsInicial
  , actualLocal = localsInicial
  , actualReg   = regInicial
  }

--------------------------------------------------------------------------------
--
--------------------------------------------------------------------------------

-- Todos a stack por i386
prepFormals :: Frame -> [Access]
prepFormals fs = reverse $ snd
  (foldl (\(n, rs) _ -> (n + argsGap, InFrame n : rs))
         (argsInicial, [])
         (formals fs)
  )

newFrame :: Symbol -> [Escapa] -> Frame
newFrame nm fs = defaultFrame { name = nm, formals = fs }

-- | Función auxiliar que hace una llamada externa.
externalCall :: String -> [Exp] -> Exp
externalCall s = Call (Name $ pack s)

-- | A medida que vamos procesando los argumentos vamos pidiendo 'memoria' para ellos.
-- Dependiendo de la arquitectura algunos pueden ir por memoria o por stack. Salvo obviamente
-- que escapen, en ese caso tienen que ir a memoria.
allocArg :: (Monad w, TLGenerator w) => Frame -> Escapa -> w (Frame, Access)
allocArg fr escapa = do
  let actual = actualArg fr
      acc    = InFrame $ actual * wSz + argsGap
  return (fr { formals = (formals fr) ++ [escapa], actualStackArg = (actualStackArg fr) + 1,
                actualArg = actual + 1 }, acc)
-- allocArg fr NoEscapa =
--   if actualArg fr < 6
--   then 
--   -- Aún tengo registros disponibles para argumentos
--     let actual = actualArg fr
--         argReg = argregs !! actual -- TODO: argregs deberia estar en orden. como el index arranca de 0, esto elige el proximo bien.
--     in return (fr { formals = (formals fr) ++ [NoEscapa], actualArg = actual + 1}, InReg argReg)
--   else
--   -- Ya no tengo registros lo mando a stack
--     let actual = actualArg fr
--         acc    = InFrame $ actual + wSz + argsGap
--     in return (fr { formals = (formals fr) ++ [Escapa], actualArg = actual + 1,
--                     actualStackArg = (actualStackArg fr) + 1}, acc)

allocLocal :: (Monad w, TLGenerator w) => Frame -> Escapa -> w (Frame, Access)
allocLocal fr Escapa =
  let actual = actualLocal fr
      acc    = InFrame $ -(actual * wSz + localsGap) --TODO: revisar func exp. El stack crece abajo??
  in  return (fr { locals = (locals fr) ++ [Escapa], actualLocal = actual + 1 }, acc)
allocLocal fr NoEscapa = do
  s <- newTemp
  return (fr {locals = (locals fr) ++ [NoEscapa]}, InReg s)

-- Función auxiliar par el calculo de acceso a una variable, siguiendo el Static Link.
-- Revisar bien antes de usarla, pero ajustando correctamente la variable |fpPrevLev|
-- debería estar relativamente cerca de la solución
auxexp :: Int -> Exp
auxexp 0 = Temp fp
auxexp n = Mem (Binop Plus (auxexp (n - 1)) (Const fpPrevLev))

exp
  ::
  -- Acceso de la variable.
     Access
    -- Diferencia entre el nivel que se usa y donde se definió.
  -> Int
  -> Exp
exp (InFrame k) e = Mem (Binop Plus (auxexp e) (Const k))
  -- Si esta en un registro, directamente damos ese acceso. Por definición el
  -- nivel tendría que ser el mismo, sino hay un error en el calculo de escapes.
exp (InReg l) c | c /= 0    = error "Megaerror en el calculo de escapes?"
                | otherwise = Temp l

procEntryExit2 :: Frame -> [Instr] -> [Instr]
procEntryExit2 _fr instr =
  instr ++ [Oper {oassem = " ", osrc = specialregs ++ calleesaves, odst = [], ojump = Nothing}]

-- Esta cosa es el Pre y Post de una llamada a funcion (callee)
procEntryExit3 :: ([Instr], Frame) -> [Instr]
procEntryExit3 (body, frame) =
    let frameOffset = wSz * ((actualLocal frame) + (actualStackArg frame))
    in
    [   Oper {oassem = ".globl " ++ (unpack $ name frame) ++ "\n", osrc = [], odst = [], ojump=Nothing},
        Oper {oassem = ".type " ++ (unpack $ name frame) ++ ", @function" ++ "\n", osrc = [], odst = [], ojump=Nothing},
        Assem.Label {lassem = (unpack $ name frame)++ ":\n", llab=name frame },
        Oper {oassem = "push s0\n", osrc=[fp], odst=[sp],ojump=Nothing }, -- pusheo el frame-pointer
        Assem.Move {massem = "mov s0, d0\n", mdst=fp, msrc=sp} -- el frame-pointer apunta a donde está el stack ahora
    ]
    ++ pushList calleesaves ++
    [
        Oper {oassem = "sub $" ++ show frameOffset ++ ", d0\n", osrc = [], odst = [sp], ojump = Nothing}
    ]
    ++ body ++
    [
        Oper {oassem = "add $" ++ show frameOffset ++ ", d0\n", osrc = [], odst = [sp], ojump = Nothing}
    ]
    ++ popList ((reverse calleesaves) ++ [fp]) ++
    [
        Oper {oassem = "ret\n", osrc = [eax], odst = [], ojump = Nothing}
    ]

pushList :: [Temp] -> [Instr]
pushList [] = []
pushList (reg:regs) = (Oper {oassem = "push s0\n", osrc=[reg], odst=[sp], ojump=Nothing}): pushList regs

popList :: [Temp] -> [Instr]
popList [] = []
popList (reg:regs) = (Oper {oassem = "pop s0\n", osrc=[reg], odst=[sp], ojump=Nothing}): popList regs

-- https://stackoverflow.com/questions/24549912/where-and-why-is-the-x64-frame-pointer-supposed-to-point-windows-x64-abi
