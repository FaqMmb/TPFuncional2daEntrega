module MicroEntrega2 where
--import Text.Show.Functions
instance Show (a -> b) where
  show f = "<una función>"

-- 3.1. Utilizamos data para el modelado ya que me permite guardar diferentes datos de un microprocesador y poder trabajar libremente con diferentes funciones para los puntos siguientes.

type Modelo = String
type Memoria = Int
type AcumuladorA = Int
type AcumuladorB = Int
type ProgramCounter = Int
type MensajeError = String
type Instruccion = Microprocesador -> Microprocesador


data Microprocesador = Microprocesador {memoria :: [Memoria], acumuladorA :: AcumuladorA, acumuladorB :: AcumuladorB, programCounter :: ProgramCounter, mensajeError :: MensajeError, programa :: [Instruccion]} deriving (Show)

xt8088 = Microprocesador {memoria = memoriaVacia, acumuladorA = 0, acumuladorB = 0, programCounter = 0, mensajeError = [],programa = []}

fp20 = Microprocesador {memoria = [0], acumuladorA = 7, acumuladorB = 24, programCounter = 0, mensajeError = [], programa = []}

at8086 = Microprocesador {memoria = [1..20], acumuladorA = 0, acumuladorB = 0, programCounter = 0, mensajeError = [], programa = []}

microDesorden = Microprocesador {memoria = [2,5,1,0,6,9], acumuladorA = 0, acumuladorB = 0, programCounter = 0, mensajeError = [], programa = []}

microMemoriaInfinita = Microprocesador {memoria = [0..], acumuladorA = 0, acumuladorB = 0, programCounter = 0, mensajeError = [], programa = []} 

memoriaVacia = replicate 1024 0

-- 3.2. Concepto: composicion, porque nos permite aplicar la misma funcion 3 veces

nop :: Microprocesador -> Microprocesador
nop unMicroprocesador = unMicroprocesador {programCounter = programCounter unMicroprocesador + 1}

ejecutarInstruccion :: Microprocesador -> (Microprocesador -> Microprocesador) -> Microprocesador
ejecutarInstruccion micro unaInstruccion = (nop.unaInstruccion) micro

{- PRUEBA EN CONSOLA

*MicroEntrega1> (nop.nop.nop) xt8088
Microprocesador {memoria = memoriaVacia, acumuladorA = 0, acumuladorB = 0, programCounter = 3, mensajeError = ""}

-}

-- 3.3

lodv :: Int -> Microprocesador -> Microprocesador
lodv val = cargarAcumuladorA val


cargarAcumuladorA :: Int -> Microprocesador -> Microprocesador
cargarAcumuladorA val unMicroprocesador =  unMicroprocesador {acumuladorA = val}


swap :: Microprocesador -> Microprocesador
swap = intercambiarValores


intercambiarValores :: Microprocesador -> Microprocesador
intercambiarValores unMicroprocesador = unMicroprocesador {acumuladorA = acumuladorB unMicroprocesador, acumuladorB = acumuladorA unMicroprocesador}


add :: Microprocesador -> Microprocesador
add = igualarAcumuladorBaCero.sumarAcumuladores


sumarAcumuladores :: Microprocesador -> Microprocesador
sumarAcumuladores unMicroprocesador = unMicroprocesador {acumuladorA = acumuladorA unMicroprocesador + acumuladorB unMicroprocesador}


igualarAcumuladorBaCero :: Microprocesador -> Microprocesador
igualarAcumuladorBaCero unMicroprocesador = unMicroprocesador {acumuladorB = 0}

{- PRUEBA EN CONSOLA

*MicroEntrega1> (add.(lodv 22).swap.(lodv 10)) xt8088
Microprocesador {memoria = memoriaVacia, acumuladorA = 32, acumuladorB = 0, programCounter = 4, mensajeError = ""}

-}


-- 3.4

divide :: Microprocesador -> Microprocesador
divide = igualarAcumuladorBaCero.dividirAcumuladores


dividirAcumuladores :: Microprocesador -> Microprocesador
dividirAcumuladores (Microprocesador memoria acumuladorA 0  pc _ programa) = (Microprocesador memoria acumuladorA 0  pc "DIVISION BY ZERO" programa)   
dividirAcumuladores unMicroprocesador = unMicroprocesador {acumuladorA = div (acumuladorA unMicroprocesador) (acumuladorB unMicroprocesador)} 


str :: Int -> Int -> Microprocesador -> Microprocesador
str addr valor = modificarMemoria addr valor


modificarMemoria :: Int -> Int -> Microprocesador -> Microprocesador
modificarMemoria addr valor unMicroprocesador = unMicroprocesador {memoria = (take (addr - 1) (memoria unMicroprocesador)) ++ [valor] ++ (drop (addr) (memoria unMicroprocesador))}


lod :: Int -> Microprocesador -> Microprocesador
lod addr = modificarAcumuladorA addr


modificarAcumuladorA :: Int -> Microprocesador -> Microprocesador
modificarAcumuladorA addr unMicroprocesador = unMicroprocesador { acumuladorA = last (take addr (memoria unMicroprocesador))}


--2da entrega

programa1 = [lodv 10, swap, lodv 22, add]  --suma de 10 y 22 
programa2 = [str 1 2, str 2 0, lod 2, swap, lod 1, divide ] --division de 2 por 0
programa3 = [swap, nop, lodv 133, lodv 0 , str 1 3, str 2 0, lodv 133, str 1 3 ]
programa4 = [swap,nop,lodv 133, lodv 0, str 1 3, str 2 0]

--3.1

cargarPrograma micro unPrograma = micro{programa = unPrograma}
cargarPrograma :: Microprocesador -> [Instruccion] -> Microprocesador

--3.2

ejecutarPrograma :: Microprocesador -> Microprocesador
ejecutarPrograma micro = aplicarInstrucciones (programa micro) micro

aplicarInstrucciones :: [Instruccion] -> Microprocesador -> Microprocesador
aplicarInstrucciones instrucciones micro = foldl ejecutar micro instrucciones

ejecutar :: Microprocesador -> Instruccion -> Microprocesador
ejecutar (Microprocesador memoria acumA acumB pc [] programa) instruccion = ejecutarInstruccion (Microprocesador memoria acumA acumB pc [] programa) instruccion
ejecutar micro _ = micro

--3.3

ifnz :: Microprocesador -> [Instruccion] -> Microprocesador
ifnz (Microprocesador memoria 0 acumuladorB pc flag program) _ = (Microprocesador memoria 0 acumuladorB pc flag program)
ifnz micro instrucciones = aplicarInstrucciones instrucciones micro

--3.4

depurarPrograma :: Microprocesador -> [Instruccion]
depurarPrograma micro = depuracion (programa micro) micro

depuracion :: [Instruccion] -> Microprocesador -> [Instruccion]
depuracion instrucciones micro = filter (esDepurable.(ejecutarInstruccion micro)) instrucciones

--esDepurable :: Microprocesador -> Bool
--esDepurable (Microprocesador memoriaVacia 0 0 _ _ _) = False
--esDepurable micro = True

esDepurable :: Microprocesador -> Bool
esDepurable micro  |  ((memoria micro) == memoriaVacia || (acumuladorA micro) == 0 || (acumuladorB micro) == 0 ) = False
                   |  otherwise = True

--3.5

tieneMemoriaOrdenada :: Microprocesador -> Bool
tieneMemoriaOrdenada micro = esMemoriaOrdenada (memoria micro) 

esMemoriaOrdenada :: [Int] -> Bool
esMemoriaOrdenada [] = True
esMemoriaOrdenada (primerDato:[]) = True
esMemoriaOrdenada (primerDato:segundoDato:datos) = segundoDato >= primerDato && esMemoriaOrdenada (segundoDato:datos)
 
 
{- PRUEBA EN CONSOLA

*MicroEntrega1> (divide.(lod 1).swap.(lod 2).(str 2 0).(str 1 2)) xt8088
Microprocesador {memoria = [2,0,0,..,0], acumuladorA = 2, acumuladorB = 0, programCounter = 6, mensajeError = "DIVISION BY ZERO"}

-}



{- CASOS DE PRUEBA	
 
4.1

*MicroEntrega1> (nop.nop.nop) xt8088
Microprocesador {memoria = memoriaVacia, acumuladorA = 0, acumuladorB = 0, programCounter = 3, mensajeError = ""}

4.2

*MicroEntrega1> lodv 5 xt8088
Microprocesador {memoria = memoriaVacia, acumuladorA = 5, acumuladorB = 0, programCounter = 1, mensajeError = ""}

*MicroEntrega1> swap fp20
Microprocesador {memoria = memoriaVacia, acumuladorA = 24, acumuladorB = 7, programCounter = 1, mensajeError = ""}

*MicroEntrega1> (add.(lodv 22).swap.(lodv 10)) xt8088
Microprocesador {memoria = [], acumuladorA = 32, acumuladorB = 0, programCounter = 4, mensajeError = ""}

4.3

*MicroEntrega1> str 2 5 at8086
Microprocesador {memoria = [1,5,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20], acumuladorA = 0, acumuladorB = 0, programCounter = 1, mensajeError = ""}

*MicroEntrega1> lod 2 xt8088
Microprocesador {memoria = [0,0,..,0], acumuladorA = 0, acumuladorB = 0, programCounter = 1, mensajeError = ""}

*MicroEntrega1> (divide.(lod 1).swap.(lod 2).(str 2 0).(str 1 2)) xt8088
Microprocesador {memoria = [2,0,0,..,0], acumuladorA = 2, acumuladorB = 0, programCounter = 6, mensajeError = "DIVISION BY ZERO"}

*MicroEntrega1> (divide.(lod 1).swap.(lod 2).(str 2 4).(str 1 12)) xt8088
Microprocesador {memoria = [12,4,0,..,0], acumuladorA = 3, acumuladorB = 0, programCounter = 6, mensajeError = ""}

-2da entrega 

4.2 
*MicroEntrega2> ejecutarPrograma (cargarPrograma xt8088 programa1)
Microprocesador {memoria = [0,..], acumuladorA = 32, acumuladorB = 0, programCounter = 4, mensajeError = "", programa = [<una función>,<una función>,<una función>,<una función>]}

*MicroEntrega2> ejecutarPrograma (cargarPrograma xt8088 programa2)
Microprocesador {memoria = [2,0,0,0,0,0,0,0,0,0,0,0,..], acumuladorA = 2, acumuladorB = 0, programCounter = 6, mensajeError = "DIVISION BY ZERO", programa = [<una función>,<una función>,<una función>,<una función>,<una función>,<una función>]}

4.3

*MicroEntrega2> ifnz fp20 [lodv 3, swap]
Microprocesador {memoria = [0], acumuladorA = 24, acumuladorB = 3, programCounter = 2, mensajeError = "", programa = []}

*MicroEntrega2> ifnz xt8088 [lodv 3, swap]
Microprocesador {memoria = [0,..], acumuladorA = 0, acumuladorB = 0, programCounter = 0, mensajeError = "", programa = []}

4.4

4.5

*MicroEntrega2> tieneMemoriaOrdenada at8086
True

*MicroEntrega2> tieneMemoriaOrdenada microDesorden
False

-}