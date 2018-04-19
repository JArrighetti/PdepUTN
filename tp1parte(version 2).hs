{-# LANGUAGE NoMonomorphismRestriction #-}
import Text.Show.Functions
import Data.List
import Data.Maybe
import Test.Hspec

data Usuario = Usuario {
 nombre :: String,
 billetera :: Float
} deriving (Show)

--Usuarios de prueba
pepe = Usuario "Jose" 10
lucho = Usuario "Luciano" 2
pepe2 = Usuario "Jose" 50

billeteraDePrueba = 10

ejecutarTests = hspec $ do
	describe "Tests Eventos:" $ do
	it "1 - Depositar 10 monedas en una billetera de 10 monedas. Billetera inicial: 10, Billetera final: 20" (deposito 10 billeteraDePrueba `shouldBe` 20)
	it "2 - Extraer 3 monedas de una billetera de 10 monedas. Billetera inicial: 10, Billetera final: 7" (extraccion 3 billeteraDePrueba `shouldBe` 7)
	it "3 - Extraer 15 monedas de una billetera de 10 monedas. Billetera inicial: 10, Billetera final: 0" (extraccion 15 billeteraDePrueba `shouldBe` 0)
	it "4 - Upgrade en una billetera de 10 monedas. Billetera inicial: 10, Billetera final: 12" (upgrade billeteraDePrueba `shouldBe` 12)
	it "5 - Cerar la cuenta en una billetera de 10 monedas. Billetera inicial: 10, Billetera final: 0" (cierreDeCuenta billeteraDePrueba `shouldBe` 0)
	it "6 - Evento 'queda igual' en una billetera de 10 monedas. Billetera inicial: 10, Billetera final: 10" (quedaIgual billeteraDePrueba `shouldBe` 10)
	it "7 - Depositar 1000 monedas y luego hacer un upgrade en una billetera de 10 monedas. Billetera inicial: 10, Billetera final: 1020" ((upgrade.deposito 1000) billeteraDePrueba `shouldBe` 1020) -- ARREGLAR PARENTESIS 
	describe "Tests Usuarios:" $ do
	it "8 - Billetera de Pepe: 10" (billetera pepe `shouldBe` 10)
	it "9 - Evento 'cierre de cuenta' en la billetera de pepe de 10 monedas. Billetera final: 0" ((cierreDeCuenta.billetera) pepe  `shouldBe` 0)
	it "10 - Se depositan 15 monedas, extraen 2 y tiene un upgrade la billetera de Pepe de 10 monedas. Billetera inicial: 10, Billetera final: 27.6" ((upgrade.extraccion 2.deposito 15.billetera) pepe `shouldBe` 27.6)  -- ARREGLAR PARENTESIS
	describe "Tests Transacciones:" $ do
	transaccion1EnPepe
	transaccion2EnPepe
	transaccion2EnPepe2
	describe "Tests Nuevos Eventos:" $ do
	transaccion3EnLucho
	transaccion4EnLucho
	describe "Tests Pago entre usuarios:" $ do
	transaccion5EnPepe
	transaccion5EnLucho

comoMinimo0 numero = max numero 0
comoMaximo10 numero = min numero 10

type Evento = Float->Float

deposito :: Float->Evento
extraccion :: Float->Evento
upgrade :: Evento
cierreDeCuenta :: Evento
quedaIgual :: Evento

deposito montoADepositar billetera = billetera + montoADepositar
extraccion montoAExtraer billetera = comoMinimo0 (billetera-montoAExtraer)
upgrade billetera = ((+) billetera.comoMaximo10.(*) 0.2) billetera -- USAR COMPOSICION 
cierreDeCuenta billetera = 0
quedaIgual =  id  -- USAR ID


-------------------------Transacciones--------------------------------

compararDosNombres nombre1 nombre2 = nombre1 == nombre2

transaccion :: Evento-> Usuario-> Usuario-> Evento

transaccion evento usuarioAlQueSeLeDebeAplicarLaTransaccion usuarioAlQueSeLeIntentaAplicarLaTransaccion
  |  compararDosNombres (nombre usuarioAlQueSeLeDebeAplicarLaTransaccion) (nombre usuarioAlQueSeLeIntentaAplicarLaTransaccion) = evento --DELEGAR
  |  otherwise = quedaIgual

--Tests Transacciones
transaccion1 = transaccion cierreDeCuenta lucho
transaccion2 = transaccion (deposito 5) pepe

ejecutarTestTransacciones = hspec $ do
	transaccion1EnPepe
	transaccion2EnPepe
	transaccion2EnPepe2

transaccion1EnPepe = it "11 - Transaccion: 'Lucho cierra la cuenta' aplicada en Pepe. Produce el evento 'Queda igual', que cuando se aplica a una billetera de 10 monedas, esta termina con el mismo monto. Billetera inicial: 10, Billetera final: 10" ((transaccion1 pepe.billetera) pepe  `shouldBe` 10) -- COMPARAR CON FLOAT Y COMPONER
transaccion2EnPepe = it "12 - Transaccion 'Pepe deposita 5 monedas' en Pepe. Produce el evento 'deposito de 5 monedas', que aplicado a una billetera de 10 monedas quedaria: Billetera inicial: 10, Billetera final: 15" ((transaccion2 pepe.billetera) pepe `shouldBe` 15)
transaccion2EnPepe2 = it "13 - Transaccion 'Pepe deposita 5 monedas' en Pepe2. Produce el evento 'deposito de 5 monedas', que aplicado a una billetera de 50 monedas quedaria: Billetera inicial: 50, Billetera final: 55" ((transaccion2 pepe2.billetera) pepe2 `shouldBe` 55)


--------------------------NuevosEventos------------------------------

tocoMeVoy =  (cierreDeCuenta.upgrade.deposito 15) -- POINT FREE
ahorranteErrante = (deposito 10.upgrade.deposito 8.extraccion 1.deposito 2.deposito 1) -- POINT FREE

--Test Nuevos Eventos

transaccion3 = transaccion tocoMeVoy lucho2
transaccion4 = transaccion ahorranteErrante lucho2
lucho2 = Usuario "Luciano" 10 --creo otro porque se pide probarlo en una billetera de 10

ejecutarTestNuevosEventos = hspec $ do
	transaccion3EnLucho
	transaccion4EnLucho


transaccion3EnLucho = it "14 - Transaccion: 'Lucho toca y se va' en Lucho. Produce el evento 'Cierre de cuenta' que aplicado a una billetera de 10 monedas quedaria: Billetera inicial: 10, Billetera final: 0" ((transaccion3 lucho2.billetera) lucho2 `shouldBe` 0)
transaccion4EnLucho = it "15 -  Transaccion: 'Lucho es un ahorrante errante' en Lucho. Produce los eventos 'deposito  de 1 moneda', 'deposito de 2 monedas', 'extraccion de 1 moneda', 'deposito de 8 monedas', 'upgrade' y 'deposito de 10 monedas', que al aplicarlo a una billetera de 10 monedas, quedaria: Billetera inicial: 10 Billetera final: 34" ((transaccion4 lucho2.billetera) lucho2 `shouldBe` 34)

--------------------------Pago entre usuarios------------------------

pagoEntreUsuarios :: Usuario->Usuario->Float->Usuario->Evento

pagoEntreUsuarios usuarioExtraccion usuarioRecibeDeposito montoDeLaTransaccion usuarioAlQueSeLeDebeAplicarLaTransaccion  
	| compararDosNombres (nombre usuarioExtraccion) (nombre usuarioAlQueSeLeDebeAplicarLaTransaccion) =  extraccion montoDeLaTransaccion -- DELEGAR
	| compararDosNombres (nombre usuarioRecibeDeposito) (nombre usuarioAlQueSeLeDebeAplicarLaTransaccion) = deposito montoDeLaTransaccion 
	| otherwise = quedaIgual

transaccion5 :: Usuario->Evento
transaccion5 = pagoEntreUsuarios pepe lucho 7 

ejecutarTestPagoEntreUsuarios = hspec $ do
	transaccion5EnPepe
	transaccion5EnLucho

transaccion5EnPepe = it "16 - Transaccion: 'Pepe le da 7 unidades a Lucho' en Pepe. Produce le evento 'extraccion de 7 unidades' que al aplicarlo a una billetera de 10 monedas, la misma queda con 3 monedas" ((transaccion5 pepe.billetera) pepe `shouldBe` 3) --COMPONER
transaccion5EnLucho = it "17 - Transaccion: 'Pepe le da 7 unidades a Lucho' en Pepe. Produce le evento 'deposito de 7 unidades' que al aplicarlo a una billetera de 10 monedas, la misma queda con 17 monedas" ((transaccion5 lucho2.billetera) lucho2 `shouldBe` 17)




---------------------------------2da Entrega-------------------------------


bloque1 = [transaccion1,transaccion2,transaccion2,transaccion2,transaccion3,transaccion4,transaccion5,transaccion3]
bloque2 = [transaccion2,transaccion2,transaccion2,transaccion2,transaccion2]

aplicarEventoALaBilleteraDeUnUsuario evento usuario = usuario {billetera = (evento.billetera) usuario}


aplicarBloqueDeTransaccionesAUnUsuario [] usuario = usuario
aplicarBloqueDeTransaccionesAUnUsuario (cabeza:cola) usuario = aplicarBloqueDeTransaccionesAUnUsuario cola (aplicarEventoALaBilleteraDeUnUsuario (cabeza usuario) usuario)

-------------------------blockChain---------------------------------

type BlockChain = [[Usuario->Evento]]
blockChain:: BlockChain

blockChain = [bloque2,bloque1,bloque1,bloque1,bloque1,bloque1,bloque1,bloque1,bloque1,bloque1,bloque1]

--punto 26 de blockChain
aplicarBlockChainAUnUsuario [] usuario = usuario
aplicarBlockChainAUnUsuario (cabeza:cola) usuario = aplicarBlockChainAUnUsuario cola (aplicarBloqueDeTransaccionesAUnUsuario cabeza usuario)

--punto 27 de blockChain
usuarioDespuesDeUnBloqueN numeroDeBloque blockChain usuario = aplicarBlockChainAUnUsuario (take numeroDeBloque blockChain) usuario

--punto 28 de blockChain
aplicarBlockChainAUnConjuntoDeUsuario blockChain listaDeUsuarios = map (aplicarBlockChainAUnUsuario blockChain) listaDeUsuarios

------------------------BlockChain infinito-------------------------------

--punto 29 de BlockChain infinito

agregarBloque lista = (lista++lista) : agregarBloque (lista++lista)
crearBlockChainInfinito bloque = bloque : agregarBloque bloque

aplicarBlockChainInfinitoAUnUsuario bloquesAplicados (cabeza:cola) usuario  | (billetera usuario) >= 10000 = bloquesAplicados
																	 		| otherwise = aplicarBlockChainInfinitoAUnUsuario (bloquesAplicados+1) cola (aplicarBloqueDeTransaccionesAUnUsuario cabeza usuario)



enCuantosBloquesUnUsuarioLlegaADiezMilMonedas bloque usuario = aplicarBlockChainInfinitoAUnUsuario 0 (crearBlockChainInfinito bloque) usuario