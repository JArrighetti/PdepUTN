{-# LANGUAGE NoMonomorphismRestriction #-}
import Text.Show.Functions
import Data.List
import Data.Maybe
import Test.Hspec

data Usuario = Usuario {
 nombre :: String,
 billetera :: Float
} deriving (Show,Eq)

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
	describe "Tests de bloques" $ do
	testpunto21 
	testpunto22 
	testpunto23 
	testpunto24 
	describe "Tests de BlockChain" $ do
	it "26 - Se le aplica una BlockaChain (compuesta por 1 bloque2 y 10 bloque1) a Pepe, y este termina con una billetera de 115 monedas" (aplicarBlockChainAUnUsuario blockChain pepe `shouldBe` pepe {billetera=115})


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

type Bloque = [Usuario->Evento]

bloque1 :: Bloque
bloque2 :: Bloque
bloque1 = [transaccion1,transaccion2,transaccion2,transaccion2,transaccion3,transaccion4,transaccion5,transaccion3]
bloque2 = [transaccion2,transaccion2,transaccion2,transaccion2,transaccion2]

listaDeUsuarios = [pepe,lucho]

aplicarEventoALaBilleteraDeUnUsuario evento usuario = usuario {billetera = (evento.billetera) usuario}
nuevaBilletera nuevoMonto unUsuario = unUsuario {billetera=nuevoMonto}

------------------------------------Bloques---------------------------------------

--punto 21 de Bloques

aplicarBloqueDeTransaccionesAUnUsuario :: Bloque->Usuario->Usuario
--la lista que recibe ((cabeza:cola)) debe ser un Bloque (lista de transacciones)
aplicarBloqueDeTransaccionesAUnUsuario [] usuario = usuario
aplicarBloqueDeTransaccionesAUnUsuario (cabeza:cola) usuario = aplicarBloqueDeTransaccionesAUnUsuario cola (aplicarEventoALaBilleteraDeUnUsuario (cabeza usuario) usuario)

--punto 22 de bloques
quientienemasdeN numero lista bloqueaaplicar = filter (\usuario-> billetera (aplicarBloqueDeTransaccionesAUnUsuario bloqueaaplicar usuario) > numero)  lista

--punto 23 de bloques
billeteramasrica bloqueaaplicar lista = maximum (map(billetera.aplicarBloqueDeTransaccionesAUnUsuario bloqueaaplicar) lista)

quienesmasrico bloqueaaplicar lista = fromJust (find (\usuario-> billetera (aplicarBloqueDeTransaccionesAUnUsuario bloqueaaplicar usuario) == (billeteramasrica bloqueaaplicar lista))  lista)

--punto 24 de bloques
billeteramenosrica bloqueaaplicar lista = minimum (map(billetera.aplicarBloqueDeTransaccionesAUnUsuario bloqueaaplicar) lista)

quienesmenosrico bloqueaaplicar lista = fromJust (find (\usuario-> billetera (aplicarBloqueDeTransaccionesAUnUsuario bloqueaaplicar usuario) == (billeteramenosrica bloqueaaplicar lista))  lista)

testpunto21 = it "21 - Si le aplico el bloque 1 a pepe este deberia quedar con una billetera de 18" (aplicarBloqueDeTransaccionesAUnUsuario bloque1 pepe `shouldBe` nuevaBilletera 18 pepe)
testpunto22 = it "22 - A partir del bloque 1 y la lista de usuarios pepe y lucho el unico que supera los 10 pesos despues del bloque es pepe" (quientienemasdeN 10 listaDeUsuarios bloque1 `shouldBe` [pepe])
testpunto23 = it "23 - A partir del bloque 1 y la lista de usuarios pepe y lucho el mas rico es pepe" (quienesmasrico bloque1 listaDeUsuarios `shouldBe` pepe)
testpunto24 = it "24 - A partir del bloque 1 y la lista de usuarios pepe y lucho el menos rico es lucho" (quienesmenosrico bloque1 listaDeUsuarios `shouldBe` lucho)
ejecutartestdebloque = hspec $ do
  testpunto21
  testpunto21
  testpunto23
  testpunto24

-------------------------blockChain---------------------------------

type BlockChain = [Bloque]
blockChain:: BlockChain

blockChain = [bloque2,bloque1,bloque1,bloque1,bloque1,bloque1,bloque1,bloque1,bloque1,bloque1,bloque1]

--punto 25 de BlockChain

billeteraDeUnUsuarioDespuesDeUnBloque usuario bloque = (billetera.aplicarBloqueDeTransaccionesAUnUsuario bloque) usuario
listaDeBilleterasDeUnUsuarioDespuesDeUnaBlockChain blockChain usuario = map (billeteraDeUnUsuarioDespuesDeUnBloque usuario) blockChain
compararSiUnUsuarioTerminaConUnSaldoNDespuesDeUnBloque saldo usuario bloque = saldo == (billetera.aplicarBloqueDeTransaccionesAUnUsuario bloque) usuario 

buscarPeorBloqueDeUnUsuarioEnUnaBlockChain :: BlockChain->Usuario->Bloque
buscarPeorBloqueDeUnUsuarioEnUnaBlockChain blockChain usuario = fromJust (find (compararSiUnUsuarioTerminaConUnSaldoNDespuesDeUnBloque ((minimum.listaDeBilleterasDeUnUsuarioDespuesDeUnaBlockChain blockChain) usuario) usuario) blockChain)

--punto 26 de blockChain

aplicarBlockChainAUnUsuario :: BlockChain->Usuario->Usuario
--la lista que recibe ((cabeza:cola)) debe ser una blockChain (lista de bloques)
aplicarBlockChainAUnUsuario [] usuario = usuario
aplicarBlockChainAUnUsuario (cabeza:cola) usuario = aplicarBlockChainAUnUsuario cola (aplicarBloqueDeTransaccionesAUnUsuario cabeza usuario)

--punto 27 de blockChain
usuarioDespuesDeUnBloqueN :: BlockChain->Usuario->Int->Usuario
usuarioDespuesDeUnBloqueN blockChain usuario numeroDeBloque = aplicarBlockChainAUnUsuario (take numeroDeBloque blockChain) usuario

--punto 28 de blockChain
aplicarBlockChainAUnConjuntoDeUsuarios :: BlockChain->[Usuario]->[Usuario]
aplicarBlockChainAUnConjuntoDeUsuarios blockChain listaDeUsuarios = map (aplicarBlockChainAUnUsuario blockChain) listaDeUsuarios

------------------------BlockChain infinito-------------------------------

--punto 29 de BlockChain infinito

agregarBloque lista = (lista++lista) : agregarBloque (lista++lista)
crearBlockChainInfinito bloque = bloque : agregarBloque bloque

--la lista que recibe ((cabeza:cola)) debe ser un BlockChain infinito (lista infinita de bloques)
aplicarBlockChainInfinitoAUnUsuarioHastaLlegarADiezMilMonedas bloquesAplicados (cabeza:cola) usuario  | (billetera usuario) >= 10000 = bloquesAplicados
																	 		| otherwise = aplicarBlockChainInfinitoAUnUsuarioHastaLlegarADiezMilMonedas (bloquesAplicados+1) cola (aplicarBloqueDeTransaccionesAUnUsuario cabeza usuario)

enCuantosBloquesDeUnBlockChainInfinitoUnUsuarioLlegaADiezMilMonedas bloque usuario = aplicarBlockChainInfinitoAUnUsuarioHastaLlegarADiezMilMonedas 0 (crearBlockChainInfinito bloque) usuario

--El concepto clave que se aplico en este punto es el de evaluacion diferida, ya que sin esto no se hubiese podido trabajar con una lista infinita
--porque es imposible evaluar completamente una lista infinita, pero con la evaluacion diferida se hace posible evaluar una parte de esa lista
--infinita, la parte que haga falta evaluar