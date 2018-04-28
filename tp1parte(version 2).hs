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
	it "7 - Depositar 1000 monedas y luego hacer un upgrade en una billetera de 10 monedas. Billetera inicial: 10, Billetera final: 1020" ((upgrade.deposito 1000) billeteraDePrueba `shouldBe` 1020) 
	describe "Tests Usuarios:" $ do
	it "8 - Billetera de Pepe: 10" (billetera pepe `shouldBe` 10)
	it "9 - Evento 'cierre de cuenta' en la billetera de pepe de 10 monedas. Billetera final: 0" ((cierreDeCuenta.billetera) pepe  `shouldBe` 0)
	it "10 - Se depositan 15 monedas, extraen 2 y tiene un upgrade la billetera de Pepe de 10 monedas. Billetera inicial: 10, Billetera final: 27.6" ((upgrade.extraccion 2.deposito 15.billetera) pepe `shouldBe` 27.6)  
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
	describe "\n\nTests de Usuarios luego de una transaccion:" $ do
	it "18 - Si le aplico la transaccion 1 a pepe deberia quedar igual" (aplicarTransaccionAUnUsuario pepe transaccion1 `shouldBe` pepe)
	it "19 - Si le aplico la transaccion 5 a lucho su billetera deberia ser de 9" (aplicarTransaccionAUnUsuario lucho transaccion5 `shouldBe` lucho{billetera= 9})
	it "20 - Si le aplico la transaccion 5 y luego la transaccion2 a pepe deberia quedar con 8 monedas en su billetera" (aplicarTransaccionAUnUsuario (aplicarTransaccionAUnUsuario pepe transaccion2) transaccion5 `shouldBe` pepe{billetera= 8})
	describe "Tests de Bloques:" $ do
	it "21 - Si le aplico el bloque 1 a pepe este deberia quedar con una billetera de 18" (aplicarBloqueDeTransaccionesAUnUsuario pepe bloque1 `shouldBe` pepe{billetera=18} )
	it "22 - A partir del bloque 1 y la lista de usuarios pepe y lucho el unico que supera los 10 pesos despues del bloque es pepe" (quienTieneMasDeN bloque1 listaDeUsuarios 10 `shouldBe` [pepe])
	it "23 - A partir del bloque 1 y la lista de usuarios pepe y lucho el mas rico es pepe" (quienesMasRico bloque1 listaDeUsuarios `shouldBe` pepe)
	it "24 - A partir del bloque 1 y la lista de usuarios pepe y lucho el menos rico es lucho" (quienEsMenosRico bloque1 listaDeUsuarios `shouldBe` lucho)
	describe "Tests de BlockChain:" $ do
	it "25 - El peor bloque que tuvo Pepe en la BlockChain fue el bloque1, ya que si comenzara por ese, su billetera de 10 monedas acabaria con 18 monedas" (aplicarBloqueDeTransaccionesAUnUsuario pepe (buscarPeorBloqueDeUnUsuarioEnUnaBlockChain blockChain pepe) `shouldBe` pepe{billetera=18}) 
	it "26 - Se le aplica una BlockaChain (compuesta por 1 bloque2 y 10 bloque1) a Pepe, y este termina con una billetera de 115 monedas" (aplicarBlockChainAUnUsuario blockChain pepe `shouldBe` pepe {billetera=115})
	it "27 - Se le aplican los primeros 3 bloques de una BlockChain a Pepe, y este queda con una billetera de 51 monedas" (usuarioDespuesDeUnBloqueN blockChain 3 pepe `shouldBe` pepe {billetera=51})
	it "28 - Se aplica una BlockChain a Pepe y a Lucho, y estos quedan con billeteras de 115 y 0 monedas, respectivamente" (aplicarBlockChainAUnConjuntoDeUsuarios blockChain [pepe,lucho] `shouldBe` [pepe {billetera=115}, lucho {billetera=0}])
	describe "Tests de BlockChain infinita:" $ do
	it "29 - Pepe llega a tener 10000 creditos despues de aplicarle 11 bloques de una BlockChain infinita" (enCuantosBloquesDeUnBlockChainInfinitoUnUsuarioLlegaADiezMilMonedas bloque1 pepe `shouldBe` 11)

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
upgrade billetera = ((+) billetera.comoMaximo10.(*) 0.2) billetera 
cierreDeCuenta billetera = 0
quedaIgual =  id  


-------------------------Transacciones--------------------------------

compararDosNombres nombre1 nombre2 = nombre1 == nombre2

transaccion :: Evento-> Usuario-> Usuario-> Evento

transaccion evento usuarioAlQueSeLeDebeAplicarLaTransaccion usuarioAlQueSeLeIntentaAplicarLaTransaccion
  |  compararDosNombres (nombre usuarioAlQueSeLeDebeAplicarLaTransaccion) (nombre usuarioAlQueSeLeIntentaAplicarLaTransaccion) = evento 
  |  otherwise = quedaIgual

--Tests Transacciones
transaccion1 = transaccion cierreDeCuenta lucho
transaccion2 = transaccion (deposito 5) pepe

ejecutarTestTransacciones = hspec $ do
	transaccion1EnPepe
	transaccion2EnPepe
	transaccion2EnPepe2

transaccion1EnPepe = it "11 - Transaccion: 'Lucho cierra la cuenta' aplicada en Pepe. Produce el evento 'Queda igual', que cuando se aplica a una billetera de 10 monedas, esta termina con el mismo monto. Billetera inicial: 10, Billetera final: 10" ((transaccion1 pepe.billetera) pepe  `shouldBe` 10) 
transaccion2EnPepe = it "12 - Transaccion 'Pepe deposita 5 monedas' en Pepe. Produce el evento 'deposito de 5 monedas', que aplicado a una billetera de 10 monedas quedaria: Billetera inicial: 10, Billetera final: 15" ((transaccion2 pepe.billetera) pepe `shouldBe` 15)
transaccion2EnPepe2 = it "13 - Transaccion 'Pepe deposita 5 monedas' en Pepe2. Produce el evento 'deposito de 5 monedas', que aplicado a una billetera de 50 monedas quedaria: Billetera inicial: 50, Billetera final: 55" ((transaccion2 pepe2.billetera) pepe2 `shouldBe` 55)


--------------------------NuevosEventos------------------------------

tocoMeVoy =  (cierreDeCuenta.upgrade.deposito 15) 
ahorranteErrante = (deposito 10.upgrade.deposito 8.extraccion 1.deposito 2.deposito 1) 

--Test Nuevos Eventos

transaccion3 = transaccion tocoMeVoy lucho2
transaccion4 = transaccion ahorranteErrante lucho2
lucho2 = Usuario "Luciano" 10 --creo otro solo para las pruebas

ejecutarTestNuevosEventos = hspec $ do
	transaccion3EnLucho
	transaccion4EnLucho


transaccion3EnLucho = it "14 - Transaccion: 'Lucho toca y se va' en Lucho. Produce el evento 'Cierre de cuenta' que aplicado a una billetera de 10 monedas quedaria: Billetera inicial: 10, Billetera final: 0" ((transaccion3 lucho2.billetera) lucho2 `shouldBe` 0)
transaccion4EnLucho = it "15 -  Transaccion: 'Lucho es un ahorrante errante' en Lucho. Produce los eventos 'deposito  de 1 moneda', 'deposito de 2 monedas', 'extraccion de 1 moneda', 'deposito de 8 monedas', 'upgrade' y 'deposito de 10 monedas', que al aplicarlo a una billetera de 10 monedas, quedaria: Billetera inicial: 10 Billetera final: 34" ((transaccion4 lucho2.billetera) lucho2 `shouldBe` 34)

--------------------------Pago entre usuarios------------------------

pagoEntreUsuarios :: Usuario->Usuario->Float->Usuario->Evento

pagoEntreUsuarios usuarioExtraccion usuarioRecibeDeposito montoDeLaTransaccion usuarioAlQueSeLeDebeAplicarLaTransaccion  
	| compararDosNombres (nombre usuarioExtraccion) (nombre usuarioAlQueSeLeDebeAplicarLaTransaccion) =  extraccion montoDeLaTransaccion 
	| compararDosNombres (nombre usuarioRecibeDeposito) (nombre usuarioAlQueSeLeDebeAplicarLaTransaccion) = deposito montoDeLaTransaccion 
	| otherwise = quedaIgual

transaccion5 :: Usuario->Evento
transaccion5 = pagoEntreUsuarios pepe lucho 7 

ejecutarTestPagoEntreUsuarios = hspec $ do
	transaccion5EnPepe
	transaccion5EnLucho

transaccion5EnPepe = it "16 - Transaccion: 'Pepe le da 7 unidades a Lucho' en Pepe. Produce le evento 'extraccion de 7 unidades' que al aplicarlo a una billetera de 10 monedas, la misma queda con 3 monedas" ((transaccion5 pepe.billetera) pepe `shouldBe` 3) 
transaccion5EnLucho = it "17 - Transaccion: 'Pepe le da 7 unidades a Lucho' en Pepe. Produce le evento 'deposito de 7 unidades' que al aplicarlo a una billetera de 10 monedas, la misma queda con 17 monedas" ((transaccion5 lucho2.billetera) lucho2 `shouldBe` 17)


-----------------------------------------------------2da Entrega--------------------------------------------------------------

type Bloque = [Usuario->Evento]

bloque1 :: Bloque
bloque2 :: Bloque
bloque1 = [transaccion1,transaccion2,transaccion2,transaccion2,transaccion3,transaccion4,transaccion5,transaccion3]
bloque2 = [transaccion2,transaccion2,transaccion2,transaccion2,transaccion2]

listaDeUsuarios = [pepe,lucho]

aplicarTransaccionAUnUsuario usuario transaccion = usuario {billetera = (transaccion usuario.billetera) usuario} 

-------------------------------------------------------------Bloques----------------------------------------------------------

--punto 21 de Bloques

--aplicarBloqueDeTransaccionesAUnUsuario :: Bloque->Usuario->Usuario
--la lista que recibe ((cabeza:cola)) debe ser un Bloque (lista de transacciones)
{-
aplicarBloqueDeTransaccionesAUnUsuario [] usuario = usuario
aplicarBloqueDeTransaccionesAUnUsuario (cabeza:cola) usuario = aplicarBloqueDeTransaccionesAUnUsuario cola (aplicarTransaccionAUnUsuario usuario cabeza)
-}

aplicarBloqueDeTransaccionesAUnUsuario usuario = foldl aplicarTransaccionAUnUsuario usuario 

--punto 22 de bloques
quienTieneMasDeN :: Bloque->[Usuario]->Float->[Usuario]
quienTieneMasDeN bloqueAAplicar lista numero = filter (\usuario-> billetera (aplicarBloqueDeTransaccionesAUnUsuario usuario bloqueAAplicar) > numero)  lista

--punto 23 de bloques
billeteraMasRica bloqueAAplicar lista = maximum (map(billetera.(flip aplicarBloqueDeTransaccionesAUnUsuario) bloqueAAplicar) lista)

quienesMasRico :: Bloque->[Usuario]->Usuario
quienesMasRico bloqueAAplicar lista = fromJust (find (\usuario-> billetera (aplicarBloqueDeTransaccionesAUnUsuario usuario bloqueAAplicar) == (billeteraMasRica bloqueAAplicar lista))  lista)

--punto 24 de bloques
billeteraMenosRica bloqueAAplicar lista = minimum (map(billetera.(flip aplicarBloqueDeTransaccionesAUnUsuario) bloqueAAplicar) lista)

quienEsMenosRico :: Bloque->[Usuario]->Usuario
quienEsMenosRico bloqueAAplicar lista = fromJust (find (\usuario-> billetera (aplicarBloqueDeTransaccionesAUnUsuario usuario bloqueAAplicar) == (billeteraMenosRica bloqueAAplicar lista))  lista)



----------------------------------------------------------BlockChain------------------------------------------------------------

type BlockChain = [Bloque]
blockChain:: BlockChain

blockChain = [bloque2,bloque1,bloque1,bloque1,bloque1,bloque1,bloque1,bloque1,bloque1,bloque1,bloque1]

--punto 25 de BlockChain

billeteraDeUnUsuarioDespuesDeUnBloque usuario bloque = (billetera.aplicarBloqueDeTransaccionesAUnUsuario usuario) bloque 
listaDeBilleterasDeUnUsuarioDespuesDeUnaBlockChain blockChain usuario = map (billeteraDeUnUsuarioDespuesDeUnBloque usuario) blockChain
compararSiUnUsuarioTerminaConUnSaldoNDespuesDeUnBloque saldo usuario bloque = saldo == (billetera.aplicarBloqueDeTransaccionesAUnUsuario usuario) bloque  

buscarPeorBloqueDeUnUsuarioEnUnaBlockChain :: BlockChain->Usuario->Bloque
buscarPeorBloqueDeUnUsuarioEnUnaBlockChain blockChain usuario = fromJust (find (compararSiUnUsuarioTerminaConUnSaldoNDespuesDeUnBloque ((minimum.listaDeBilleterasDeUnUsuarioDespuesDeUnaBlockChain blockChain) usuario) usuario) blockChain)

--punto 26 de blockChain

aplicarBlockChainAUnUsuario :: BlockChain->Usuario->Usuario
--la lista que recibe ((cabeza:cola)) debe ser una blockChain (lista de bloques)
{-
aplicarBlockChainAUnUsuario [] usuario = usuario
aplicarBlockChainAUnUsuario (cabeza:cola) usuario = aplicarBlockChainAUnUsuario cola (aplicarBloqueDeTransaccionesAUnUsuario usuario cabeza)
-}
aplicarBlockChainAUnUsuario blockChain usuario = foldl aplicarBloqueDeTransaccionesAUnUsuario usuario blockChain 

--punto 27 de blockChain
usuarioDespuesDeUnBloqueN :: BlockChain->Int->Usuario->Usuario
usuarioDespuesDeUnBloqueN blockChain numeroDeBloque = aplicarBlockChainAUnUsuario (take numeroDeBloque blockChain) 

--punto 28 de blockChain
aplicarBlockChainAUnConjuntoDeUsuarios :: BlockChain->[Usuario]->[Usuario]
aplicarBlockChainAUnConjuntoDeUsuarios blockChain listaDeUsuarios = map (aplicarBlockChainAUnUsuario blockChain) listaDeUsuarios

----------------------------------------------------BlockChain infinito--------------------------------------------------------

--punto 29 de BlockChain infinito

agregarBloque lista = (lista++lista) : agregarBloque (lista++lista)
crearBlockChainInfinito bloque = bloque : agregarBloque bloque

--la lista que recibe ((cabeza:cola)) debe ser un BlockChain infinito (lista infinita de bloques)
aplicarBlockChainInfinitoAUnUsuarioHastaLlegarADiezMilMonedas (cabeza:cola) usuario  | (billetera usuario) >= 10000 = 0
																	 				 | otherwise = 1 + aplicarBlockChainInfinitoAUnUsuarioHastaLlegarADiezMilMonedas cola (aplicarBloqueDeTransaccionesAUnUsuario usuario cabeza)

enCuantosBloquesDeUnBlockChainInfinitoUnUsuarioLlegaADiezMilMonedas :: Bloque->Usuario->Int
enCuantosBloquesDeUnBlockChainInfinitoUnUsuarioLlegaADiezMilMonedas bloque usuario = aplicarBlockChainInfinitoAUnUsuarioHastaLlegarADiezMilMonedas (crearBlockChainInfinito bloque) usuario

--El concepto clave que se aplico en este punto es el de evaluacion diferida, ya que sin esto no se hubiese podido trabajar con una lista infinita
--porque es imposible evaluar completamente una lista infinita, pero con la evaluacion diferida se hace posible evaluar una parte de esa lista
--infinita, la parte que haga falta evaluar