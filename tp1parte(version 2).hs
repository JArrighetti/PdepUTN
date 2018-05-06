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
	it "1 - Al depositar 10 monedas en una billetera de 10, produce una billetera de 20" (deposito 10 billeteraDePrueba `shouldBe` 20)
	it "2 - Extraer 3 monedas de una billetera de 10 monedas, produce una billetera de 7" (extraccion 3 billeteraDePrueba `shouldBe` 7)
	it "3 - Extraer 15 monedas de una billetera de 10 monedas, produce una billetera de 0" (extraccion 15 billeteraDePrueba `shouldBe` 0)
	it "4 - Upgrade en una billetera de 10 monedas, produce una billetera de 12" (upgrade billeteraDePrueba `shouldBe` 12)
	it "5 - Cerrar la cuenta en una billetera de 10 monedas, produce una billetera de 0" (cierreDeCuenta billeteraDePrueba `shouldBe` 0)
	it "6 - Evento 'queda igual' en una billetera de 10 monedas, produce una billetera de 10" (quedaIgual billeteraDePrueba `shouldBe` 10)
	it "7 - Depositar 1000 monedas y luego hacer un upgrade en una billetera de 10 monedas, produce una billetera de 1020" ((upgrade.deposito 1000) billeteraDePrueba `shouldBe` 1020)
	describe "Tests Usuarios:" $ do
	it "8 - Billetera de Pepe: 10" (billetera pepe `shouldBe` 10)
	it "9 - Evento 'cierre de cuenta' en la billetera de pepe de 10 monedas, produce una billetera de 0" ((cierreDeCuenta.billetera) pepe  `shouldBe` 0)
	it "10 - Se depositan 15 monedas, extraen 2 y tiene un upgrade la billetera de Pepe de 10 monedas, con esto se produce una billetera de 27.6" ((upgrade.extraccion 2.deposito 15.billetera) pepe `shouldBe` 27.6)
	describe "Tests Transacciones:" $ do
	it "11 - Transaccion: 'Lucho cierra la cuenta' aplicada en Pepe. Produce el evento 'Queda igual', que cuando se aplica a una billetera de 10 monedas, produce una billetera de 10" ((transaccion1LuchoCierraLaCuenta pepe.billetera) pepe  `shouldBe` 10)
	it "12 - Transaccion 'Pepe deposita 5 monedas' en Pepe. Produce el evento 'deposito de 5 monedas', que aplicado a una billetera de 10 monedas, produce una billetera de 15" ((transaccion2PepeDepositaCincoMonedas pepe.billetera) pepe `shouldBe` 15)
	it "13 - Transaccion 'Pepe deposita 5 monedas' en Pepe2. Produce el evento 'deposito de 5 monedas', que aplicado a una billetera de 50 monedas, produce una billetera de 55" ((transaccion2PepeDepositaCincoMonedas pepe2.billetera) pepe2 `shouldBe` 55)
	describe "Tests Nuevos Eventos:" $ do
	it "14 - Transaccion: 'Lucho toca y se va' en Lucho. Produce el evento 'Cierre de cuenta' que aplicado a una billetera de 10 monedas, produce una billetera de 0" ((transaccion3LuchoTocaYSeVa lucho2.billetera) lucho2 `shouldBe` 0)
	it "15 -  Transaccion: 'Lucho es un ahorrante errante' en Lucho. Produce los eventos 'deposito  de 1 moneda', 'deposito de 2 monedas', 'extraccion de 1 moneda', 'deposito de 8 monedas', 'upgrade' y 'deposito de 10 monedas', que al aplicarlo a una billetera de 10 monedas, quedaria: Billetera inicial: 10 Billetera final: 34" ((transaccion4LuchoEsUnAhorranteErrante lucho2.billetera) lucho2 `shouldBe` 34)
	describe "Tests Pago entre usuarios:" $ do
	it "16 - Transaccion: 'Pepe le da 7 unidades a Lucho' en Pepe. Produce le evento 'extraccion de 7 unidades' que al aplicarlo a una billetera de 10 monedas, lproduce una billetera de 3" ((transaccion5LuchoLePagaSieteMonedasApepe pepe.billetera) pepe `shouldBe` 3)
	it "17 - Transaccion: 'Pepe le da 7 unidades a Lucho' en Pepe. Produce le evento 'deposito de 7 unidades' que al aplicarlo a una billetera de 10 monedas, produce una billetera de 17" ((transaccion5LuchoLePagaSieteMonedasApepe lucho2.billetera) lucho2 `shouldBe` 17)
	describe "\n\nTests de Usuarios luego de una transaccion:" $ do
	it "18 - Si le aplico la transaccion 1 (Lucho cierra la cuenta) a pepe, su billetera de 10 monedas queda igual" (aplicarTransaccionAUnUsuario pepe transaccion1LuchoCierraLaCuenta `shouldBe` pepe)
	it "19 - Si le aplico la transaccion 5 (Lucho le paga 7 monedas a Pepe) a lucho, su billetera de 2 monedas termina con 9" (aplicarTransaccionAUnUsuario lucho transaccion5LuchoLePagaSieteMonedasApepe `shouldBe` lucho{billetera= 9})
	it "20 - Si le aplico la transaccion 5 (Lucho le paga 7 monedas a Pepe) y luego la transaccion 2 (Pepe deposita 5 monedas) a pepe, su billetera de 10 monedas termina con 8" (aplicarTransaccionAUnUsuario (aplicarTransaccionAUnUsuario pepe transaccion2PepeDepositaCincoMonedas) transaccion5LuchoLePagaSieteMonedasApepe `shouldBe` pepe{billetera= 8})
	describe "Tests de Bloques:" $ do
	it "21 - Si le aplico el bloque 1 a pepe este deberia quedar con una billetera de 18" (aplicarBloqueDeTransaccionesAUnUsuario  bloque1 pepe `shouldBe` pepe{billetera=18} )
	it "22 - A partir del bloque 1 y la usuarios de usuarios pepe y lucho el unico que supera los 10 pesos despues del bloque es pepe" (quienTieneMasDeN bloque1 usuariosDeUsuarios 10 `shouldBe` [pepe])
	it "23 - A partir del bloque 1 y la usuarios de usuarios pepe y lucho el mas rico es pepe" (quienEsMasRico bloque1 usuariosDeUsuarios `shouldBe` pepe)
	it "24 - A partir del bloque 1 y la usuarios de usuarios pepe y lucho el menos rico es lucho" (quienEsMenosRico bloque1 usuariosDeUsuarios `shouldBe` lucho)
	describe "Tests de BlockChain:" $ do
	it "25 - El peor bloque que tuvo Pepe en la BlockChain fue el bloque1, ya que si comenzara por ese, su billetera de 10 monedas acabaria con 18 monedas" (aplicarBloqueDeTransaccionesAUnUsuario (buscarPeorBloqueDeUnUsuarioEnUnaBlockChain blockChain pepe) pepe `shouldBe` pepe{billetera=18})
	it "26 - Se le aplica una BlockaChain (compuesta por 1 bloque2 y 10 bloque1) a Pepe, y este termina con una billetera de 115 monedas" (aplicarBlockChainAUnUsuario blockChain pepe `shouldBe` pepe {billetera=115})
	it "27 - Se le aplican los primeros 3 bloques de una BlockChain a Pepe, y este queda con una billetera de 51 monedas" (usuarioDespuesDeUnaCantidadDeBloquesNdeUnaBlockChain blockChain 3 pepe `shouldBe` pepe {billetera=51})
	it "28 - Se aplica una BlockChain a Pepe y a Lucho, y estos quedan con billeteras de 115 y 0 monedas, respectivamente" (aplicarBlockChainAUnConjuntoDeUsuarios blockChain [pepe,lucho] `shouldBe` [pepe {billetera=115}, lucho {billetera=0}])
	describe "Tests de BlockChain infinita:" $ do
	it "29 - Pepe llega a tener 10000 creditos despues de aplicarle 11 bloques de una BlockChain infinita" (enCuantosBloquesDeUnaBlockChainUnUsuarioLlegaAUnaCantidadDeDineroN (crearBlockChainInfinito bloque1) 10000 pepe `shouldBe` 11)

comoMinimo0 numero = max numero 0
comoMaximo10 numero = min numero 10

type Evento = Float->Float
type Dinero = Float

deposito :: Dinero->Evento
extraccion :: Dinero->Evento
upgrade :: Evento
cierreDeCuenta :: Evento
quedaIgual :: Evento

deposito montoADepositar billetera = billetera + montoADepositar
extraccion montoAExtraer billetera = comoMinimo0 (billetera-montoAExtraer)
upgrade billetera = ((+) billetera.comoMaximo10.(*) 0.2) billetera
cierreDeCuenta _ = 0
quedaIgual =  id


-------------------------Transacciones--------------------------------

dosUsuariosSonIguales usuario1 usuario2 = nombre usuario1 == nombre usuario2

transaccion :: Evento-> Usuario-> Usuario-> Evento

transaccion evento usuarioAlQueSeLeDebeAplicarLaTransaccion usuarioAlQueSeLeIntentaAplicarLaTransaccion
  |  dosUsuariosSonIguales usuarioAlQueSeLeDebeAplicarLaTransaccion usuarioAlQueSeLeIntentaAplicarLaTransaccion = evento
  |  otherwise = quedaIgual

transaccion1LuchoCierraLaCuenta = transaccion cierreDeCuenta lucho
transaccion2PepeDepositaCincoMonedas = transaccion (deposito 5) pepe

--------------------------NuevosEventos------------------------------

tocoMeVoy =  cierreDeCuenta.upgrade.deposito 15
ahorranteErrante = deposito 10.upgrade.deposito 8.extraccion 1.deposito 2.deposito 1

transaccion3LuchoTocaYSeVa = transaccion tocoMeVoy lucho2
transaccion4LuchoEsUnAhorranteErrante = transaccion ahorranteErrante lucho2
lucho2 = Usuario "Luciano" 10 --creo otro solo para las pruebas

--------------------------Pago entre usuarios------------------------

pagoEntreUsuarios :: Usuario->Usuario->Float->Usuario->Evento

pagoEntreUsuarios usuarioExtraccion usuarioRecibeDeposito montoDeLaTransaccion usuarioAlQueSeLeDebeAplicarLaTransaccion
	| dosUsuariosSonIguales usuarioExtraccion usuarioAlQueSeLeDebeAplicarLaTransaccion =  extraccion montoDeLaTransaccion
	| dosUsuariosSonIguales usuarioRecibeDeposito usuarioAlQueSeLeDebeAplicarLaTransaccion = deposito montoDeLaTransaccion
	| otherwise = quedaIgual

transaccion5LuchoLePagaSieteMonedasApepe :: Usuario->Evento
transaccion5LuchoLePagaSieteMonedasApepe = pagoEntreUsuarios pepe lucho 7

-----------------------------------------------------2da Entrega--------------------------------------------------------------

type Bloque = [Usuario->Evento]

bloque1 :: Bloque
bloque2 :: Bloque
bloque1 = [transaccion1LuchoCierraLaCuenta,transaccion2PepeDepositaCincoMonedas,transaccion2PepeDepositaCincoMonedas,transaccion2PepeDepositaCincoMonedas,transaccion3LuchoTocaYSeVa,transaccion4LuchoEsUnAhorranteErrante,transaccion5LuchoLePagaSieteMonedasApepe,transaccion3LuchoTocaYSeVa]
bloque2 = [transaccion2PepeDepositaCincoMonedas,transaccion2PepeDepositaCincoMonedas,transaccion2PepeDepositaCincoMonedas,transaccion2PepeDepositaCincoMonedas,transaccion2PepeDepositaCincoMonedas]

usuariosDeUsuarios = [pepe,lucho]

aplicarTransaccionAUnUsuario usuario transaccion = usuario {billetera = (transaccion usuario.billetera) usuario}

-------------------------------------------------------------Bloques----------------------------------------------------------
billeteraDeUnUsuarioDespuesDeUnBloque :: Bloque->Usuario->Float
billeteraDeUnUsuarioDespuesDeUnBloque bloque = billetera.aplicarBloqueDeTransaccionesAUnUsuario bloque

aplicarBloqueDeTransaccionesAUnUsuario :: Bloque->Usuario->Usuario
aplicarBloqueDeTransaccionesAUnUsuario bloque usuario = foldl aplicarTransaccionAUnUsuario usuario bloque


quienTieneMasDeN :: Bloque->[Usuario]->Float->[Usuario]
quienTieneMasDeN bloqueAAplicar usuarios numero = filter ((>numero).billeteraDeUnUsuarioDespuesDeUnBloque bloqueAAplicar) usuarios

quienEsMasRico :: Bloque->[Usuario]->Usuario
quienEsMasRico bloqueAAplicar = calcularMaximoOminimoDeUnaListaSegunUnCriterio (maximoEntreDosUsuariosDespuesDeUnBloque bloqueAAplicar)

quienEsMenosRico :: Bloque->[Usuario]->Usuario
quienEsMenosRico bloqueAAplicar = calcularMaximoOminimoDeUnaListaSegunUnCriterio (minimoEntreDosUsuariosDespuesDeUnBloque bloqueAAplicar) 

calcularMaximoOminimoDeUnaListaSegunUnCriterio :: (t->t->t)->[t]->t
calcularMaximoOminimoDeUnaListaSegunUnCriterio _ [] = error "Excepcion: lista vacia"
calcularMaximoOminimoDeUnaListaSegunUnCriterio _ (cabeza:[]) = cabeza 
calcularMaximoOminimoDeUnaListaSegunUnCriterio criterioParaCalcularMaximoOminimoEntreDosElementos (cabeza:segundaCabeza:cola) = calcularMaximoOminimoDeUnaListaSegunUnCriterio criterioParaCalcularMaximoOminimoEntreDosElementos ((criterioParaCalcularMaximoOminimoEntreDosElementos cabeza segundaCabeza):cola)


maximoEntreDosUsuariosDespuesDeUnBloque :: Bloque->Usuario->Usuario->Usuario
maximoEntreDosUsuariosDespuesDeUnBloque bloque usuario1 usuario2 	| billeteraDeUnUsuarioDespuesDeUnBloque bloque usuario1 > billeteraDeUnUsuarioDespuesDeUnBloque bloque usuario2 = usuario1
																	| otherwise = usuario2
minimoEntreDosUsuariosDespuesDeUnBloque :: Bloque->Usuario->Usuario->Usuario
minimoEntreDosUsuariosDespuesDeUnBloque bloque usuario1 usuario2 	| billeteraDeUnUsuarioDespuesDeUnBloque bloque usuario1 < billeteraDeUnUsuarioDespuesDeUnBloque bloque usuario2 = usuario1
																 	| otherwise = usuario2

----------------------------------------------------------BlockChain------------------------------------------------------------

type BlockChain = [Bloque]

blockChain:: BlockChain
blockChain = [bloque2] ++ replicate 10 bloque1

peorBloqueDeUnUsuario :: Usuario->Bloque->Bloque->Bloque
peorBloqueDeUnUsuario usuario bloque1 bloque2	| billeteraDeUnUsuarioDespuesDeUnBloque bloque1 usuario < billeteraDeUnUsuarioDespuesDeUnBloque bloque2 usuario = bloque1
												| otherwise = bloque2

buscarPeorBloqueDeUnUsuarioEnUnaBlockChain :: BlockChain->Usuario->Bloque
buscarPeorBloqueDeUnUsuarioEnUnaBlockChain blockChain usuario = calcularMaximoOminimoDeUnaListaSegunUnCriterio (peorBloqueDeUnUsuario usuario) blockChain

aplicarBlockChainAUnUsuario :: BlockChain->Usuario->Usuario
aplicarBlockChainAUnUsuario blockChain usuario = foldl (flip aplicarBloqueDeTransaccionesAUnUsuario) usuario blockChain

usuarioDespuesDeUnaCantidadDeBloquesNdeUnaBlockChain :: BlockChain->Int->Usuario->Usuario
usuarioDespuesDeUnaCantidadDeBloquesNdeUnaBlockChain blockChain cantidadDeBloques = aplicarBlockChainAUnUsuario (take cantidadDeBloques blockChain)

aplicarBlockChainAUnConjuntoDeUsuarios :: BlockChain->[Usuario]->[Usuario]
aplicarBlockChainAUnConjuntoDeUsuarios blockChain = map (aplicarBlockChainAUnUsuario blockChain)

----------------------------------------------------BlockChain infinito--------------------------------------------------------

agregarBloque bloque = (bloque++bloque) : agregarBloque (bloque++bloque)
crearBlockChainInfinito bloque = bloque : agregarBloque bloque

enCuantosBloquesDeUnaBlockChainUnUsuarioLlegaAUnaCantidadDeDineroN :: BlockChain->Dinero->Usuario->Int
enCuantosBloquesDeUnaBlockChainUnUsuarioLlegaAUnaCantidadDeDineroN [] _ _ = error "Error: El usuario no alcanza esa cantidad de dinero"
enCuantosBloquesDeUnaBlockChainUnUsuarioLlegaAUnaCantidadDeDineroN (bloque:blockChain) cantidadDeDinero usuario | (billetera usuario) >= cantidadDeDinero = 0
																	 				 	   						| otherwise = 1 + enCuantosBloquesDeUnaBlockChainUnUsuarioLlegaAUnaCantidadDeDineroN blockChain cantidadDeDinero (aplicarBloqueDeTransaccionesAUnUsuario bloque usuario)

--El concepto clave que se aplicó en este punto es el de evaluación diferida, ya que, dado que es imposible evaluar completamente una lista infinita,
--con esto se hace posible trabajar una parte de la misma
