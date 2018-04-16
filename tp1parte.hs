
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
victor = Usuario "victor" 10
pepe = Usuario "Jose" 10
lucho = Usuario "Luciano" 2
pepe2 = Usuario "Jose" 50

--Ejecucion de todos los tests

ejecutarTests = hspec $ do
	describe "Tests Eventos:" $ do
	it "1 - Depositar 10 monedas en una billetera de 10 monedas. Billetera inicial: 10, Billetera final: 20" (billetera (deposito 10 pepe) `shouldBe` 20)
	it "2 - Extraer 3 monedas de una billetera de 10 monedas. Billetera inicial: 10, Billetera final: 7" (billetera (extraccion 3 pepe) `shouldBe` 7)
	it "3 - Extraer 15 monedas de una billetera de 10 monedas. Billetera inicial: 10, Billetera final: 0" (billetera (extraccion 15 pepe) `shouldBe` 0)
	it "4 - Upgrade en una billetera de 10 monedas. Billetera inicial: 10, Billetera final: 12" (billetera (upgrade pepe) `shouldBe` 12)
	it "5 - Cerar la cuenta en una billetera de 10 monedas. Billetera inicial: 10, Billetera final: 0" (billetera (cierreDeCuenta pepe) `shouldBe` 0)
	it "6 - Evento 'queda igual' en una billetera de 10 monedas. Billetera inicial: 10, Billetera final: 10" (billetera (quedaIgual pepe) `shouldBe` 10)
	it "7 - Depositar 1000 monedas y luego hacer un upgrade en una billetera de 10 monedas. Billetera inicial: 10, Billetera final: 1020" (billetera ((upgrade.(deposito 1000)) pepe) `shouldBe` 1020)
	describe "Tests Usuarios:" $ do
	it "8 - Billetera de Pepe: 10" (billetera pepe `shouldBe` 10)
	it "9 - Evento 'cierre de cuenta' en la billetera de pepe de 10 monedas. Billetera final: 0" (billetera (cierreDeCuenta pepe)  `shouldBe` 0)
	it "10 - Se depositan 15 monedas, extraen 2 y tiene un upgrade la billetera de Pepe de 10 monedas. Billetera inicial: 10, Billetera final: 27.6" (billetera ((upgrade.(extraccion 2).(deposito 15)) pepe) `shouldBe` 27.6)
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

type Evento = Usuario -> Usuario

evento:: Float->Evento
deposito :: Float->Evento
extraccion :: Float->Evento
upgrade :: Evento
cierreDeCuenta :: Evento
quedaIgual :: Evento


evento nuevaBilletera unUsuario = unUsuario { billetera=nuevaBilletera}
comoMinimo0 numero = max numero 0
comoMaximo10 numero = min numero 10
deposito montoADepositar unUsuario = evento (billetera unUsuario + montoADepositar) unUsuario
extraccion montoAExtraer unUsuario = evento (comoMinimo0 (billetera unUsuario - montoAExtraer)) unUsuario
upgrade unUsuario = evento (comoMaximo10 (billetera unUsuario * 0.2) + billetera unUsuario) unUsuario
cierreDeCuenta unUsuario = evento 0 unUsuario
quedaIgual unUsuario = evento (billetera unUsuario) unUsuario

-------------------------Transacciones--------------------------------


transaccion :: Evento-> Usuario-> Usuario-> Evento

transaccion evento usuarioAlQueSeLeDebeAplicarLaTransaccion usuarioAlQueSeLeIntentaAplicarLaTransaccion
  |  nombre usuarioAlQueSeLeDebeAplicarLaTransaccion == nombre usuarioAlQueSeLeIntentaAplicarLaTransaccion = evento
  |  otherwise = quedaIgual

--Tests Transacciones
transaccion1 = transaccion cierreDeCuenta lucho
transaccion2 = transaccion (deposito 5) pepe

ejecutarTestTransacciones = hspec $ do
	transaccion1EnPepe
	transaccion2EnPepe
	transaccion2EnPepe2

transaccion1EnPepe = it "11 - Transaccion: 'Lucho cierra la cuenta' aplicada en Pepe. Produce el evento 'Queda igual', que cuando se aplica a una billetera de 10 monedas, esta termina con el mismo monto. Billetera inicial: 10, Billetera final: 10" (billetera ((transaccion1 pepe) pepe)  `shouldBe` billetera pepe)
transaccion2EnPepe = it "12 - Transaccion 'Pepe deposita 5 monedas' en Pepe. Produce el evento 'deposito de 5 monedas', que aplicado a una billetera de 10 monedas quedaria: Billetera inicial: 10, Billetera final: 15" (billetera ((transaccion2 pepe) pepe) `shouldBe` (billetera pepe)+5)
transaccion2EnPepe2 = it "13 - Transaccion 'Pepe deposita 5 monedas' en Pepe2. Produce el evento 'deposito de 5 monedas', que aplicado a una billetera de 50 monedas quedaria: Billetera inicial: 50, Billetera final: 55" (billetera ((transaccion2 pepe2) pepe2) `shouldBe` (billetera pepe2)+5)


--------------------------NuevosEventos------------------------------

tocoMeVoy usuario =  (cierreDeCuenta.upgrade.deposito 15) usuario
ahorranteErrante usuario = (deposito 10.upgrade.deposito 8.extraccion 1.deposito 2.deposito 1) usuario

--Test Nuevos Eventos

transaccion3 = transaccion tocoMeVoy lucho2
transaccion4 = transaccion ahorranteErrante lucho2
lucho2 = Usuario "Luciano" 10 --creo otro porque se pide probarlo en una billetera de 10

ejecutarTestNuevosEventos = hspec $ do
	transaccion3EnLucho
	transaccion4EnLucho


transaccion3EnLucho = it "14 - Transaccion: 'Lucho toca y se va' en Lucho. Produce el evento 'Cierre de cuenta' que aplicado a una billetera de 10 monedas quedaria: Billetera inicial: 10, Billetera final: 0" (billetera ((transaccion3 lucho2) lucho2) `shouldBe` 0)
transaccion4EnLucho = it "15 - Transaccion: 'Lucho es un ahorrante errante' en Lucho. Produce los eventos 'deposito  de 1 moneda', 'deposito de 2 monedas', 'extraccion de 1 moneda', 'deposito de 8 monedas', 'upgrade' y 'deposito de 10 monedas', que al aplicarlo a una billetera de 10 monedas, quedaria: Billetera inicial: 10 Billetera final: 34" (billetera ((transaccion4 lucho2) lucho2) `shouldBe` 34)

--------------------------Pago entre usuarios------------------------

pagoEntreUsuarios :: Usuario->Usuario->Float->Usuario->Evento

pagoEntreUsuarios usuarioExtraccion usuarioRecibeDeposito montoDeLaTransaccion usuarioAlQueSeLeDebeAplicarLaTransaccion  
	| nombre usuarioExtraccion == nombre usuarioAlQueSeLeDebeAplicarLaTransaccion =  extraccion montoDeLaTransaccion
	| nombre usuarioRecibeDeposito == nombre usuarioAlQueSeLeDebeAplicarLaTransaccion = deposito montoDeLaTransaccion 
	| otherwise = quedaIgual

--Test Pago entre usuarios 

transaccion5 :: Usuario->Evento
transaccion5 = pagoEntreUsuarios pepe lucho 7 

ejecutarTestPagoEntreUsuarios = hspec $ do
	transaccion5EnPepe
	transaccion5EnLucho

transaccion5EnPepe = it "16 - Transaccion: 'Pepe le da 7 unidades a Lucho' aplicada a Pepe. Produce le evento 'extraccion de 7 unidades' que al aplicarlo a una billetera de 10 monedas, la misma queda con 3 monedas" (billetera ((transaccion5 pepe) pepe) `shouldBe` 3)
transaccion5EnLucho = it "17 - Transaccion: 'Pepe le da 7 unidades a Lucho' aplicada a Pepe. Produce le evento 'deposito de 7 unidades' que al aplicarlo a una billetera de 10 monedas, la misma queda con 17 monedas" (billetera ((transaccion5 lucho2) lucho2) `shouldBe` 17)
