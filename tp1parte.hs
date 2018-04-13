
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

type Evento = Usuario -> Usuario

evento nuevabilletera unusuario = unusuario { billetera=nuevabilletera}
comominimo0 numero = max numero 0
comomaximo10 numero = min numero 10
deposito montodepo unusuario = evento (billetera unusuario + montodepo) unusuario
extraccion montoextr unusuario = evento (comominimo0 (billetera unusuario - montoextr)) unusuario
--upgrade con 20% fijo
upgrade unusuario = evento (comomaximo10 (billetera unusuario * 0.2) + billetera unusuario) unusuario
cierredecuenta unusuario = evento 0 unusuario
quedaigual unusuario = evento (billetera unusuario) unusuario


-------------------------Transacciones--------------------------------


transaccion :: Evento-> Usuario-> Usuario-> Evento

transaccion evento usuarioAlQueSeLeDebeAplicarLaTransaccion usuarioAlQueSeLeIntentaAplicarLaTransaccion
  |  nombre usuarioAlQueSeLeDebeAplicarLaTransaccion == nombre usuarioAlQueSeLeIntentaAplicarLaTransaccion = evento
  |  otherwise = quedaigual

--Tests Transacciones
transaccion1 = transaccion cierredecuenta lucho
transaccion2 = transaccion (deposito 5) pepe

ejecutarTestTransacciones = hspec $ do
	transaccion1EnPepe
	transaccion2EnPepe
	transaccion2EnPepe2

transaccion1EnPepe = it "La billetera de pepe queda igual al querersele realizar la transaccion: 'Lucho cierra la cuenta'" (billetera ((transaccion1 pepe) pepe)  `shouldBe` billetera pepe)
transaccion2EnPepe = it "Transaccion 'Pepe deposita 5 monedas' en Pepe. Billetera inicial: 10, Billetera final: 15" (billetera ((transaccion2 pepe) pepe) `shouldBe` (billetera pepe)+5)
transaccion2EnPepe2 = it "Transaccion 'Pepe deposita 5 monedas' en Pepe2. Billetera inicial: 50, Billetera final: 55" (billetera ((transaccion2 pepe2) pepe2) `shouldBe` (billetera pepe2)+5)


--------------------------NuevosEventos------------------------------

tocoMeVoy usuario =  (cierredecuenta.upgrade.deposito 15) usuario
ahorranteErrante usuario = (deposito 10.upgrade.deposito 8.extraccion 1.deposito 2.deposito 1) usuario

--Test Nuevos Eventos

transaccion3 = transaccion tocoMeVoy lucho2
transaccion4 = transaccion ahorranteErrante lucho2
lucho2 = Usuario "Luciano" 10 --creo otro porque se pide probarlo en una billetera de 10

ejecutarTestNuevosEventos = hspec $ do
	transaccion3EnLucho
	transaccion4EnLucho


transaccion3EnLucho = it "Transaccion: 'Lucho toca y se va' en Lucho. Su billetera queda en 0" (billetera ((transaccion3 lucho2) lucho2) `shouldBe` 0)
transaccion4EnLucho = it "Transaccion: 'Lucho es un ahorrante errante' en Lucho. Billetera inicial: 10 Billetera final: 34" (billetera ((transaccion4 lucho2) lucho2) `shouldBe` 34)


pagosentreusuarios usuarioAlQueSeLeDebeAplicarLaTransaccion montoaextraerodepositar
 | nombre usuarioAlQueSeLeDebeAplicarLaTransaccion == "Jose" = extraccion montoaextraerodepositar usuarioAlQueSeLeDebeAplicarLaTransaccion
 | nombre usuarioAlQueSeLeDebeAplicarLaTransaccion == "Luciano" = deposito montoaextraerodepositar usuarioAlQueSeLeDebeAplicarLaTransaccion
 | otherwise = quedaigual usuarioAlQueSeLeDebeAplicarLaTransaccion
