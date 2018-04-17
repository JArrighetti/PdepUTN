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
tocoMeVoy usuario =  (cierreDeCuenta.upgrade.deposito 15) usuario
ahorranteErrante usuario = (deposito 10.upgrade.deposito 8.extraccion 1.deposito 2.deposito 1) usuario


transaccion :: Evento-> Usuario-> Usuario-> Usuario

pagosEntreUsuarios :: Usuario->Usuario->Float->Usuario-> Usuario

pagosEntreUsuarios usuarioExtraccion usuarioRecibeDeposito montoDeLaTransaccion usuarioAlQueSeLeDebeAplicarLaTransaccion
 | nombre usuarioExtraccion == nombre usuarioAlQueSeLeDebeAplicarLaTransaccion = extraccion montoDeLaTransaccion usuarioAlQueSeLeDebeAplicarLaTransaccion
 | nombre usuarioRecibeDeposito == nombre usuarioAlQueSeLeDebeAplicarLaTransaccion = deposito montoDeLaTransaccion usuarioAlQueSeLeDebeAplicarLaTransaccion
 | otherwise = quedaIgual usuarioAlQueSeLeDebeAplicarLaTransaccion


transaccion evento usuarioAlQueSeLeDebeAplicarLaTransaccion usuarioAlQueSeLeIntentaAplicarLaTransaccion
  |  nombre usuarioAlQueSeLeDebeAplicarLaTransaccion == nombre usuarioAlQueSeLeIntentaAplicarLaTransaccion = evento usuarioAlQueSeLeIntentaAplicarLaTransaccion
  |  otherwise = quedaIgual usuarioAlQueSeLeIntentaAplicarLaTransaccion

transaccion1 = transaccion cierreDeCuenta lucho
transaccion2 = transaccion (deposito 5) pepe
transaccion3 = transaccion tocoMeVoy lucho
transaccion4 = transaccion ahorranteErrante lucho
transaccion5 = pagosEntreUsuarios pepe lucho 7

bloque1 = [transaccion1,transaccion2,transaccion2,transaccion2,transaccion3,transaccion4,transaccion5,transaccion3]
pruebadebloque = ((bloque1 !! 0).(bloque1 !! 1).(bloque1 !! 2).(bloque1 !! 3).(bloque1 !! 4).(bloque1 !! 5).(bloque1 !! 6).(bloque1 !! 7))
