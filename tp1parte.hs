{-# LANGUAGE NoMonomorphismRestriction #-}
import Text.Show.Functions
import Data.List
import Data.Maybe

data Usuario = Usuario {
 nombre :: String,
 billetera :: Float
} deriving (Show)

victor = Usuario "victor" 10
evento nuevabilletera unusuario = unusuario { billetera=nuevabilletera}
comominimo0 numero = max numero 0
comomaximo10 numero = min numero 10
deposito montodepo unusuario = evento (billetera unusuario + montodepo) unusuario
extraccion montoextr unusuario = evento (comominimo0 (billetera unusuario - montoextr)) unusuario
upgrade porcentaje unusuario = evento (comomaximo10 (billetera unusuario * porcentaje) + billetera unusuario) unusuario
cierredecuenta unusuario = evento 0 unusuario
quedaigual unusuario = evento (billetera unusuario) unusuario
