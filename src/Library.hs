module Library where
import PdePreludat
import Data.Int (Int)
import GHC.Integer (Integer)

-- Rambo tiene dos armas: una principal y una secundaria, cada una
-- con un cargador de cierta capacidad con alguna cantidad de balas

data Arma = UnArma {
    balas :: Number,
    tamañoCargador :: Number
} deriving (Show, Eq)

data Rambo = UnRambo {
    armaPrincipal :: Arma,
    armaSecundaria :: Arma
} deriving (Show, Eq)

-- 1) Averiguar cuantas balas le quedan a Rambo en total
-- "me meto al arma principal de rambo y despues me meto a las balas de dicha arma"
balasTotales :: Rambo -> Number
balasTotales rambo = balas (armaPrincipal rambo) + balas (armaSecundaria rambo)

-- 2) Dada un arma, disparar si tiene balas, algunas consideraciones:
-- Si el cargador esta lleno, Rambo se confia y dispara 2 tiros
-- Si el cargador NO esta lleno pero tiene al menos una bala la dispara (es decir que consumio balas)
-- Si el cargador esta vacio, Rambo gatilla pero no pasa nada

-- EL ORDEN ES MUY IMPORTANTE!!

-- "trabajo sobre un arma y devuelva otra arma que representa como seria el arma disparada"
disparar :: Arma -> Arma
disparar arma 
    | balas arma == tamañoCargador arma = arma{balas = balas arma - 2} 
    | balas arma > 0                    = arma{balas = balas arma - 1}
    | otherwise                         = arma 
-- devuelve la misma arma!!

-- 3) Hacer que Rambo dispare todo a la vez (cambio a rambo!!)
-- rambo antes de disparar -> rambo despues de disparar (la copia del rambo original pero luego de haber disparado)

dispararTodo :: Rambo -> Rambo
dispararTodo rambo = rambo{
            armaPrincipal = disparar (armaPrincipal rambo), 
            armaSecundaria = disparar (armaSecundaria rambo)
                    }

                    