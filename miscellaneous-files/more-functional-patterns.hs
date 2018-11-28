bindExp :: Integer -> String
bindExp x =
  let y = 5 in
    let z = y + x in
      "the integer was: "
      ++ show x ++ " and y was: "
      ++ show y ++ " and z was: "
      ++ show z

data WherePenguinsLive
  = Galapagos
  | Antarctica
  | Australia
  | SouthAfrica
  | SouthAmerica
  deriving (Show, Eq)

data Penguin =
  Peng WherePenguinsLive
  deriving (Eq, Show)

isSouthAfrica :: WherePenguinsLive -> Bool
isSouthAfrica SouthAfrica  = True
isSouthAfrica _            = False

gimmeWhereTheyLive :: Penguin
                   -> WherePenguinsLive

gimmeWhereTheyLive (Peng whereitlives)
  = whereitlives

galapagosPenguin :: Penguin -> Bool
galapagosPenguin (Peng Galapagos) = True
galapagosPenguin _                = False

antarcticPenguin :: Penguin -> Bool
antarcticPenguin (Peng Antarctica) = True
antarcticPenguin _                = False

antarcticaOrGalapagos :: Penguin -> Bool
antarcticaOrGalapagos p =
  antarcticPenguin p || galapagosPenguin p

