module Chapter07.Penguins where

data WherePenguinsLive = Galapagos
                       | Antarctica
                       | Australia
                       | SouthAfrica
                       | SouthAmerica deriving (Eq, Show)

data Penguin = Penguin WherePenguinsLive deriving (Eq, Show)

isSouthAfrica :: WherePenguinsLive -> Bool
isSouthAfrica SouthAfrica = True
isSouthAfrica _           = False

gimmeWhereTheyLive :: Penguin -> WherePenguinsLive
gimmeWhereTheyLive (Penguin whereItLives) = whereItLives

humboldt = Penguin SouthAmerica
gentoo = Penguin Antarctica
macaroni = Penguin Antarctica
little = Penguin Australia
galapagos = Penguin Galapagos

galapagosPenguin :: Penguin -> Bool
galapagosPenguin (Penguin Galapagos) = True
galapagosPenguin _                   = False

antarcticPenguin :: Penguin -> Bool
antarcticPenguin (Peng Antarctica) = True
antarcticPenguin _                 = False

antarcticOrGalapagos :: Penguin -> Bool
antarcticOrGalapagos p = (galapagosPenguin p) || (antarcticPenguin p)
