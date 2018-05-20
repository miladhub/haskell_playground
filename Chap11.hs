module Chap11 where

data Price =
    Price Integer deriving (Eq, Show)

data Manufacturer =
    Mini
    | Mazda
    | Tata
    deriving (Eq, Show)

data Airline =
    PapuAir
    | CatapultsR'Us
    | TakeYourChancesUnited
    deriving (Eq, Show)

data Size = Size Int
:
data Vehicle =
    Car Manufacturer Price
    | Plane Airline Size
    deriving (Eq, Show)

myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane (Plane _) = True
isPlane _ = False

areCars :: [Vehicle] -> [Bool]
areCars = fmap isCar

getManu :: Vehicle -> Manufacturer
getManu (Car m _) = m
