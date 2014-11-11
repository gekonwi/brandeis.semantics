{-# LANGUAGE MultiParamTypeClasses #-}

class Catching a b where
	catch :: a -> b -> String

data Human = Human { humanName :: String } deriving (Show)
data Animal = Animal { animalName :: String } deriving (Show)
data Vehicle = Vehicle { vehicleName :: String } deriving (Show)


instance Catching Human Human where
	catch x y = (humanName x) ++ " catches " ++ (humanName y)

instance Catching Human Animal where
	catch x y = (humanName x) ++ " catches the " ++ (animalName y)

instance Catching Human Vehicle where
	catch x y = (humanName x) ++ " catches the " ++ (vehicleName y)



instance Catching Animal Human where
	catch x y = (animalName x) ++ " catches " ++ (humanName y)

instance Catching Animal Animal where
	catch x y = "the " ++ (animalName x) ++ " catches the " ++ (animalName y)



instance Catching Vehicle Vehicle where
	catch x y = "the " ++ (vehicleName x) ++ " catches the " ++ (vehicleName y)



--shlomo = Human {humanName = "Shlomo"}
--bus = Vehicle {vehicleName = "bus"}

--test1 = catch shlomo bus
--test2 = catch bus shlomo