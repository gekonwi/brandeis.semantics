class Catching a where
	catch :: a -> a -> String

data Agent = Human {name :: String} | Animal {name :: String} | Vehicle {name :: String}

instance Catching Agent where
	catch (Human x) (Human y) = x ++ " catches " ++ y
	catch (Human x) (Animal y) = x ++ " catches the " ++ y


--instance Catching Human Animal where
--	catch x y = (humanName x) ++ " catches the " ++ (animalName y)

--instance Catching Human Vehicle where
--	catch x y = (humanName x) ++ " catches the " ++ (vehicleName y)



--instance Catching Animal Human where
--	catch x y = (animalName x) ++ " catches " ++ (humanName y)

--instance Catching Animal Animal where
--	catch x y = "the " ++ (animalName x) ++ " catches the " ++ (animalName y)



--instance Catching Vehicle Vehicle where
--	catch x y = "the " ++ (vehicleName x) ++ " catches the " ++ (vehicleName y)