-- An example file showing how to define, create, and access members from a custom datatype
-- For more on custom datatypes, review chapter 8 of Learn You A Haskell: http://learnyouahaskell.com/making-our-own-types-and-typeclasses

data DataType = DataType {
      param1 :: String,
      param2 :: Int,
      param3 :: Float } deriving (Show)

myData = DataType {param1 = "Data String",
       	           param2 = 1,
		   param3 = 0.142857}

-- try:
--	showData myData

showData :: DataType -> String
showData d = "Data is: {" ++ (param1 d) ++ ", " ++ show(param2 d) ++ ", " ++ show(param3 d) ++ "}"