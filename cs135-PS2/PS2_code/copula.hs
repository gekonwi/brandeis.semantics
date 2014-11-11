{-# LANGUAGE MultiParamTypeClasses #-}

data Pronoun = Pronoun { pronoun :: String } deriving (Show)
data Adjective = Adjective { adjective :: String } deriving (Show)

class Copula x y where
      copula :: x -> y -> String

instance Copula Pronoun Pronoun where
         copula x y = "ExEy(" ++ (pronoun x) ++ "(x) ^ " ++ (pronoun y) ++ "(y) ^ " ++ "x=y)"

instance Copula Pronoun Adjective where
         copula x y = "Ex(" ++ (pronoun x) ++ "(x) ^ " ++ (adjective y) ++ "(x))"