module World where

import HRAS

import Prop

type Prop = String

data ModalOperator = Necessarily | Possibly deriving (Show, Eq)

data TemporalOperator = H | P | F | G deriving (Show, Eq)

data TProp = TProp { tempOp :: TemporalOperator,
     	     	     prop ::  Prop } deriving (Show, Eq)

data World = World { propositions :: [Prop] } deriving (Show, Eq)

w1 = World { propositions = [] }
w2 = World { propositions = [] }
w3 = World { propositions = [] }
w4 = World { propositions = [] }
w5 = World { propositions = [] }

model = [w1,w2,w3,w4,w5]

-- isValid :: TProp -> Bool

-- isSatisfiable :: TProp -> Bool

-- isSatisfied :: TProp -> World -> Bool

-- entailments :: Prop -> World -> [(World, Prop)]