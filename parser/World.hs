module World where

import HRAS
import Prop
import Data.List
import Data.Maybe

type Prop = String

data ModalOperator = Necessarily | Possibly deriving (Show, Eq)

data TemporalOperator = H | P | F | G deriving (Show, Eq)

data TProp = TProp { tempOp :: TemporalOperator,
     	     	     prop ::  Prop } deriving (Show, Eq)

data World = World { propositions :: [Prop] } deriving (Show, Eq)

w1 = World { propositions = [("john eats"),
	("the wizard smiled"), ("georg will_sleep")] }
w2 = World { propositions = [("john eats"),
	("georg will_sleep")] }
w3 = World { propositions = [("john eats"),
	("amy catches"), ("georg will_sleep"), ("they attest")] }
w4 = World { propositions = [("john eats"),
	("georg will_sleep"), ("amy catches"), ("they will_attest")] }
w5 = World { propositions = [("john eats"),
	("amy catches"), ("tim has_averted"), ("they will_attest"))] }

model = [w1,w2,w3,w4,w5]

-- isValid :: TProp -> Bool

-- isSatisfiable :: TProp -> Bool

isSatisfied :: TProp -> World -> Bool
isSatisfied (TProp tempOp prop) world =
	case tempOp of
		H -> existsInAllWorldCollection prop (previousAndCurrentWorlds world)
		P -> existsInWorldCollection prop (previousAndCurrentWorlds world)
		F -> existsInWorldCollection prop (futureAndCurrentWorlds world)
		G -> existsInAllWorldCollection prop (futureAndCurrentWorlds world)

-- entailments :: Prop -> World -> [(World, Prop)]

previousAndCurrentWorlds :: World -> [World]
previousAndCurrentWorlds world = 
	take ((fromJust (elemIndex world model))+1) $ model

futureAndCurrentWorlds :: World -> [World]
futureAndCurrentWorlds world = 
	drop (fromJust (elemIndex world model)) . take 5 $ model

existsInWorld :: Prop -> World -> Bool
existsInWorld prop (World propositions) = 
	any (==prop) propositions

existsInWorldCollection :: Prop -> [World] -> Bool
existsInWorldCollection prop worlds = 
	any (==True) (map (existsInWorld prop) worlds)

existsInAllWorldCollection :: Prop -> [World] -> Bool
existsInAllWorldCollection prop worlds = 
	all (==True) (map (existsInWorld prop) worlds)

-- Test Data
tp1 = TProp { tempOp = P, prop = "john eats" }
tp2 = TProp { tempOp = F, prop = "tim has_averted" }
tp3 = TProp { tempOp = H, prop = "amy catches" }
tp4 = Tprop { tempOp = P, prop = "amy catches" }
tp5 = Tprop { tempOp = G, prop = "they will_attest" }

-- quick tests (c&p)
	-- isSatisfied tp3 w3 
		-- False
	-- isSatisfied tp4 w3
		-- True
	-- isSatisfied tp2 w4
		-- False
	-- isSatisfied tp2 w5
		-- True