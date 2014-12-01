module World where

import HRAS
import Conjugations
import Lexicon
import UnitTest

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


-- parameters:
-- 		verb root
--		target person (assuming singular plurality), one of [Fst, Snd, Thrd]
--		target number, one of [Sg, Pl]
--		target tense, one of [Past, Perf, Pres, Fut]
-- returns: 
--		conjugated form of the given verb with target person, target number, and target tense
conjugateVerb :: String -> Feat -> Feat -> Feat -> String

conjugateVerb verb _ _ Past = (conjugations verb)!!0
conjugateVerb verb Thrd Sg Perf = (conjugations verb)!!2
conjugateVerb verb _ _ Perf = (conjugations verb)!!1
conjugateVerb verb Thrd Sg Pres = (conjugations verb)!!4
conjugateVerb verb _ _ Pres = (conjugations verb)!!3
conjugateVerb verb _ _ Fut = (conjugations verb)!!5

conjugateVerbTests = [
	("conjugateVerb 'eat' Fst Sg Past",	conjugateVerb "eat" Fst Sg Past,	"ate"),
	("conjugateVerb 'eat' Fst Sg Perf",	conjugateVerb "eat" Fst Sg Perf,	"have_eaten"),
	("conjugateVerb 'eat' Fst Sg Pres",	conjugateVerb "eat" Fst Sg Pres,	"eat"),
	("conjugateVerb 'eat' Fst Sg Fut", 	conjugateVerb "eat" Fst Sg Fut,		"will_eat"),

	("conjugateVerb 'eat' Fst Pl Past",	conjugateVerb "eat" Fst Pl Past,	"ate"),
	("conjugateVerb 'eat' Fst Pl Perf",	conjugateVerb "eat" Fst Pl Perf,	"have_eaten"),
	("conjugateVerb 'eat' Fst Pl Pres",	conjugateVerb "eat" Fst Pl Pres,	"eat"),
	("conjugateVerb 'eat' Fst Pl Fut",	conjugateVerb "eat" Fst Pl Fut,		"will_eat"),


	("conjugateVerb 'eat' Snd Sg Past",	conjugateVerb "eat" Snd Sg Past,	"ate"),
	("conjugateVerb 'eat' Snd Sg Perf",	conjugateVerb "eat" Snd Sg Perf,	"have_eaten"),
	("conjugateVerb 'eat' Snd Sg Pres",	conjugateVerb "eat" Snd Sg Pres,	"eat"),
	("conjugateVerb 'eat' Snd Sg Fut",	conjugateVerb "eat" Snd Sg Fut,		"will_eat"),
	
	("conjugateVerb 'eat' Snd Pl Past",	conjugateVerb "eat" Snd Pl Past,	"ate"),
	("conjugateVerb 'eat' Snd Pl Perf",	conjugateVerb "eat" Snd Pl Perf,	"have_eaten"),
	("conjugateVerb 'eat' Snd Pl Pres",	conjugateVerb "eat" Snd Pl Pres,	"eat"),
	("conjugateVerb 'eat' Snd Pl Fut",	conjugateVerb "eat" Snd Pl Fut,		"will_eat"),
	

	("conjugateVerb 'eat' Thrd Sg Past",conjugateVerb "eat" Thrd Sg Past,	"ate"),
	("conjugateVerb 'eat' Thrd Sg Perf",conjugateVerb "eat" Thrd Sg Perf,	"has_eaten"),
	("conjugateVerb 'eat' Thrd Sg Pres",conjugateVerb "eat" Thrd Sg Pres,	"eats"),
	("conjugateVerb 'eat' Thrd Sg Fut",	conjugateVerb "eat" Thrd Sg Fut,	"will_eat"),
	
	("conjugateVerb 'eat' Thrd Pl Past",conjugateVerb "eat" Thrd Pl Past,	"ate"),
	("conjugateVerb 'eat' Thrd Pl Perf",conjugateVerb "eat" Thrd Pl Perf,	"have_eaten"),
	("conjugateVerb 'eat' Thrd Pl Pres",conjugateVerb "eat" Thrd Pl Pres,	"eat"),
	("conjugateVerb 'eat' Thrd Pl Fut",	conjugateVerb "eat" Thrd Pl Fut,	"will_eat")
	]


-- parameters:
--		a proposition
--		target tense, one of [Past, Perf, Pres, Fut]
-- returns
--		the given proposition with the verb conjugated to the target tense
--conjugateProp :: Prop -> Feat -> Prop




runWorldTests = runUnitTests [
	runTests conjugateVerbTests
	]

-- isValid :: TProp -> Bool

-- isSatisfiable :: TProp -> Bool

-- isSatisfied :: TProp -> World -> Bool

-- entailments :: Prop -> World -> [(World, Prop)]