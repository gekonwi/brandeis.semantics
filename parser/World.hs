module World where

import HRAS
import Conjugations
import Lexicon
import UnitTest
import P
import VerbRoot

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
conjugVerb :: String -> Feat -> Feat -> Feat -> String

conjugVerb verb _ _ Past = (conjugations verb)!!0
conjugVerb verb Thrd Sg Perf = (conjugations verb)!!2
conjugVerb verb _ _ Perf = (conjugations verb)!!1
conjugVerb verb Thrd Sg Pres = (conjugations verb)!!4
conjugVerb verb _ _ Pres = (conjugations verb)!!3
conjugVerb verb _ _ Fut = (conjugations verb)!!5

conjugVerbTests = [
	("conjugVerb 'eat' Fst Sg Past",	conjugVerb "eat" Fst Sg Past,	"ate"),
	("conjugVerb 'eat' Fst Sg Perf",	conjugVerb "eat" Fst Sg Perf,	"have_eaten"),
	("conjugVerb 'eat' Fst Sg Pres",	conjugVerb "eat" Fst Sg Pres,	"eat"),
	("conjugVerb 'eat' Fst Sg Fut", 	conjugVerb "eat" Fst Sg Fut,		"will_eat"),

	("conjugVerb 'eat' Fst Pl Past",	conjugVerb "eat" Fst Pl Past,	"ate"),
	("conjugVerb 'eat' Fst Pl Perf",	conjugVerb "eat" Fst Pl Perf,	"have_eaten"),
	("conjugVerb 'eat' Fst Pl Pres",	conjugVerb "eat" Fst Pl Pres,	"eat"),
	("conjugVerb 'eat' Fst Pl Fut",	conjugVerb "eat" Fst Pl Fut,		"will_eat"),


	("conjugVerb 'eat' Snd Sg Past",	conjugVerb "eat" Snd Sg Past,	"ate"),
	("conjugVerb 'eat' Snd Sg Perf",	conjugVerb "eat" Snd Sg Perf,	"have_eaten"),
	("conjugVerb 'eat' Snd Sg Pres",	conjugVerb "eat" Snd Sg Pres,	"eat"),
	("conjugVerb 'eat' Snd Sg Fut",	conjugVerb "eat" Snd Sg Fut,		"will_eat"),
	
	("conjugVerb 'eat' Snd Pl Past",	conjugVerb "eat" Snd Pl Past,	"ate"),
	("conjugVerb 'eat' Snd Pl Perf",	conjugVerb "eat" Snd Pl Perf,	"have_eaten"),
	("conjugVerb 'eat' Snd Pl Pres",	conjugVerb "eat" Snd Pl Pres,	"eat"),
	("conjugVerb 'eat' Snd Pl Fut",	conjugVerb "eat" Snd Pl Fut,		"will_eat"),
	

	("conjugVerb 'eat' Thrd Sg Past",conjugVerb "eat" Thrd Sg Past,	"ate"),
	("conjugVerb 'eat' Thrd Sg Perf",conjugVerb "eat" Thrd Sg Perf,	"has_eaten"),
	("conjugVerb 'eat' Thrd Sg Pres",conjugVerb "eat" Thrd Sg Pres,	"eats"),
	("conjugVerb 'eat' Thrd Sg Fut",	conjugVerb "eat" Thrd Sg Fut,	"will_eat"),
	
	("conjugVerb 'eat' Thrd Pl Past",conjugVerb "eat" Thrd Pl Past,	"ate"),
	("conjugVerb 'eat' Thrd Pl Perf",conjugVerb "eat" Thrd Pl Perf,	"have_eaten"),
	("conjugVerb 'eat' Thrd Pl Pres",conjugVerb "eat" Thrd Pl Pres,	"eat"),
	("conjugVerb 'eat' Thrd Pl Fut",	conjugVerb "eat" Thrd Pl Fut,	"will_eat")
	]



-- parameters:
--		a proposition
--		target tense, one of [Past, Perf, Pres, Fut]
-- returns
--		the given proposition with the verb conjugated to the target tense
conjugProp :: Prop -> Feat -> String

conjugProp prop targetTense = wordsToString wordList
	
	where
		parseTree = head $ prs prop
		wordList = map conjugate $ map t2c $ filter isLeaf $ subtrees $ parseTree

		wordsToString (x:xs) = foldl (\ acc x -> acc ++ " " ++ x) x xs

		isLeaf (Leaf c) = True
		isLeaf _ = False

		conjugate x
			| catLabel x == "VP"	= conjugPhonVP x
			| otherwise				= phon x
		
			where
				conjugPhonVP x = conjugVerb verbRoot npPerson npNumber targetTense

					where
						npFs = fs $ t2c $ subtree parseTree [0]
						verbRoot = root $ phon x
						npPerson = head $ person $ npFs
						npNumber = head $ number $ npFs






runWorldTests = runUnitTests [
	runTests conjugVerbTests
	]

-- isValid :: TProp -> Bool

-- isSatisfiable :: TProp -> Bool

-- isSatisfied :: TProp -> World -> Bool

-- entailments :: Prop -> World -> [(World, Prop)]