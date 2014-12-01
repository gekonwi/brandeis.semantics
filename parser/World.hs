module World where

import HRAS
import Conjugations
import Lexicon
import UnitTest
import P
import VerbRoot
import Data.List
import Data.Maybe


{-
=============================================================================================================
Disclaimer

We have one file with hundreds and two files with thousands of entries for various verbs (Conjugations.hs, 
VerbRoot.hs, and Lexicon.hs). They all work, however loading them all takes forever. Thus we commented out 
all the entries we don't use in our examples and unit tests. Please feel free to comment in any entries in 
VerbRoot.hs, Conjugations.hs, and Lexicon.hs.


1. Unit Tests

We provide a unit test suite in the module UnitTest and employ it in the World module for various functions.
After loading the World module in GHCI, run the command runWorldTests to see the results of each test, an
accumulation of each test suite at the end of it, as well as an accumulation of all test suites at the very
end.


2. Our understanding of tenses:

simple past:
	if true at a particular time x, the same event is true in present tense at time x or at some time 
	before x

present perfect:
	if true at a particular time x, the same event cannot be true in present tense at time x - it must be 
	true in present tense at some time before x

present:
	if true at a particular time x, trivially it must be true at x but we we don't know anything about the
	same event being true at any time other than x

future:
	if true at a particular time x, the same event cannot be true in present tense at time x - it must be 
	true in present tense at some time after x

=============================================================================================================
-}


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
	("amy catches"), ("tim has_averted"), ("they will_attest")] }

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


conjugPropTests = [

	("conjugProp 'i smiled' Past", conjugProp "i smiled" Past, "i smiled"),
	("conjugProp 'i smiled' Perf", conjugProp "i smiled" Perf, "i have_smiled"),
	("conjugProp 'i smiled' Pres", conjugProp "i smiled" Pres, "i smile"),
	("conjugProp 'i smiled' Fut",  conjugProp "i smiled" Fut, "i will_smile"),

	("conjugProp 'you have_shouted' Past", conjugProp "you have_shouted" Past, "you shouted"),
	("conjugProp 'you have_shouted' Perf", conjugProp "you have_shouted" Perf, "you have_shouted"),
	("conjugProp 'you have_shouted' Pres", conjugProp "you have_shouted" Pres, "you shout"),
	("conjugProp 'you have_shouted' Fut",  conjugProp "you have_shouted" Fut, "you will_shout"),

	("conjugProp 'the wizard will_eat the princess' Past", conjugProp "the wizard will_eat the princess" Past, "the wizard ate the princess"),
	("conjugProp 'the wizard will_eat the princess' Perf", conjugProp "the wizard will_eat the princess" Perf, "the wizard has_eaten the princess"),
	("conjugProp 'the wizard will_eat the princess' Pres", conjugProp "the wizard will_eat the princess" Pres, "the wizard eats the princess"),
	("conjugProp 'the wizard will_eat the princess' Fut",  conjugProp "the wizard will_eat the princess" Fut, "the wizard will_eat the princess"),

	("conjugProp 'they will_eat the princess' Past", conjugProp "they will_eat the princess" Past, "they ate the princess"),
	("conjugProp 'they will_eat the princess' Perf", conjugProp "they will_eat the princess" Perf, "they have_eaten the princess"),
	("conjugProp 'they will_eat the princess' Pres", conjugProp "they will_eat the princess" Pres, "they eat the princess"),
	("conjugProp 'they will_eat the princess' Fut",  conjugProp "they will_eat the princess" Fut, "they will_eat the princess"),

	("conjugProp 'the princess gives the wizard a boy' Past", conjugProp "the princess gives the wizard a boy" Past, "the princess gave the wizard a boy"),
	("conjugProp 'the princess gives the wizard a boy' Perf", conjugProp "the princess gives the wizard a boy" Perf, "the princess has_given the wizard a boy"),
	("conjugProp 'the princess gives the wizard a boy' Pres", conjugProp "the princess gives the wizard a boy" Pres, "the princess gives the wizard a boy"),
	("conjugProp 'the princess gives the wizard a boy' Fut",  conjugProp "the princess gives the wizard a boy" Fut, "the princess will_give the wizard a boy"),

	("conjugProp 'the giants give the dwarfs to a wizard' Past", conjugProp "the giants give the dwarfs to a wizard" Past, "the giants gave the dwarfs to a wizard"),
	("conjugProp 'the giants give the dwarfs to a wizard' Perf", conjugProp "the giants give the dwarfs to a wizard" Perf, "the giants have_given the dwarfs to a wizard"),
	("conjugProp 'the giants give the dwarfs to a wizard' Pres", conjugProp "the giants give the dwarfs to a wizard" Pres, "the giants give the dwarfs to a wizard"),
	("conjugProp 'the giants give the dwarfs to a wizard' Fut",  conjugProp "the giants give the dwarfs to a wizard" Fut,  "the giants will_give the dwarfs to a wizard")

	]




runWorldTests = runUnitTests [
	runTests conjugVerbTests,
	runTests conjugPropTests
	]

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
tp4 = TProp { tempOp = P, prop = "amy catches" }
tp5 = TProp { tempOp = G, prop = "they will_attest" }

-- quick tests (c&p)
	-- isSatisfied tp3 w3 
		-- False
	-- isSatisfied tp4 w3
		-- True
	-- isSatisfied tp2 w4
		-- False
	-- isSatisfied tp2 w5
		-- True
