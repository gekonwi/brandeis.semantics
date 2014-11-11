{-# LANGUAGE MultiParamTypeClasses #-}
--{-# LANGUAGE FlexibleInstances #-}

-- GL TYPES
data Type =	HUMN |		   -- human
			ANIM |		   -- animate
			ORGN | 		   -- organic
			ORGZ |		   -- organization
			PHYS |		   -- physical object
			ARTF |		   -- artifact
			EVNT |		   -- event
			PROP |		   -- proposition
			INFO |		   -- information
			SENS |		   -- sensation
			LOCA |		   -- location
			TIME |		   -- time period
			ATTD |		   -- attitude
			EMOT |		   -- emotion
			PPTY |		   -- property
			OBLG |		   -- obligation
			RULE 		   -- rule
			deriving (Show, Eq, Enum)

-- CUSTOM DATA TYPES
data Argument = Argument { ttype :: Type, value :: String } deriving (Show, Eq)

-- MODIFIED LOGIC compared to the provided template for this problem set:
-- each predicate can be constructed out of arguments or a mix of arguments and predicates,
-- e.g. [to boston], [fly john (to boston)], [want mary (fly john (to boston))]
-- therefore I added the list 'predicates' to each Predicate to keep this tree structure
data Predicate = Predicate { lemma :: String, arguments :: [Argument], predicates :: [Predicate]} deriving (Show, Eq)

type Context = [Argument]

-- CREATE SEMANTICALLY TYPED ARGUMENTS AS FOLLOWS
date :: String -> Argument
date s = Argument { ttype = TIME, value = s }

time :: String -> Argument
time s = Argument { ttype = TIME, value = s }

location :: String -> Argument
location s = Argument { ttype = LOCA, value = s }

human :: String -> Argument
human s = Argument { ttype = HUMN, value = s }

phys :: String -> Argument
phys s = Argument { ttype = PHYS, value = s }

artifact :: String -> Argument
artifact s = Argument { ttype = ARTF, value = s }

animate :: String -> Argument
animate s = Argument { ttype = ANIM, value = s }

organization :: String -> Argument
organization s = Argument { ttype = ORGZ, value = s }


-- CREATE ENTITIES/PPs AS FOLLOWS
may15 = date "May 15, 2014"
sevenAM = time "7:00"
sandiego = location "San Diego"
john = human "John"
mary = human "Mary"
boston = location "Boston"
ball = phys "ball"
car = artifact "car"
cat = animate "cat"
mouse = animate "mouse"
boy = human "the boy"
apple_inc = organization "Apple Inc."
google = organization "Google"
plane = artifact "the plane"

-- CONTEXT
-- you can consider this the "here and now": a list of facts ("is" predicates) that are true about the current state of the world

context = [
	may15,
	sevenAM,
	sandiego,
	john,
	mary,
	boston,
	ball,
	car,
	cat,
	mouse,
	boy,
	apple_inc,
	google,
	plane
	]

-- HELPER FUNCTIONS
getValue :: Argument -> String
getValue c = value c

getType :: Argument -> Type
getType c = ttype c

isType :: Argument -> Type -> Bool
isType c t = (ttype c == t)

hasPredicates :: Predicate -> Bool
hasPredicates x = (predicates x) /= []

moreThan1Argument :: Predicate -> Bool
moreThan1Argument x = length (arguments x) > 1


-- CREATE PREPOSITIONS AS FOLLOWS

preposition :: String -> Argument -> Predicate
preposition l a = Predicate { lemma = l, arguments = [a], predicates = [] }

from :: Argument -> Predicate
from x = preposition "from" x

to :: Argument -> Predicate
to x = preposition "to" x

at :: Argument -> Predicate
at x = preposition "at" x

on :: Argument -> Predicate
on x = preposition "on" x

in' :: Argument -> Predicate
in' x = preposition "in" x



-- CREATE VERBS AS FOLLOWS


-- extracts the common functionality of all verb creations
class ToVerb b where
	toVerb :: String -> Argument -> b -> Predicate

instance ToVerb Argument where
	toVerb l x y = Predicate { lemma = l, arguments = [x, y], predicates = [] }

-- MODIFIED LOGIC compared to the provided template for this problem set:
-- (see comment on the definition of Predicate above)
-- this way facts with different prepositions can be distinguished
-- e.g. [fly john (to boston)] vs. [fly john (from boston)]
-- also more complex predicates can be constructed, e.g. [want mary (fly john (to boston))]
instance ToVerb Predicate where
	toVerb l x y = Predicate { lemma = l, arguments = [x], predicates = [y] }




class Fly a b where
  fly :: a -> b -> Predicate

instance Fly Argument Argument where
  fly x y = toVerb "fly" x y

instance Fly Argument Predicate where
  fly x y = toVerb "fly" x y




class Catch a b where
	catch :: a -> b -> Predicate

instance Catch Argument Argument where
	catch x y = toVerb "catch" x y




class Throw a b where
	throw :: a -> b -> Predicate

instance Throw Argument Argument where
	throw x y = toVerb "throw" x y

instance Throw Argument Predicate where
	throw x y = toVerb "throw" x y




class Fall a b where
	fall :: a -> b -> Predicate

instance Fall Argument Predicate where
	fall x y = toVerb "fall" x y




class Work a b where
	work :: a -> b -> Predicate

instance Work Argument Predicate where
	work x y = toVerb "work" x y




class See a b where
	see :: a -> b -> Predicate

instance See Argument Predicate where
	see x y = toVerb "see" x y




class Wish a b where
	wish :: a -> b -> Predicate

instance Wish Argument Predicate where
	wish x y = toVerb "wish" x y






-- POPULATE THE KNOWLEDGE BASE AS FOLLOWS
facts =	[
	fly john (from sandiego),
	fly john (to boston),
	fly john plane,
	fly mary (from boston),
	fly mary (to sandiego),
	fly mary (at sevenAM),
	
	catch cat mouse,
	catch cat ball,
	catch john mary,
	
	throw boy ball,
	
	fall john (on mary),
	fall car (on john),
	fall plane (on car),
	fall mary (in' boston),
	fall mary (at sevenAM),
	
	work john (in' boston),
	work john (at apple_inc),
	work john (at sevenAM),
	work mary (in' sandiego),
	work mary (from boston),
	work mary (at google),
	work mary (at sevenAM),

	-- demonstrates the power of the modified predicate:
	-- "The boy wishes to see John catching Mary."
	wish boy $ see boy $ catch john mary
	]


-- implement functions that can answer the following questions about your fact base
-- where (e.g. qWhere fly john -- boston OR (to boston), depending on how you compute your answer)
-- who (e.g. qWho fly (to boston) -- john)
-- what (e.g. qWhat throw boy -- ball)
-- when (e.g. qWhen fly john -- 7:00 AM or at_7, a predicate representing at + 7:00 AM)

-- extra credit:
-- how (e.g. qHow john boston -- fly)
-- CREATE QUESTION-ANSWERING FUNCTIONS HERE

-- qWhere always asks for patient / object

whereTests = [	
	("qWhere fly john", qWhere fly john, [(from sandiego), (to boston)]),
	("qWhere fly mary", qWhere fly mary, [(from boston), (to sandiego)]),
	("qWhere fall mary", qWhere fall mary, [(in' boston)]),
	("qWhere work john", qWhere work john, [(in' boston), (at apple_inc)]),

	-- cats don't work and john just doesn't fall at all
	("qWhere work cat", qWhere work cat, []),
	("qWhere fall john", qWhere fall john, []),

	-- she lives (in boston), thus she (usually) works (remotely) (from boston),
	-- but her company is in sandiego, therefore sometimes she flies over to 
	-- and works (in sandiego) 
	("qWhere work mary", qWhere work mary, [(in' sandiego), (from boston), (at google)])
	]

qWhere :: (Argument -> Predicate -> Predicate) -> Argument -> [Predicate]
-- the location must be a predicate like (in boston) or (at google)
-- filtering the facts list avoids 'index too large' Exception for facts without predicates
-- TODO this is true for the defined verbs here, however this does not generally hold,
-- e.g. [love john boston] would have a location not being a predicate 
qWhere verb agent = [ patientPred |	fact <- filter hasPredicates facts,
	
	let patientPred = predicates fact !! 0,
	let patient = arguments (predicates fact !! 0) !! 0,
	
	getType patient `elem` [LOCA, ORGZ],
	fact == (verb agent patientPred)
	]




-- qWho is polymorphic ("qWho catch mouse" vs. "qWho work (at sevenAM)"),
-- always asks for agent (in contrary to "Whom")

whoTests = [
	("qWho fly plane", qWho fly plane, [john]),
	("qWho catch mouse", qWho catch mouse, [cat]),
	("qWho throw ball", qWho throw ball, [boy]),
	("qWho fall (in' boston)", qWho fall (in' boston), [mary]),
	("qWho fall (at sevenAM)", qWho fall (at sevenAM), [mary]),
	("qWho fall (on mary)", qWho fall (on mary), [john]),			
	("qWho work (at apple_inc)", qWho work (at apple_inc), [john]),
	("qWho work (at sevenAM)", qWho work (at sevenAM), [john, mary]),
	("qWho work (in' boston)", qWho work (in' boston), [john]),			
	
	-- john flies from sandiego to boston, mary the other way around
	-- questioning system should be aware of this difference
	("qWho fly (from sandiego)", qWho fly (from sandiego), [john]),			
	("qWho fly (to sandiego)", qWho fly (to sandiego), [mary]),			
	("qWho fly (from boston)", qWho fly (from boston), [mary]),			
	("qWho fly (to boston)", qWho fly (to boston), [john]),			

	-- inverted agent and patient
	("qWho catch john", qWho catch john, []),
	("qWho catch mary", qWho catch mary, [john]),

	-- no such facts given
	("qWho fall (in' sandiego)", qWho fall (in' sandiego), []),			
	
	-- questions with wrong prepositions, shouldn't find anything
	("qWho work (at boston)", qWho work (at boston), []),
	("qWho fall (in' mary)", qWho fall (in' mary), [])			
	]

class QWho b where
	qWho :: (Argument -> b -> Predicate) -> b -> [Argument]
	qWho verb x = [ agent | fact <- facts,
		let agent = arguments fact !! 0, 
		
		getType agent `elem` [HUMN, ANIM], 
		fact == (verb agent x)
		]

instance QWho Argument
instance QWho Predicate




-- qWhat is polymorphic ("qWhat throw boy" vs. "qWhat fall (on john)"), 
-- can ask for agent (What falls on John?) or object / patient (What does the boy throw?)
-- one fact could answer both variants, e.g. "The plane falls on the car." answers 
-- "the plane" to "What falls on car?" and "the car" to "What does the plane fall on?"

whatTests = [
	("qWhat fly john", qWhat fly john, [plane]),
	("qWhat fall (on john)", qWhat fall (on john), [car]),

	-- questions with wrong prepositions, shouldn't find anything
	("qWhat fly (on john)", qWhat fly (on john), []),
	
	-- using patient instead of agent
	("qWhat throw ball", qWhat throw ball, []),
	("qWhat throw boy", qWhat throw boy, [ball]),

	-- john falls on mary but john is not approprite for "what"
	("qWhat fall (on mary)", qWhat fall (on mary), []),			

	-- john catches mary but mary is not appropriate for "what"
	("qWhat catch john", qWhat catch john, []),			

	-- no such facts given
	("qWhat throw (to john)", qWhat throw (to john), []),			
	("qWhat fall (in' john)", qWhat fall (in' john), [])
	]

class QWhat b where
	qWhat :: (Argument -> b -> Predicate) -> b -> [Argument]

whatTypes :: [Type]
whatTypes = [ANIM, ORGN, ORGZ, PHYS, ARTF, EVNT, PROP, INFO, SENS, ATTD, EMOT, PPTY, OBLG, RULE]

instance QWhat Argument where
	-- due to the typing of this QWhat instance matching facts must have two arguments;
	-- the filtering is not necessary since this order of list comprehension arguments
	-- somehow lets Haskell avoid the 'index too large' Exception for facts with only
	-- one argument; however filtering here is safer and clearer code logic
	qWhat verb x = [ result | fact <- filter moreThan1Argument facts,
		let agent = arguments fact !! 0,
		let patient = arguments fact !! 1,		 
		let result = if x == agent then patient else agent,
		
		fact == (verb agent patient),
		(x == agent && getType patient `elem` whatTypes) 
			|| (x == patient && getType agent `elem` whatTypes)
		]

-- in case of qWhat :: (Argument -> Predicate -> Predicate) the result is always the agent
-- did not come up with any counter-example ...
-- e.g. [qWhat fall (on car)] should return "plane" from the fact [fall plane (on car)] 
instance QWhat Predicate where
	-- the patient must be a predicate like (on car) because of the typing of this QWhat instance;
	-- filtering the facts list avoids 'index too large' Exception for facts without predicates
	qWhat verb x = [ agent | fact <- filter hasPredicates facts,
		let agent = arguments fact !! 0,
		let patient = predicates fact !! 0,
		
		x == patient,
		getType agent `elem` whatTypes,
		fact == (verb agent x)
		]




--qHow is polymorphic ("qHow john (to boston)" vs. "qHow cat mouse")
--qHow always asks for the verb

howTests = [
	-- john flies from sandiego to boston, there is nothing for john and (from boston)
	("qHow john (from Boston)", qHow john (from boston), []),
	("qHow john (to Boston)", qHow john (to boston), ["fly"]),

	-- there are multiple things happening for mary from boston:
	-- she "flies from boston" and (usually) she "works from boston"
	("qHow mary (from Boston)", qHow mary (from boston), ["fly", "work"]),

	-- the boy just happens not to do anything with the mouse and vice versa
	("qHow boy mouse", qHow boy mouse, []),

	-- but the cat knows what to do with the mouse
	("qHow cat mouse", qHow cat mouse, ["catch"])
	]

class QHow b where
	qHow :: Argument -> b -> [String]

instance QHow Argument where
	-- due to the typing of this QHow instance matching facts must have two arguments;
	-- the filtering is not necessary since this order of list comprehension arguments
	-- somehow lets Haskell avoid the 'index too large' Exception for facts with only
	-- one argument; however filtering here is safer and clearer code logic
	qHow x y = [ lemma fact | fact <- filter moreThan1Argument facts,
		let agent = arguments fact !! 0,
		let patient = arguments fact !! 1,
		
		x == agent,
		y == patient
		]

instance QHow Predicate where
	-- due to the typing of this QHow instance matching facts must contain predicates;
	-- filtering facts accordingly avoids 'index too large' Exception
	qHow x y = [ lemma fact | fact <- filter hasPredicates facts,
		let agent = arguments fact !! 0,
		let patient = predicates fact !! 0,
		
		x == agent,
		y == patient
		]




-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- the rest of the file runs unit tests. after compiling the results can be seen by 
-- executing the function 'main' without arguments
-- ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


indent :: (Show a) => a -> String
indent x = "    " ++ (filter (\c -> c /= '"') $ show x)


sepLine :: String -> String
sepLine c = (concat $ take 60 $ repeat c)

runTests :: (Show a, Eq a) => [(String, [a], [a])] -> ([String], Int)
runTests tests = (concat details, errorCount)
	where
		details = [ oneTestDetails | (name, actual, expected) <- tests,
			let header h1 h2 = [sepLine "-", h1, indent name, h2],
			let passedHead = header "Test Passed:" "expected == actual:",
			let failedHead = (header ">>>>>>> TEST FAILED:" "expected:") 
				++ (map indent expected) ++ ["actual:"],
			let detailsHead = if actual == expected then passedHead else failedHead,
			let oneTestDetails = detailsHead ++ (map indent actual)
			]

		errorCount = sum [boolNum | (_, actual, expected) <- tests,
			let boolNum = if actual == expected then 0 else 1
			]


showErrorCount :: Int -> String
showErrorCount count = 
	if count == 0 
	then "All tests passed :D"
	else ">>>>>>> FAILED TESTS: " ++ show count


showTestResults :: [([String], Int)] -> [String]
showTestResults results = (concat lineGroups) ++ lastLines
	where 
		lineGroups = [ header ++ details ++ errorCount | r <- results,
			let header = ["", "", "Starting test suite:"],
			let details = map indent $ fst r,
			let errorCount = [(sepLine "="), showErrorCount $ snd r]
			]

		lastLines = 
			["", "", "", sepLine "=", "Result of all test suites:"]
			++ [showErrorCount $ sum [ snd r | r <- results]]
			++ [sepLine "=", "", ""]


main :: IO()
main = do
	-- can't put all test suites at once since they have different types
	mapM_ putStrLn $ showTestResults $ [
		runTests whereTests,
		runTests whoTests,
		runTests whatTests,
		runTests howTests
		]