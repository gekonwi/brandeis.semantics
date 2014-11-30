module Prop where

import Conjugations
import Lexicon

-- parameters:
-- 		verb root
--		target person (assuming singular plurality), one of [Fst, Snd, Thrd]
--		target tense, one of [Past, Perf, Pres, Fut]
-- returns: 
--		conjugated form of the given verb with target person (in Singular), and target tense
conjugateVerb :: String -> Feat -> Feat -> String

conjugateVerb verb _ Past = (conjugations verb)!!0
conjugateVerb verb Thrd Perf = (conjugations verb)!!2
conjugateVerb verb _ Perf = (conjugations verb)!!1
conjugateVerb verb Thrd Pres = (conjugations verb)!!4
conjugateVerb verb _ Pres = (conjugations verb)!!3
conjugateVerb verb _ Fut = (conjugations verb)!!5

