module Prop where

import Conjugations

data Tense = Past | Perf | Pres | Fut

data Person = Third | NotThird

-- parameter: verb root, target person (assuming singular plurality), target tense
-- output: conjugated form of the given verb with target person (in Singular), and tense
conjugateVerb :: String -> Person -> Tense -> String

conjugateVerb verb _ Past = (conjugations verb)!!0
conjugateVerb verb NotThird Perf = (conjugations verb)!!1
conjugateVerb verb Third Perf = (conjugations verb)!!2
conjugateVerb verb NotThird Pres = (conjugations verb)!!3
conjugateVerb verb Third Pres = (conjugations verb)!!4
conjugateVerb verb _ Fut = (conjugations verb)!!5

