module Conjugations where

-- parameter: verb root
-- output: an array of conjugations of the given verb in the following order:
-- Past
-- Sg NotThird Perf
-- Sg Third Perf
-- Sg NotThird Pres
-- Sg Third Pres
-- Fut
-- sample output: ["ate", "have_eaten", "has_eaten", "eat", "eats", "will_eat"]
conjugations :: String -> [String]

-- these two verbs are used in unit tests but not given in the Brandeis Verb Lexicon, thus static here
conjugations "smile" = ["smiled", "have_smiled", "has_smiled", "smile", "smiles", "will_smile"]
conjugations "shout" = ["shouted", "have_shouted", "has_shouted", "shout", "shouts", "will_shout"]

