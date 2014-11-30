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
