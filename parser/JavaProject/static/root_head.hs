module VerbRoot where

-- determines the root of a given verb
-- e.g. root "ate" or root "have_eaten" both result in the output "eat"
root :: String -> String

-- these two verbs are used in unit tests but not given in the Brandeis Verb Lexicon, thus static here

root "smiled" = "smile"
root "have_smiled" = "smile"
root "has_smiled" = "smile"
root "smile" = "smile"
root "smiles" = "smile"
root "will_smile" = "smile"

root "shouted" = "shout"
root "have_shouted" = "shout"
root "has_shouted" = "shout"
root "shout" = "shout"
root "shouts" = "shout"
root "will_shout" = "shout"

