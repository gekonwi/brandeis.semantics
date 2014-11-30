module Lexicon where

type CatLabel = String
type Phon     = String

data Cat      = Cat Phon CatLabel Agreement [Cat]
              deriving Eq

type Agreement = [Feat]

data Feat = Masc  | Fem  | Neutr | MascOrFem 
          | Sg    | Pl 
          | Fst   | Snd  | Thrd 
          | Nom   | AccOrDat 
          | Pers  | Refl | Wh 
          | Past  | Pres | Fut   | Perf | Infl
          | About | At | By | For | From | In | Into 
          | Of | On | To | With | Against | Over 
          | Through | Onto | Upon | Like
          deriving (Eq,Show,Ord)

lexicon :: String -> [Cat]

lexicon "i"   = [Cat "i"   "NP" [Pers,Fst,Sg,Nom]        []]
lexicon "me"  = [Cat "me"  "NP" [Pers,Fst,Sg,AccOrDat]   []]
lexicon "we"  = [Cat "we"  "NP" [Pers,Fst,Pl,Nom]        []]
lexicon "us"  = [Cat "us"  "NP" [Pers,Fst,Pl,AccOrDat]   []]
lexicon "you" = [Cat "you" "NP" [Pers,Snd]               []]
lexicon "he"  = [Cat "he"  "NP" [Pers,Thrd,Sg,Nom,Masc]  []]
lexicon "him" = [Cat "him" "NP" [Pers,Thrd,Sg,AccOrDat,Masc] 
	      	     	    []]
lexicon "she" = [Cat "she" "NP" [Pers,Thrd,Sg,Nom,Fem]   []]
lexicon "her" = [Cat "her" "NP" [Pers,Thrd,Sg,AccOrDat,Fem] 
	      	     	    []]
lexicon "it"  = [Cat "it"  "NP" [Pers,Thrd,Sg,Neutr]     []]
lexicon "they" = [Cat "they" "NP" [Pers,Thrd,Pl,Nom]     []]
lexicon "them" = [Cat "them" "NP" [Pers,Thrd,Pl,AccOrDat] 
	       	      	      []]

lexicon "myself"     = 
 [Cat "myself"     "NP" [Refl,Sg,Fst,AccOrDat] []]
lexicon "ourselves"  = 
 [Cat "ourselves"  "NP" [Refl,Pl,Fst,AccOrDat] []]
lexicon "yourself"   = 
 [Cat "yourself"   "NP" [Refl,Sg,Snd,AccOrDat] []]
lexicon "yourselves" = 
 [Cat "yourselves" "NP" [Refl,Pl,Snd,AccOrDat] []]
lexicon "himself"    = 
 [Cat "himself"    "NP" [Refl,Sg,Thrd,AccOrDat,Masc]  []]
lexicon "herself"    = 
 [Cat "herself"    "NP" [Refl,Sg,Thrd,AccOrDat,Fem]   []]
lexicon "itself"     = 
 [Cat "itself"     "NP" [Refl,Sg,Thrd,AccOrDat,Neutr] []]
lexicon "themselves" = 
 [Cat "themselves" "NP" [Refl,Pl,Thrd,AccOrDat] []]

lexicon "who"     = [Cat "who" "NP"  [Wh,Thrd,MascOrFem] [], 
     Cat "who" "REL" [MascOrFem]         []]
lexicon "whom"    = 
 [Cat "whom" "NP"  [Sg,Wh,Thrd,AccOrDat,MascOrFem] [], 
  Cat "whom" "REL" [Sg,MascOrFem,AccOrDat]         []]
lexicon "what"    = 
 [Cat "what" "NP"  [Wh,Thrd,AccOrDat,Neutr]    []]
lexicon "that"    = [Cat "that"  "REL" []      [], 
                     Cat "that"  "DET" [Sg]    []]
lexicon "which"   = [Cat "which" "REL" [Neutr] [], 
                     Cat "which" "DET" [Wh]    []]

lexicon "snowwhite"    = 
 [Cat "snowwhite"  "NP" [Thrd,Fem,Sg]  []]
lexicon "alice"        = 
 [Cat "alice"      "NP" [Thrd,Fem,Sg]  []]
lexicon "dorothy"      = 
 [Cat "dorothy"    "NP" [Thrd,Fem,Sg]  []]
lexicon "goldilocks"   = 
 [Cat "goldilocks" "NP" [Thrd,Fem,Sg]  []]
lexicon "littlemook"   = 
 [Cat "littlemook" "NP" [Thrd,Masc,Sg] []]
lexicon "atreyu"       = 
 [Cat "atreyu"     "NP" [Thrd,Masc,Sg] []]

lexicon "every"   = [Cat "every"   "DET" [Sg]  []]
lexicon "all"     = [Cat "all"     "DET" [Pl]  []]
lexicon "some"    = [Cat "some"    "DET" []    []]
lexicon "several" = [Cat "several" "DET" [Pl]  []]
lexicon "a"       = [Cat "a"       "DET" [Sg]  []]
lexicon "no"      = [Cat "no"      "DET" []    []]
lexicon "the"     = [Cat "the"     "DET" []    []]

lexicon "most"    = [Cat "most"    "DET" [Pl]  []]
lexicon "many"    = [Cat "many"    "DET" [Pl]  []]
lexicon "few"     = [Cat "few"     "DET" [Pl]  []]
lexicon "this"    = [Cat "this"    "DET" [Sg]  []]
lexicon "these"   = [Cat "these"   "DET" [Pl]  []]
lexicon "those"   = [Cat "those"   "DET" [Pl]  []]

lexicon "less_than" = [Cat "less_than" "DF" [Pl] []]
lexicon "more_than" = [Cat "more_than" "DF" [Pl] []]

lexicon "thing"   = [Cat "thing"   "CN" [Sg,Neutr,Thrd] []]
lexicon "things"  = [Cat "things"  "CN" [Pl,Neutr,Thrd] []]
lexicon "person"  = [Cat "person"  "CN" [Sg,Masc,Thrd]  []]
lexicon "persons" = [Cat "persons" "CN" [Pl,Masc,Thrd]  []]
lexicon "boy"     = [Cat "boy"     "CN" [Sg,Masc,Thrd]  []]
lexicon "boys"    = [Cat "boys"    "CN" [Pl,Masc,Thrd]  []]
lexicon "man"     = [Cat "man"     "CN" [Sg,Masc,Thrd]  []]
lexicon "men"     = [Cat "men"     "CN" [Pl,Masc,Thrd]  []]
lexicon "girl"    = [Cat "girl"    "CN" [Sg,Fem,Thrd]   []]
lexicon "girls"   = [Cat "girls"   "CN" [Pl,Fem,Thrd]   []]
lexicon "woman"   = [Cat "woman"   "CN" [Sg,Fem,Thrd]   []]
lexicon "women"   = [Cat "women"   "CN" [Pl,Fem,Thrd]   []]
lexicon "princess" = [Cat "princess" "CN" [Sg,Fem,Thrd] []]
lexicon "princesses" = [Cat "princesses" "CN" [Pl,Fem,Thrd] []]
lexicon "dwarf"    = [Cat "dwarf"    "CN" [Sg,Masc,Thrd] []]
lexicon "dwarfs"   = [Cat "dwarfs"   "CN" [Pl,Masc,Thrd] []]
lexicon "dwarves"  = [Cat "dwarves"  "CN" [Pl,Masc,Thrd] []]
lexicon "giant"    = [Cat "giant"    "CN" [Sg,Masc,Thrd] []]
lexicon "giants"   = [Cat "giants"   "CN" [Pl,Masc,Thrd] []]

lexicon "wizard"   = [Cat "wizard"   "CN" [Sg,Masc,Thrd]  []]
lexicon "wizards"  = [Cat "wizards"  "CN" [Pl,Masc,Thrd]  []]
lexicon "sword"    = [Cat "sword"    "CN" [Sg,Neutr,Thrd] []]
lexicon "swords"   = [Cat "swords"   "CN" [Pl,Neutr,Thrd] []]
lexicon "dagger"   = [Cat "dagger"   "CN" [Sg,Neutr,Thrd] []]
lexicon "daggers"  = [Cat "daggers"  "CN" [Pl,Neutr,Thrd] []]

lexicon "did"    = [Cat "did"    "AUX" [] []]
lexicon "didn't" = [Cat "didn't" "AUX" [] []]

lexicon "smiled"    = [Cat "smiled"    "VP" [Past] []]
lexicon "smile"     = [Cat "smile"     "VP" [Infl]  []]
lexicon "laughed"   = [Cat "laughed"   "VP" [Past] []]
lexicon "laugh"     = [Cat "laugh"     "VP" [Infl]  []]
lexicon "cheered"   = [Cat "cheered"   "VP" [Past] []]
lexicon "cheer"     = [Cat "cheer"     "VP" [Infl]  []]
lexicon "shuddered" = [Cat "shuddered" "VP" [Past] []]
lexicon "shudder"   = [Cat "shudder"   "VP" [Infl]  []]

lexicon "loved"     = [Cat "loved"    "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "love"         = [Cat "love"     "VP" [Infl]  [Cat "_" "NP" [AccOrDat] []]]
                    
lexicon "admired"      = [Cat "admired"  "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "admire"       = [Cat "admire"   "VP" [Infl]  [Cat "_" "NP" [AccOrDat] []]]
                          
lexicon "helped"       = [Cat "helped"   "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "help"         = [Cat "help"     "VP" [Infl]  [Cat "_" "NP" [AccOrDat] []]]
                    
lexicon "defeated"     = [Cat "defeated" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]
lexicon "defeat"       = [Cat "defeat"   "VP" [Infl]  [Cat "_" "NP" [AccOrDat] []]]

lexicon "gave"         = 
 [Cat "gave" "VP" [Past]  [Cat "_" "NP" [AccOrDat] [],
                           Cat "_" "PP" [To]       []],
  Cat "gave" "VP" [Past]  [Cat "_" "NP" [AccOrDat] [],
                           Cat "_" "NP" [AccOrDat] []]]

lexicon "give"         = 
 [Cat "give" "VP" [Infl]  [Cat "_" "NP" [AccOrDat] [],
                           Cat "_" "PP" [To]       []],
  Cat "give" "VP" [Infl]  [Cat "_" "NP" [AccOrDat] [],
                           Cat "_" "NP" [AccOrDat] []]]

lexicon "sold" = 
 [Cat "sold" "VP" [Past] [Cat "_" "NP" [AccOrDat] [],
                           Cat "_" "PP" [To]       []],
  Cat "sold" "VP" [Past] [Cat "_" "NP" [AccOrDat] [],
                           Cat "_" "NP" [AccOrDat] []]]
lexicon "sell" = 
 [Cat "sell" "VP" [Infl]  [Cat "_" "NP" [AccOrDat] [],
                           Cat "_" "PP" [To]       []],
  Cat "sell" "VP" [Infl]  [Cat "_" "NP" [AccOrDat] [],
                           Cat "_" "NP" [AccOrDat] []]]

lexicon "kicked" = 
 [Cat "kicked" "VP" [Past] [Cat "_" "NP" [AccOrDat] [],
                             Cat "_" "PP" [With]     []], 
  Cat "kicked" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]

lexicon "kick" = 
 [Cat "kick"   "VP" [Infl]  [Cat "_" "NP" [AccOrDat] [],
                      Cat "_" "PP" [With]     []], 
  Cat "kick"   "VP" [Infl]  [Cat "_" "NP" [AccOrDat] []]] 

lexicon "took" = 
 [Cat "took" "VP" [Past] [Cat "_" "NP" [AccOrDat] [],
                    Cat "_" "PP" [From]     []], 
  Cat "took" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]]

lexicon "take" = 
 [Cat "take" "VP" [Infl]  [Cat "_" "NP" [AccOrDat] [],
                           Cat "_" "PP" [From]     []], 
  Cat "take" "VP" [Infl]  [Cat "_" "NP" [AccOrDat] []]] 

lexicon "about" = [Cat "about" "PREP" [About] []]
lexicon "at" = [Cat "at" "PREP" [At] []]
lexicon "by" = [Cat "by" "PREP" [By] []]
lexicon "for" = [Cat "for" "PREP" [For] []]
lexicon "from" = [Cat "from" "PREP" [From] []]
lexicon "in" = [Cat "in" "PREP" [In] []]
lexicon "into" = [Cat "into" "PREP" [Into] []]
lexicon "of" = [Cat "of" "PREP" [Of] []]
lexicon "on" = [Cat "on" "PREP" [On] []]
lexicon "to" = [Cat "to" "PREP" [To] []]
lexicon "with" = [Cat "with" "PREP" [With] []]
lexicon "against" = [Cat "against" "PREP" [Against] []]
lexicon "over" = [Cat "over" "PREP" [Over] []]
lexicon "through" = [Cat "through" "PREP" [Through] []]
lexicon "onto" = [Cat "onto" "PREP" [Onto] []]
lexicon "upon" = [Cat "upon" "PREP" [Upon] []]
lexicon "like" = [Cat "like" "PREP" [Like] []]

lexicon "and"   = [Cat "and"  "CONJ" [] []]
lexicon "."     = [Cat "."    "CONJ" [] []]
lexicon "if"    = [Cat "if"   "COND" [] []]
lexicon "then"  = [Cat "then" "THEN" [] []]

lexicon "shouted"    = [Cat "shouted"    "VP" [Past] []]
lexicon "shout"    = [
  Cat "shout"    "VP" [Pres,Sg,Fst] [],
	Cat "shout"    "VP" [Pres,Sg,Snd] [],
	Cat "shout"    "VP" [Pres,Pl]  [],
	Cat "shout"    "VP" [Infl]  []]
lexicon "shouts"    = [Cat "shouts"    "VP" [Pres,Sg,Thrd] []]
lexicon "will_shout"    = [Cat "will_shout"    "VP" [Fut] []]
lexicon "have_shouted"	= [
  Cat "have_shouted"  "VP" [Perf,Sg,Fst] [],
	Cat "have_shouted"  "VP" [Perf,Sg,Snd] [],
	Cat "have_shouted"  "VP" [Perf,Pl] []]
lexicon "has_shouted"   = [Cat "has_shouted"   "VP" [Perf,Sg,Thrd] []]



lexicon "abandon" = [
  Cat "abandon" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "abandon" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "abandon" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "abandon" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []],
  Cat "abandon" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "abandon" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "abandon" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "abandon" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "abandons" = [
  Cat "abandons" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []],
  Cat "abandons" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "abandoned" = [
  Cat "abandoned" "VP" [Past] [Cat "_" "NP" [AccOrDat] []],
  Cat "abandoned" "VP" [Past] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "have_abandoned" = [
  Cat "have_abandoned" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_abandoned" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_abandoned" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_abandoned" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "have_abandoned" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "have_abandoned" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "has_abandoned" = [
  Cat "has_abandoned" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []],
  Cat "has_abandoned" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "will_abandon" = [
  Cat "will_abandon" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []],
  Cat "will_abandon" "VP" [Fut] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]

lexicon "abase" = [
  Cat "abase" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "abase" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "abase" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "abase" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "abases" = [
  Cat "abases" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "abased" = [
  Cat "abased" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_abased" = [
  Cat "have_abased" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_abased" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_abased" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_abased" = [
  Cat "has_abased" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_abase" = [
  Cat "will_abase" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "abbreviate" = [
  Cat "abbreviate" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "abbreviate" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "abbreviate" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "abbreviate" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []],
  Cat "abbreviate" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "abbreviate" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "abbreviate" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "abbreviate" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "abbreviates" = [
  Cat "abbreviates" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []],
  Cat "abbreviates" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "abbreviated" = [
  Cat "abbreviated" "VP" [Past] [Cat "_" "NP" [AccOrDat] []],
  Cat "abbreviated" "VP" [Past] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "have_abbreviated" = [
  Cat "have_abbreviated" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_abbreviated" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_abbreviated" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_abbreviated" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "have_abbreviated" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "have_abbreviated" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "has_abbreviated" = [
  Cat "has_abbreviated" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []],
  Cat "has_abbreviated" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "will_abbreviate" = [
  Cat "will_abbreviate" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []],
  Cat "will_abbreviate" "VP" [Fut] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]

lexicon "abhor" = [
  Cat "abhor" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "abhor" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "abhor" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "abhor" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "abhors" = [
  Cat "abhors" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "abhored" = [
  Cat "abhored" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_abhored" = [
  Cat "have_abhored" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_abhored" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_abhored" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_abhored" = [
  Cat "has_abhored" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_abhor" = [
  Cat "will_abhor" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "abide" = [
  Cat "abide" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [By] []],
  Cat "abide" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [By] []],
  Cat "abide" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [By] []],
  Cat "abide" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [By] []]
  ]
lexicon "abides" = [
  Cat "abides" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [By] []]
  ]
lexicon "abided" = [
  Cat "abided" "VP" [Past] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [By] []]
  ]
lexicon "have_abided" = [
  Cat "have_abided" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [By] []],
  Cat "have_abided" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [By] []],
  Cat "have_abided" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [By] []]
  ]
lexicon "has_abided" = [
  Cat "has_abided" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [By] []]
  ]
lexicon "will_abide" = [
  Cat "will_abide" "VP" [Fut] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [By] []]
  ]

lexicon "abolish" = [
  Cat "abolish" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "abolish" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "abolish" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "abolish" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "abolishes" = [
  Cat "abolishes" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "abolished" = [
  Cat "abolished" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_abolished" = [
  Cat "have_abolished" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_abolished" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_abolished" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_abolished" = [
  Cat "has_abolished" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_abolish" = [
  Cat "will_abolish" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "absorb" = [
  Cat "absorb" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "absorb" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "absorb" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "absorb" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "absorbs" = [
  Cat "absorbs" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "absorbed" = [
  Cat "absorbed" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_absorbed" = [
  Cat "have_absorbed" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_absorbed" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_absorbed" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_absorbed" = [
  Cat "has_absorbed" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_absorb" = [
  Cat "will_absorb" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "abstain" = [
  Cat "abstain" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [From] []],
  Cat "abstain" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [From] []],
  Cat "abstain" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [From] []],
  Cat "abstain" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [From] []]
  ]
lexicon "abstains" = [
  Cat "abstains" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [From] []]
  ]
lexicon "abstained" = [
  Cat "abstained" "VP" [Past] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [From] []]
  ]
lexicon "have_abstained" = [
  Cat "have_abstained" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [From] []],
  Cat "have_abstained" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [From] []],
  Cat "have_abstained" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [From] []]
  ]
lexicon "has_abstained" = [
  Cat "has_abstained" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [From] []]
  ]
lexicon "will_abstain" = [
  Cat "will_abstain" "VP" [Fut] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [From] []]
  ]

lexicon "abstract" = [
  Cat "abstract" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "abstract" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "abstract" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "abstract" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "abstracts" = [
  Cat "abstracts" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "abstracted" = [
  Cat "abstracted" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_abstracted" = [
  Cat "have_abstracted" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_abstracted" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_abstracted" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_abstracted" = [
  Cat "has_abstracted" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_abstract" = [
  Cat "will_abstract" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "abuse" = [
  Cat "abuse" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "abuse" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "abuse" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "abuse" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "abuses" = [
  Cat "abuses" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "abused" = [
  Cat "abused" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_abused" = [
  Cat "have_abused" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_abused" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_abused" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_abused" = [
  Cat "has_abused" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_abuse" = [
  Cat "will_abuse" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "accept" = [
  Cat "accept" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "accept" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "accept" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "accept" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "accepts" = [
  Cat "accepts" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "accepted" = [
  Cat "accepted" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_accepted" = [
  Cat "have_accepted" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_accepted" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_accepted" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_accepted" = [
  Cat "has_accepted" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_accept" = [
  Cat "will_accept" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "acclaim" = [
  Cat "acclaim" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "acclaim" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "acclaim" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "acclaim" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "acclaims" = [
  Cat "acclaims" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "acclaimed" = [
  Cat "acclaimed" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_acclaimed" = [
  Cat "have_acclaimed" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_acclaimed" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_acclaimed" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_acclaimed" = [
  Cat "has_acclaimed" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_acclaim" = [
  Cat "will_acclaim" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "accomodate" = [
  Cat "accomodate" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "accomodate" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "accomodate" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "accomodate" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "accomodates" = [
  Cat "accomodates" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "accomodated" = [
  Cat "accomodated" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_accomodated" = [
  Cat "have_accomodated" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_accomodated" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_accomodated" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_accomodated" = [
  Cat "has_accomodated" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_accomodate" = [
  Cat "will_accomodate" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "accompany" = [
  Cat "accompany" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "accompany" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "accompany" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "accompany" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "accompanies" = [
  Cat "accompanies" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "accompanied" = [
  Cat "accompanied" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_accompanied" = [
  Cat "have_accompanied" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_accompanied" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_accompanied" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_accompanied" = [
  Cat "has_accompanied" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_accompany" = [
  Cat "will_accompany" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "accomplish" = [
  Cat "accomplish" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "accomplish" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "accomplish" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "accomplish" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "accomplishes" = [
  Cat "accomplishes" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "accomplished" = [
  Cat "accomplished" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_accomplished" = [
  Cat "have_accomplished" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_accomplished" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_accomplished" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_accomplished" = [
  Cat "has_accomplished" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_accomplish" = [
  Cat "will_accomplish" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "account" = [
  Cat "account" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "account" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "account" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "account" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []],
  Cat "account" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [For] []],
  Cat "account" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [For] []],
  Cat "account" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [For] []],
  Cat "account" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [For] []]
  ]
lexicon "accounts" = [
  Cat "accounts" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []],
  Cat "accounts" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [For] []]
  ]
lexicon "accounted" = [
  Cat "accounted" "VP" [Past] [Cat "_" "NP" [AccOrDat] []],
  Cat "accounted" "VP" [Past] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [For] []]
  ]
lexicon "have_accounted" = [
  Cat "have_accounted" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_accounted" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_accounted" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_accounted" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [For] []],
  Cat "have_accounted" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [For] []],
  Cat "have_accounted" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [For] []]
  ]
lexicon "has_accounted" = [
  Cat "has_accounted" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []],
  Cat "has_accounted" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [For] []]
  ]
lexicon "will_account" = [
  Cat "will_account" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []],
  Cat "will_account" "VP" [Fut] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [For] []]
  ]

lexicon "accuse" = [
  Cat "accuse" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "accuse" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "accuse" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "accuse" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "accuses" = [
  Cat "accuses" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "accused" = [
  Cat "accused" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_accused" = [
  Cat "have_accused" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_accused" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_accused" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_accused" = [
  Cat "has_accused" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_accuse" = [
  Cat "will_accuse" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "achieve" = [
  Cat "achieve" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "achieve" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "achieve" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "achieve" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "achieves" = [
  Cat "achieves" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "achieved" = [
  Cat "achieved" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_achieved" = [
  Cat "have_achieved" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_achieved" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_achieved" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_achieved" = [
  Cat "has_achieved" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_achieve" = [
  Cat "will_achieve" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "acknowledge" = [
  Cat "acknowledge" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "acknowledge" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "acknowledge" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "acknowledge" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "acknowledges" = [
  Cat "acknowledges" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "acknowledged" = [
  Cat "acknowledged" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_acknowledged" = [
  Cat "have_acknowledged" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_acknowledged" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_acknowledged" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_acknowledged" = [
  Cat "has_acknowledged" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_acknowledge" = [
  Cat "will_acknowledge" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "acquire" = [
  Cat "acquire" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "acquire" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "acquire" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "acquire" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "acquires" = [
  Cat "acquires" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "acquired" = [
  Cat "acquired" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_acquired" = [
  Cat "have_acquired" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_acquired" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_acquired" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_acquired" = [
  Cat "has_acquired" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_acquire" = [
  Cat "will_acquire" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "act" = [
  Cat "act" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Like] []],
  Cat "act" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Like] []],
  Cat "act" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Like] []],
  Cat "act" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Like] []]
  ]
lexicon "acts" = [
  Cat "acts" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Like] []]
  ]
lexicon "acted" = [
  Cat "acted" "VP" [Past] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Like] []]
  ]
lexicon "have_acted" = [
  Cat "have_acted" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Like] []],
  Cat "have_acted" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Like] []],
  Cat "have_acted" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Like] []]
  ]
lexicon "has_acted" = [
  Cat "has_acted" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Like] []]
  ]
lexicon "will_act" = [
  Cat "will_act" "VP" [Fut] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Like] []]
  ]

lexicon "adapt" = [
  Cat "adapt" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "adapt" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "adapt" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "adapt" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "adapts" = [
  Cat "adapts" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "adapted" = [
  Cat "adapted" "VP" [Past] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "have_adapted" = [
  Cat "have_adapted" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "have_adapted" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "have_adapted" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "has_adapted" = [
  Cat "has_adapted" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "will_adapt" = [
  Cat "will_adapt" "VP" [Fut] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]

lexicon "add" = [
  Cat "add" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "add" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "add" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "add" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []],
  Cat "add" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "add" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "add" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "add" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "adds" = [
  Cat "adds" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []],
  Cat "adds" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "added" = [
  Cat "added" "VP" [Past] [Cat "_" "NP" [AccOrDat] []],
  Cat "added" "VP" [Past] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "have_added" = [
  Cat "have_added" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_added" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_added" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_added" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "have_added" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "have_added" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "has_added" = [
  Cat "has_added" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []],
  Cat "has_added" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "will_add" = [
  Cat "will_add" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []],
  Cat "will_add" "VP" [Fut] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]

lexicon "address" = [
  Cat "address" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "address" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "address" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "address" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []],
  Cat "address" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "address" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "address" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "address" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "addresses" = [
  Cat "addresses" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []],
  Cat "addresses" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "addressed" = [
  Cat "addressed" "VP" [Past] [Cat "_" "NP" [AccOrDat] []],
  Cat "addressed" "VP" [Past] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "have_addressed" = [
  Cat "have_addressed" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_addressed" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_addressed" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_addressed" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "have_addressed" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "have_addressed" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "has_addressed" = [
  Cat "has_addressed" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []],
  Cat "has_addressed" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "will_address" = [
  Cat "will_address" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []],
  Cat "will_address" "VP" [Fut] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]

lexicon "adhere" = [
  Cat "adhere" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "adhere" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "adhere" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "adhere" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "adheres" = [
  Cat "adheres" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "adhered" = [
  Cat "adhered" "VP" [Past] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "have_adhered" = [
  Cat "have_adhered" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "have_adhered" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "have_adhered" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "has_adhered" = [
  Cat "has_adhered" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "will_adhere" = [
  Cat "will_adhere" "VP" [Fut] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]

lexicon "adjourn" = [
  Cat "adjourn" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "adjourn" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "adjourn" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "adjourn" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []],
  Cat "adjourn" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "adjourn" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "adjourn" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "adjourn" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "adjourns" = [
  Cat "adjourns" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []],
  Cat "adjourns" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "adjourned" = [
  Cat "adjourned" "VP" [Past] [Cat "_" "NP" [AccOrDat] []],
  Cat "adjourned" "VP" [Past] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "have_adjourned" = [
  Cat "have_adjourned" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_adjourned" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_adjourned" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_adjourned" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "have_adjourned" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "have_adjourned" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "has_adjourned" = [
  Cat "has_adjourned" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []],
  Cat "has_adjourned" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "will_adjourn" = [
  Cat "will_adjourn" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []],
  Cat "will_adjourn" "VP" [Fut] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]

lexicon "admire" = [
  Cat "admire" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "admire" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "admire" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "admire" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "admires" = [
  Cat "admires" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "admired" = [
  Cat "admired" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_admired" = [
  Cat "have_admired" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_admired" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_admired" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_admired" = [
  Cat "has_admired" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_admire" = [
  Cat "will_admire" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "admonish" = [
  Cat "admonish" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "admonish" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "admonish" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "admonish" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "admonishes" = [
  Cat "admonishes" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "admonished" = [
  Cat "admonished" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_admonished" = [
  Cat "have_admonished" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_admonished" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_admonished" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_admonished" = [
  Cat "has_admonished" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_admonish" = [
  Cat "will_admonish" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "adopt" = [
  Cat "adopt" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "adopt" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "adopt" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "adopt" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "adopts" = [
  Cat "adopts" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "adopted" = [
  Cat "adopted" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_adopted" = [
  Cat "have_adopted" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_adopted" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_adopted" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_adopted" = [
  Cat "has_adopted" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_adopt" = [
  Cat "will_adopt" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "adore" = [
  Cat "adore" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "adore" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "adore" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "adore" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "adores" = [
  Cat "adores" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "adored" = [
  Cat "adored" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_adored" = [
  Cat "have_adored" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_adored" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_adored" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_adored" = [
  Cat "has_adored" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_adore" = [
  Cat "will_adore" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "advance" = [
  Cat "advance" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "advance" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "advance" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "advance" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "advances" = [
  Cat "advances" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "advanced" = [
  Cat "advanced" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_advanced" = [
  Cat "have_advanced" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_advanced" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_advanced" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_advanced" = [
  Cat "has_advanced" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_advance" = [
  Cat "will_advance" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "advertise" = [
  Cat "advertise" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "advertise" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "advertise" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "advertise" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []],
  Cat "advertise" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [For] []],
  Cat "advertise" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [For] []],
  Cat "advertise" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [For] []],
  Cat "advertise" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [For] []]
  ]
lexicon "advertises" = [
  Cat "advertises" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []],
  Cat "advertises" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [For] []]
  ]
lexicon "advertised" = [
  Cat "advertised" "VP" [Past] [Cat "_" "NP" [AccOrDat] []],
  Cat "advertised" "VP" [Past] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [For] []]
  ]
lexicon "have_advertised" = [
  Cat "have_advertised" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_advertised" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_advertised" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_advertised" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [For] []],
  Cat "have_advertised" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [For] []],
  Cat "have_advertised" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [For] []]
  ]
lexicon "has_advertised" = [
  Cat "has_advertised" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []],
  Cat "has_advertised" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [For] []]
  ]
lexicon "will_advertise" = [
  Cat "will_advertise" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []],
  Cat "will_advertise" "VP" [Fut] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [For] []]
  ]

lexicon "advise" = [
  Cat "advise" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "advise" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "advise" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "advise" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "advises" = [
  Cat "advises" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "advised" = [
  Cat "advised" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_advised" = [
  Cat "have_advised" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_advised" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_advised" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_advised" = [
  Cat "has_advised" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_advise" = [
  Cat "will_advise" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "advocate" = [
  Cat "advocate" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "advocate" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "advocate" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "advocate" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "advocates" = [
  Cat "advocates" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "advocated" = [
  Cat "advocated" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_advocated" = [
  Cat "have_advocated" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_advocated" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_advocated" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_advocated" = [
  Cat "has_advocated" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_advocate" = [
  Cat "will_advocate" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "affect" = [
  Cat "affect" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "affect" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "affect" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "affect" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "affects" = [
  Cat "affects" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "affected" = [
  Cat "affected" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_affected" = [
  Cat "have_affected" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_affected" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_affected" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_affected" = [
  Cat "has_affected" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_affect" = [
  Cat "will_affect" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "afford" = [
  Cat "afford" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "afford" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "afford" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "afford" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "affords" = [
  Cat "affords" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "afforded" = [
  Cat "afforded" "VP" [Past] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_afforded" = [
  Cat "have_afforded" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "have_afforded" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "have_afforded" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_afforded" = [
  Cat "has_afforded" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_afford" = [
  Cat "will_afford" "VP" [Fut] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "affront" = [
  Cat "affront" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "affront" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "affront" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "affront" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "affronts" = [
  Cat "affronts" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "affronted" = [
  Cat "affronted" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_affronted" = [
  Cat "have_affronted" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_affronted" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_affronted" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_affronted" = [
  Cat "has_affronted" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_affront" = [
  Cat "will_affront" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "aid" = [
  Cat "aid" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "aid" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "aid" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "aid" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "aids" = [
  Cat "aids" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "aided" = [
  Cat "aided" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_aided" = [
  Cat "have_aided" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_aided" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_aided" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_aided" = [
  Cat "has_aided" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_aid" = [
  Cat "will_aid" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "aim" = [
  Cat "aim" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "aim" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "aim" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "aim" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []],
  Cat "aim" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "aim" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []],
  Cat "aim" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Onto] []],
  Cat "aim" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [In] []],
  Cat "aim" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Into] []],
  Cat "aim" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "aim" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []],
  Cat "aim" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Onto] []],
  Cat "aim" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [In] []],
  Cat "aim" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Into] []],
  Cat "aim" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "aim" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []],
  Cat "aim" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Onto] []],
  Cat "aim" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [In] []],
  Cat "aim" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Into] []],
  Cat "aim" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "aim" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []],
  Cat "aim" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Onto] []],
  Cat "aim" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [In] []],
  Cat "aim" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Into] []]
  ]
lexicon "aims" = [
  Cat "aims" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []],
  Cat "aims" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "aims" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []],
  Cat "aims" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Onto] []],
  Cat "aims" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [In] []],
  Cat "aims" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Into] []]
  ]
lexicon "aimed" = [
  Cat "aimed" "VP" [Past] [Cat "_" "NP" [AccOrDat] []],
  Cat "aimed" "VP" [Past] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "aimed" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []],
  Cat "aimed" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Onto] []],
  Cat "aimed" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [In] []],
  Cat "aimed" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Into] []]
  ]
lexicon "have_aimed" = [
  Cat "have_aimed" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_aimed" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_aimed" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_aimed" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "have_aimed" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []],
  Cat "have_aimed" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Onto] []],
  Cat "have_aimed" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [In] []],
  Cat "have_aimed" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Into] []],
  Cat "have_aimed" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "have_aimed" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []],
  Cat "have_aimed" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Onto] []],
  Cat "have_aimed" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [In] []],
  Cat "have_aimed" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Into] []],
  Cat "have_aimed" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "have_aimed" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []],
  Cat "have_aimed" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Onto] []],
  Cat "have_aimed" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [In] []],
  Cat "have_aimed" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Into] []]
  ]
lexicon "has_aimed" = [
  Cat "has_aimed" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []],
  Cat "has_aimed" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "has_aimed" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []],
  Cat "has_aimed" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Onto] []],
  Cat "has_aimed" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [In] []],
  Cat "has_aimed" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Into] []]
  ]
lexicon "will_aim" = [
  Cat "will_aim" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []],
  Cat "will_aim" "VP" [Fut] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "will_aim" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []],
  Cat "will_aim" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Onto] []],
  Cat "will_aim" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [In] []],
  Cat "will_aim" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Into] []]
  ]

lexicon "alarm" = [
  Cat "alarm" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "alarm" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "alarm" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "alarm" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "alarms" = [
  Cat "alarms" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "alarmed" = [
  Cat "alarmed" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_alarmed" = [
  Cat "have_alarmed" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_alarmed" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_alarmed" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_alarmed" = [
  Cat "has_alarmed" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_alarm" = [
  Cat "will_alarm" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "alert" = [
  Cat "alert" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "alert" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "alert" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "alert" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "alerts" = [
  Cat "alerts" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "alerted" = [
  Cat "alerted" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_alerted" = [
  Cat "have_alerted" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_alerted" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_alerted" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_alerted" = [
  Cat "has_alerted" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_alert" = [
  Cat "will_alert" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "allocate" = [
  Cat "allocate" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "allocate" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "allocate" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "allocate" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []],
  Cat "allocate" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "allocate" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "allocate" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "allocate" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "allocates" = [
  Cat "allocates" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []],
  Cat "allocates" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "allocated" = [
  Cat "allocated" "VP" [Past] [Cat "_" "NP" [AccOrDat] []],
  Cat "allocated" "VP" [Past] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "have_allocated" = [
  Cat "have_allocated" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_allocated" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_allocated" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_allocated" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "have_allocated" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "have_allocated" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "has_allocated" = [
  Cat "has_allocated" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []],
  Cat "has_allocated" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "will_allocate" = [
  Cat "will_allocate" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []],
  Cat "will_allocate" "VP" [Fut] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]

lexicon "allow" = [
  Cat "allow" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "allow" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "allow" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "allow" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "allow" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [For] []],
  Cat "allow" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [For] []],
  Cat "allow" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [For] []],
  Cat "allow" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [For] []]
  ]
lexicon "allows" = [
  Cat "allows" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "allows" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [For] []]
  ]
lexicon "allowed" = [
  Cat "allowed" "VP" [Past] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "allowed" "VP" [Past] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [For] []]
  ]
lexicon "have_allowed" = [
  Cat "have_allowed" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "have_allowed" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "have_allowed" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "have_allowed" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [For] []],
  Cat "have_allowed" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [For] []],
  Cat "have_allowed" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [For] []]
  ]
lexicon "has_allowed" = [
  Cat "has_allowed" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "has_allowed" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [For] []]
  ]
lexicon "will_allow" = [
  Cat "will_allow" "VP" [Fut] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "will_allow" "VP" [Fut] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [For] []]
  ]

lexicon "amaze" = [
  Cat "amaze" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "amaze" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "amaze" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "amaze" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "amazes" = [
  Cat "amazes" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "amazed" = [
  Cat "amazed" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_amazed" = [
  Cat "have_amazed" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_amazed" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_amazed" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_amazed" = [
  Cat "has_amazed" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_amaze" = [
  Cat "will_amaze" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "amuse" = [
  Cat "amuse" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "amuse" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "amuse" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "amuse" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "amuses" = [
  Cat "amuses" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "amused" = [
  Cat "amused" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_amused" = [
  Cat "have_amused" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_amused" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_amused" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_amused" = [
  Cat "has_amused" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_amuse" = [
  Cat "will_amuse" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "analyze" = [
  Cat "analyze" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "analyze" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "analyze" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "analyze" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "analyzes" = [
  Cat "analyzes" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "analyzed" = [
  Cat "analyzed" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_analyzed" = [
  Cat "have_analyzed" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_analyzed" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_analyzed" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_analyzed" = [
  Cat "has_analyzed" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_analyze" = [
  Cat "will_analyze" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "anger" = [
  Cat "anger" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "anger" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "anger" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "anger" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "angers" = [
  Cat "angers" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "angered" = [
  Cat "angered" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_angered" = [
  Cat "have_angered" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_angered" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_angered" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_angered" = [
  Cat "has_angered" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_anger" = [
  Cat "will_anger" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "announce" = [
  Cat "announce" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "announce" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "announce" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "announce" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "announces" = [
  Cat "announces" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "announced" = [
  Cat "announced" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_announced" = [
  Cat "have_announced" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_announced" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_announced" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_announced" = [
  Cat "has_announced" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_announce" = [
  Cat "will_announce" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "annoy" = [
  Cat "annoy" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "annoy" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "annoy" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "annoy" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "annoys" = [
  Cat "annoys" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "annoyed" = [
  Cat "annoyed" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_annoyed" = [
  Cat "have_annoyed" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_annoyed" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_annoyed" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_annoyed" = [
  Cat "has_annoyed" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_annoy" = [
  Cat "will_annoy" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "answer" = [
  Cat "answer" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "answer" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "answer" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "answer" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "answers" = [
  Cat "answers" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "answered" = [
  Cat "answered" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_answered" = [
  Cat "have_answered" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_answered" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_answered" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_answered" = [
  Cat "has_answered" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_answer" = [
  Cat "will_answer" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "anticipate" = [
  Cat "anticipate" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "anticipate" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "anticipate" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "anticipate" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "anticipates" = [
  Cat "anticipates" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "anticipated" = [
  Cat "anticipated" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_anticipated" = [
  Cat "have_anticipated" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_anticipated" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_anticipated" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_anticipated" = [
  Cat "has_anticipated" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_anticipate" = [
  Cat "will_anticipate" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "apologize" = [
  Cat "apologize" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [For] []],
  Cat "apologize" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [For] []],
  Cat "apologize" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [For] []],
  Cat "apologize" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [For] []]
  ]
lexicon "apologizes" = [
  Cat "apologizes" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [For] []]
  ]
lexicon "apologized" = [
  Cat "apologized" "VP" [Past] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [For] []]
  ]
lexicon "have_apologized" = [
  Cat "have_apologized" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [For] []],
  Cat "have_apologized" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [For] []],
  Cat "have_apologized" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [For] []]
  ]
lexicon "has_apologized" = [
  Cat "has_apologized" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [For] []]
  ]
lexicon "will_apologize" = [
  Cat "will_apologize" "VP" [Fut] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [For] []]
  ]

lexicon "appeal" = [
  Cat "appeal" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "appeal" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "appeal" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "appeal" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "appeals" = [
  Cat "appeals" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "appealed" = [
  Cat "appealed" "VP" [Past] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "have_appealed" = [
  Cat "have_appealed" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "have_appealed" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "have_appealed" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "has_appealed" = [
  Cat "has_appealed" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "will_appeal" = [
  Cat "will_appeal" "VP" [Fut] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]

lexicon "apply" = [
  Cat "apply" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "apply" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "apply" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "apply" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "applies" = [
  Cat "applies" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "applied" = [
  Cat "applied" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_applied" = [
  Cat "have_applied" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_applied" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_applied" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_applied" = [
  Cat "has_applied" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_apply" = [
  Cat "will_apply" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "appoint" = [
  Cat "appoint" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "appoint" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "appoint" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "appoint" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "appoints" = [
  Cat "appoints" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "appointed" = [
  Cat "appointed" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_appointed" = [
  Cat "have_appointed" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_appointed" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_appointed" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_appointed" = [
  Cat "has_appointed" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_appoint" = [
  Cat "will_appoint" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "appreciate" = [
  Cat "appreciate" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "appreciate" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "appreciate" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "appreciate" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "appreciates" = [
  Cat "appreciates" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "appreciated" = [
  Cat "appreciated" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_appreciated" = [
  Cat "have_appreciated" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_appreciated" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_appreciated" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_appreciated" = [
  Cat "has_appreciated" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_appreciate" = [
  Cat "will_appreciate" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "apprehend" = [
  Cat "apprehend" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "apprehend" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "apprehend" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "apprehend" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "apprehends" = [
  Cat "apprehends" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "apprehended" = [
  Cat "apprehended" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_apprehended" = [
  Cat "have_apprehended" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_apprehended" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_apprehended" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_apprehended" = [
  Cat "has_apprehended" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_apprehend" = [
  Cat "will_apprehend" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "approach" = [
  Cat "approach" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "approach" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "approach" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "approach" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "approaches" = [
  Cat "approaches" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "approached" = [
  Cat "approached" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_approached" = [
  Cat "have_approached" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_approached" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_approached" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_approached" = [
  Cat "has_approached" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_approach" = [
  Cat "will_approach" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "approve" = [
  Cat "approve" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "approve" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "approve" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "approve" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []],
  Cat "approve" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Of] []],
  Cat "approve" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Of] []],
  Cat "approve" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Of] []],
  Cat "approve" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Of] []]
  ]
lexicon "approves" = [
  Cat "approves" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []],
  Cat "approves" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Of] []]
  ]
lexicon "approved" = [
  Cat "approved" "VP" [Past] [Cat "_" "NP" [AccOrDat] []],
  Cat "approved" "VP" [Past] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Of] []]
  ]
lexicon "have_approved" = [
  Cat "have_approved" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_approved" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_approved" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_approved" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Of] []],
  Cat "have_approved" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Of] []],
  Cat "have_approved" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Of] []]
  ]
lexicon "has_approved" = [
  Cat "has_approved" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []],
  Cat "has_approved" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Of] []]
  ]
lexicon "will_approve" = [
  Cat "will_approve" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []],
  Cat "will_approve" "VP" [Fut] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Of] []]
  ]

lexicon "argue" = [
  Cat "argue" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [About] []],
  Cat "argue" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [About] []],
  Cat "argue" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [About] []],
  Cat "argue" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [About] []]
  ]
lexicon "argues" = [
  Cat "argues" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [About] []]
  ]
lexicon "argued" = [
  Cat "argued" "VP" [Past] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [About] []]
  ]
lexicon "have_argued" = [
  Cat "have_argued" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [About] []],
  Cat "have_argued" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [About] []],
  Cat "have_argued" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [About] []]
  ]
lexicon "has_argued" = [
  Cat "has_argued" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [About] []]
  ]
lexicon "will_argue" = [
  Cat "will_argue" "VP" [Fut] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [About] []]
  ]

lexicon "arouse" = [
  Cat "arouse" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "arouse" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "arouse" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "arouse" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "arouses" = [
  Cat "arouses" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "aroused" = [
  Cat "aroused" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_aroused" = [
  Cat "have_aroused" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_aroused" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_aroused" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_aroused" = [
  Cat "has_aroused" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_arouse" = [
  Cat "will_arouse" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "arrange" = [
  Cat "arrange" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "arrange" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "arrange" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "arrange" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "arranges" = [
  Cat "arranges" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "arranged" = [
  Cat "arranged" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_arranged" = [
  Cat "have_arranged" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_arranged" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_arranged" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_arranged" = [
  Cat "has_arranged" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_arrange" = [
  Cat "will_arrange" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "arrive" = [
  Cat "arrive" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [At] []],
  Cat "arrive" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [At] []],
  Cat "arrive" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [At] []],
  Cat "arrive" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [At] []]
  ]
lexicon "arrives" = [
  Cat "arrives" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [At] []]
  ]
lexicon "arrived" = [
  Cat "arrived" "VP" [Past] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [At] []]
  ]
lexicon "have_arrived" = [
  Cat "have_arrived" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [At] []],
  Cat "have_arrived" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [At] []],
  Cat "have_arrived" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [At] []]
  ]
lexicon "has_arrived" = [
  Cat "has_arrived" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [At] []]
  ]
lexicon "will_arrive" = [
  Cat "will_arrive" "VP" [Fut] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [At] []]
  ]

lexicon "ask" = [
  Cat "ask" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [For] []],
  Cat "ask" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [For] []],
  Cat "ask" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [For] []],
  Cat "ask" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [For] []],
  Cat "ask" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "ask" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "ask" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "ask" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []],
  Cat "ask" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [About] []],
  Cat "ask" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [About] []],
  Cat "ask" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [About] []],
  Cat "ask" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [About] []],
  Cat "ask" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "ask" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "ask" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "ask" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "asks" = [
  Cat "asks" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [For] []],
  Cat "asks" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []],
  Cat "asks" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [About] []],
  Cat "asks" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "asked" = [
  Cat "asked" "VP" [Past] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [For] []],
  Cat "asked" "VP" [Past] [Cat "_" "NP" [AccOrDat] []],
  Cat "asked" "VP" [Past] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [About] []],
  Cat "asked" "VP" [Past] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_asked" = [
  Cat "have_asked" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [For] []],
  Cat "have_asked" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [For] []],
  Cat "have_asked" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [For] []],
  Cat "have_asked" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_asked" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_asked" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_asked" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [About] []],
  Cat "have_asked" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [About] []],
  Cat "have_asked" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [About] []],
  Cat "have_asked" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "have_asked" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "have_asked" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_asked" = [
  Cat "has_asked" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [For] []],
  Cat "has_asked" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []],
  Cat "has_asked" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [About] []],
  Cat "has_asked" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_ask" = [
  Cat "will_ask" "VP" [Fut] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [For] []],
  Cat "will_ask" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []],
  Cat "will_ask" "VP" [Fut] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [About] []],
  Cat "will_ask" "VP" [Fut] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "aspire" = [
  Cat "aspire" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "aspire" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "aspire" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "aspire" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "aspires" = [
  Cat "aspires" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "aspired" = [
  Cat "aspired" "VP" [Past] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "have_aspired" = [
  Cat "have_aspired" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "have_aspired" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "have_aspired" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "has_aspired" = [
  Cat "has_aspired" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "will_aspire" = [
  Cat "will_aspire" "VP" [Fut] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]

lexicon "assert" = [
  Cat "assert" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "assert" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "assert" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "assert" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "asserts" = [
  Cat "asserts" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "asserted" = [
  Cat "asserted" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_asserted" = [
  Cat "have_asserted" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_asserted" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_asserted" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_asserted" = [
  Cat "has_asserted" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_assert" = [
  Cat "will_assert" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "assess" = [
  Cat "assess" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "assess" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "assess" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "assess" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "assesses" = [
  Cat "assesses" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "assessed" = [
  Cat "assessed" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_assessed" = [
  Cat "have_assessed" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_assessed" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_assessed" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_assessed" = [
  Cat "has_assessed" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_assess" = [
  Cat "will_assess" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "assign" = [
  Cat "assign" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "assign" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "assign" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "assign" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "assign" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "assign" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "assign" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "assign" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "assigns" = [
  Cat "assigns" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "assigns" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "assigned" = [
  Cat "assigned" "VP" [Past] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "assigned" "VP" [Past] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "have_assigned" = [
  Cat "have_assigned" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "have_assigned" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "have_assigned" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "have_assigned" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "have_assigned" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "have_assigned" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "has_assigned" = [
  Cat "has_assigned" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "has_assigned" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "will_assign" = [
  Cat "will_assign" "VP" [Fut] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "will_assign" "VP" [Fut] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]

lexicon "assist" = [
  Cat "assist" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "assist" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "assist" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "assist" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []],
  Cat "assist" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [With] []],
  Cat "assist" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [With] []],
  Cat "assist" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [With] []],
  Cat "assist" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [With] []]
  ]
lexicon "assists" = [
  Cat "assists" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []],
  Cat "assists" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [With] []]
  ]
lexicon "assisted" = [
  Cat "assisted" "VP" [Past] [Cat "_" "NP" [AccOrDat] []],
  Cat "assisted" "VP" [Past] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [With] []]
  ]
lexicon "have_assisted" = [
  Cat "have_assisted" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_assisted" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_assisted" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_assisted" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [With] []],
  Cat "have_assisted" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [With] []],
  Cat "have_assisted" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [With] []]
  ]
lexicon "has_assisted" = [
  Cat "has_assisted" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []],
  Cat "has_assisted" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [With] []]
  ]
lexicon "will_assist" = [
  Cat "will_assist" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []],
  Cat "will_assist" "VP" [Fut] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [With] []]
  ]

lexicon "associate" = [
  Cat "associate" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [With] []],
  Cat "associate" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [With] []],
  Cat "associate" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [With] []],
  Cat "associate" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [With] []]
  ]
lexicon "associates" = [
  Cat "associates" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [With] []]
  ]
lexicon "associated" = [
  Cat "associated" "VP" [Past] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [With] []]
  ]
lexicon "have_associated" = [
  Cat "have_associated" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [With] []],
  Cat "have_associated" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [With] []],
  Cat "have_associated" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [With] []]
  ]
lexicon "has_associated" = [
  Cat "has_associated" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [With] []]
  ]
lexicon "will_associate" = [
  Cat "will_associate" "VP" [Fut] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [With] []]
  ]

lexicon "assume" = [
  Cat "assume" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "assume" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "assume" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "assume" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "assumes" = [
  Cat "assumes" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "assumed" = [
  Cat "assumed" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_assumed" = [
  Cat "have_assumed" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_assumed" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_assumed" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_assumed" = [
  Cat "has_assumed" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_assume" = [
  Cat "will_assume" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "astound" = [
  Cat "astound" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "astound" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "astound" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "astound" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "astounds" = [
  Cat "astounds" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "astounded" = [
  Cat "astounded" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_astounded" = [
  Cat "have_astounded" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_astounded" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_astounded" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_astounded" = [
  Cat "has_astounded" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_astound" = [
  Cat "will_astound" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "attain" = [
  Cat "attain" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "attain" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "attain" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "attain" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "attains" = [
  Cat "attains" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "attained" = [
  Cat "attained" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_attained" = [
  Cat "have_attained" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_attained" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_attained" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_attained" = [
  Cat "has_attained" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_attain" = [
  Cat "will_attain" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "attempt" = [
  Cat "attempt" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "attempt" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "attempt" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "attempt" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "attempts" = [
  Cat "attempts" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "attempted" = [
  Cat "attempted" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_attempted" = [
  Cat "have_attempted" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_attempted" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_attempted" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_attempted" = [
  Cat "has_attempted" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_attempt" = [
  Cat "will_attempt" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "attest" = [
  Cat "attest" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "attest" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "attest" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "attest" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "attests" = [
  Cat "attests" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "attested" = [
  Cat "attested" "VP" [Past] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "have_attested" = [
  Cat "have_attested" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "have_attested" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "have_attested" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "has_attested" = [
  Cat "has_attested" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "will_attest" = [
  Cat "will_attest" "VP" [Fut] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]

lexicon "attract" = [
  Cat "attract" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "attract" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "attract" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "attract" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "attracts" = [
  Cat "attracts" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "attracted" = [
  Cat "attracted" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_attracted" = [
  Cat "have_attracted" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_attracted" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_attracted" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_attracted" = [
  Cat "has_attracted" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_attract" = [
  Cat "will_attract" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "auction" = [
  Cat "auction" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "auction" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "auction" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "auction" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "auctions" = [
  Cat "auctions" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "auctioned" = [
  Cat "auctioned" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_auctioned" = [
  Cat "have_auctioned" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_auctioned" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_auctioned" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_auctioned" = [
  Cat "has_auctioned" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_auction" = [
  Cat "will_auction" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "avert" = [
  Cat "avert" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "avert" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "avert" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "avert" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "averts" = [
  Cat "averts" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "averted" = [
  Cat "averted" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_averted" = [
  Cat "have_averted" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_averted" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_averted" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_averted" = [
  Cat "has_averted" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_avert" = [
  Cat "will_avert" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "avoid" = [
  Cat "avoid" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "avoid" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "avoid" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "avoid" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "avoids" = [
  Cat "avoids" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "avoided" = [
  Cat "avoided" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_avoided" = [
  Cat "have_avoided" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_avoided" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_avoided" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_avoided" = [
  Cat "has_avoided" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_avoid" = [
  Cat "will_avoid" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "await" = [
  Cat "await" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "await" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "await" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "await" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "awaits" = [
  Cat "awaits" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "awaited" = [
  Cat "awaited" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_awaited" = [
  Cat "have_awaited" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_awaited" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_awaited" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_awaited" = [
  Cat "has_awaited" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_await" = [
  Cat "will_await" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

{-lexicon "babble" = [
  Cat "babble" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "babble" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "babble" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "babble" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []],
  Cat "babble" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [About] []],
  Cat "babble" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [About] []],
  Cat "babble" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [About] []],
  Cat "babble" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [About] []]
  ]
lexicon "babbles" = [
  Cat "babbles" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []],
  Cat "babbles" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [About] []]
  ]
lexicon "babbled" = [
  Cat "babbled" "VP" [Past] [Cat "_" "NP" [AccOrDat] []],
  Cat "babbled" "VP" [Past] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [About] []]
  ]
lexicon "have_babbled" = [
  Cat "have_babbled" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_babbled" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_babbled" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_babbled" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [About] []],
  Cat "have_babbled" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [About] []],
  Cat "have_babbled" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [About] []]
  ]
lexicon "has_babbled" = [
  Cat "has_babbled" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []],
  Cat "has_babbled" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [About] []]
  ]
lexicon "will_babble" = [
  Cat "will_babble" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []],
  Cat "will_babble" "VP" [Fut] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [About] []]
  ]

lexicon "badger" = [
  Cat "badger" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "badger" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "badger" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "badger" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "badgers" = [
  Cat "badgers" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "badgered" = [
  Cat "badgered" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_badgered" = [
  Cat "have_badgered" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_badgered" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_badgered" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_badgered" = [
  Cat "has_badgered" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_badger" = [
  Cat "will_badger" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "baffle" = [
  Cat "baffle" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "baffle" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "baffle" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "baffle" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "baffles" = [
  Cat "baffles" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "baffled" = [
  Cat "baffled" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_baffled" = [
  Cat "have_baffled" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_baffled" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_baffled" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_baffled" = [
  Cat "has_baffled" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_baffle" = [
  Cat "will_baffle" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "bake" = [
  Cat "bake" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "bake" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "bake" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "bake" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []],
  Cat "bake" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "bake" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "bake" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "bake" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "bakes" = [
  Cat "bakes" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []],
  Cat "bakes" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "baked" = [
  Cat "baked" "VP" [Past] [Cat "_" "NP" [AccOrDat] []],
  Cat "baked" "VP" [Past] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_baked" = [
  Cat "have_baked" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_baked" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_baked" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_baked" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "have_baked" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "have_baked" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_baked" = [
  Cat "has_baked" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []],
  Cat "has_baked" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_bake" = [
  Cat "will_bake" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []],
  Cat "will_bake" "VP" [Fut] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "ban" = [
  Cat "ban" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "ban" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "ban" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "ban" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "bans" = [
  Cat "bans" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "banned" = [
  Cat "banned" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_banned" = [
  Cat "have_banned" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_banned" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_banned" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_banned" = [
  Cat "has_banned" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_ban" = [
  Cat "will_ban" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "bang" = [
  Cat "bang" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "bang" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "bang" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "bang" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []],
  Cat "bang" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []],
  Cat "bang" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []],
  Cat "bang" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []],
  Cat "bang" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []]
  ]
lexicon "bangs" = [
  Cat "bangs" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []],
  Cat "bangs" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []]
  ]
lexicon "banged" = [
  Cat "banged" "VP" [Past] [Cat "_" "NP" [AccOrDat] []],
  Cat "banged" "VP" [Past] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []]
  ]
lexicon "have_banged" = [
  Cat "have_banged" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_banged" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_banged" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_banged" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []],
  Cat "have_banged" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []],
  Cat "have_banged" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []]
  ]
lexicon "has_banged" = [
  Cat "has_banged" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []],
  Cat "has_banged" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []]
  ]
lexicon "will_bang" = [
  Cat "will_bang" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []],
  Cat "will_bang" "VP" [Fut] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []]
  ]

lexicon "bear" = [
  Cat "bear" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "bear" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "bear" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "bear" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "bears" = [
  Cat "bears" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "bore" = [
  Cat "bore" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_born" = [
  Cat "have_born" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_born" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_born" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_born" = [
  Cat "has_born" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_bear" = [
  Cat "will_bear" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "beckon" = [
  Cat "beckon" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "beckon" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "beckon" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "beckon" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "beckons" = [
  Cat "beckons" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "beckoned" = [
  Cat "beckoned" "VP" [Past] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "have_beckoned" = [
  Cat "have_beckoned" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "have_beckoned" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "have_beckoned" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "has_beckoned" = [
  Cat "has_beckoned" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "will_beckon" = [
  Cat "will_beckon" "VP" [Fut] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]

lexicon "become" = [
  Cat "become" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "become" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "become" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "become" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "becomes" = [
  Cat "becomes" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "became" = [
  Cat "became" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_become" = [
  Cat "have_become" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_become" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_become" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_become" = [
  Cat "has_become" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_become" = [
  Cat "will_become" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "beg" = [
  Cat "beg" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "beg" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "beg" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "beg" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "begs" = [
  Cat "begs" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "begged" = [
  Cat "begged" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_begged" = [
  Cat "have_begged" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_begged" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_begged" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_begged" = [
  Cat "has_begged" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_beg" = [
  Cat "will_beg" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "begin" = [
  Cat "begin" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "begin" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "begin" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "begin" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "begins" = [
  Cat "begins" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "began" = [
  Cat "began" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_begun" = [
  Cat "have_begun" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_begun" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_begun" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_begun" = [
  Cat "has_begun" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_begin" = [
  Cat "will_begin" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "behave" = [
  Cat "behave" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Like] []],
  Cat "behave" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Like] []],
  Cat "behave" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Like] []],
  Cat "behave" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Like] []]
  ]
lexicon "behaves" = [
  Cat "behaves" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Like] []]
  ]
lexicon "behaved" = [
  Cat "behaved" "VP" [Past] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Like] []]
  ]
lexicon "have_behaved" = [
  Cat "have_behaved" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Like] []],
  Cat "have_behaved" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Like] []],
  Cat "have_behaved" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Like] []]
  ]
lexicon "has_behaved" = [
  Cat "has_behaved" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Like] []]
  ]
lexicon "will_behave" = [
  Cat "will_behave" "VP" [Fut] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Like] []]
  ]

lexicon "behold" = [
  Cat "behold" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "behold" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "behold" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "behold" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "beholds" = [
  Cat "beholds" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "beholded" = [
  Cat "beholded" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_beholded" = [
  Cat "have_beholded" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_beholded" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_beholded" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_beholded" = [
  Cat "has_beholded" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_behold" = [
  Cat "will_behold" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "believe" = [
  Cat "believe" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [In] []],
  Cat "believe" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [In] []],
  Cat "believe" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [In] []],
  Cat "believe" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [In] []],
  Cat "believe" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "believe" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "believe" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "believe" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "believes" = [
  Cat "believes" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [In] []],
  Cat "believes" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "believed" = [
  Cat "believed" "VP" [Past] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [In] []],
  Cat "believed" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_believed" = [
  Cat "have_believed" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [In] []],
  Cat "have_believed" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [In] []],
  Cat "have_believed" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [In] []],
  Cat "have_believed" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_believed" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_believed" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_believed" = [
  Cat "has_believed" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [In] []],
  Cat "has_believed" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_believe" = [
  Cat "will_believe" "VP" [Fut] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [In] []],
  Cat "will_believe" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "belong" = [
  Cat "belong" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "belong" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "belong" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "belong" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "belongs" = [
  Cat "belongs" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "belonged" = [
  Cat "belonged" "VP" [Past] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "have_belonged" = [
  Cat "have_belonged" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "have_belonged" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "have_belonged" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "has_belonged" = [
  Cat "has_belonged" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "will_belong" = [
  Cat "will_belong" "VP" [Fut] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]

lexicon "benefit" = [
  Cat "benefit" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [From] []],
  Cat "benefit" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [From] []],
  Cat "benefit" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [From] []],
  Cat "benefit" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [From] []],
  Cat "benefit" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "benefit" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "benefit" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "benefit" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "benefits" = [
  Cat "benefits" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [From] []],
  Cat "benefits" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "benefited" = [
  Cat "benefited" "VP" [Past] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [From] []],
  Cat "benefited" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_benefited" = [
  Cat "have_benefited" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [From] []],
  Cat "have_benefited" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [From] []],
  Cat "have_benefited" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [From] []],
  Cat "have_benefited" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_benefited" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_benefited" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_benefited" = [
  Cat "has_benefited" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [From] []],
  Cat "has_benefited" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_benefit" = [
  Cat "will_benefit" "VP" [Fut] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [From] []],
  Cat "will_benefit" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "bet" = [
  Cat "bet" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []],
  Cat "bet" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []],
  Cat "bet" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []],
  Cat "bet" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []]
  ]
lexicon "bets" = [
  Cat "bets" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []]
  ]
lexicon "bet" = [
  Cat "bet" "VP" [Past] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []]
  ]
lexicon "have_bet" = [
  Cat "have_bet" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []],
  Cat "have_bet" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []],
  Cat "have_bet" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []]
  ]
lexicon "has_bet" = [
  Cat "has_bet" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []]
  ]
lexicon "will_bet" = [
  Cat "will_bet" "VP" [Fut] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []]
  ]

lexicon "betray" = [
  Cat "betray" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "betray" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "betray" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "betray" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "betrays" = [
  Cat "betrays" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "betrayed" = [
  Cat "betrayed" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_betrayed" = [
  Cat "have_betrayed" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_betrayed" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_betrayed" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_betrayed" = [
  Cat "has_betrayed" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_betray" = [
  Cat "will_betray" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "bill" = [
  Cat "bill" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "bill" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "bill" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "bill" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "bills" = [
  Cat "bills" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "billed" = [
  Cat "billed" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_billed" = [
  Cat "have_billed" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_billed" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_billed" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_billed" = [
  Cat "has_billed" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_bill" = [
  Cat "will_bill" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "bind" = [
  Cat "bind" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "bind" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "bind" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "bind" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "binds" = [
  Cat "binds" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "bound" = [
  Cat "bound" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_bound" = [
  Cat "have_bound" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_bound" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_bound" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_bound" = [
  Cat "has_bound" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_bind" = [
  Cat "will_bind" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "blame" = [
  Cat "blame" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "blame" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "blame" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "blame" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "blames" = [
  Cat "blames" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "blamed" = [
  Cat "blamed" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_blamed" = [
  Cat "have_blamed" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_blamed" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_blamed" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_blamed" = [
  Cat "has_blamed" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_blame" = [
  Cat "will_blame" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "board" = [
  Cat "board" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "board" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "board" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "board" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "boards" = [
  Cat "boards" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "boarded" = [
  Cat "boarded" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_boarded" = [
  Cat "have_boarded" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_boarded" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_boarded" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_boarded" = [
  Cat "has_boarded" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_board" = [
  Cat "will_board" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "boast" = [
  Cat "boast" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "boast" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "boast" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "boast" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "boasts" = [
  Cat "boasts" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "boasted" = [
  Cat "boasted" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_boasted" = [
  Cat "have_boasted" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_boasted" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_boasted" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_boasted" = [
  Cat "has_boasted" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_boast" = [
  Cat "will_boast" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "bore" = [
  Cat "bore" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "bore" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "bore" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "bore" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []],
  Cat "bore" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Into] []],
  Cat "bore" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Into] []],
  Cat "bore" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Into] []],
  Cat "bore" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Into] []]
  ]
lexicon "bears" = [
  Cat "bears" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []],
  Cat "bears" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Into] []]
  ]
lexicon "bore" = [
  Cat "bore" "VP" [Past] [Cat "_" "NP" [AccOrDat] []],
  Cat "bore" "VP" [Past] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Into] []]
  ]
lexicon "have_born" = [
  Cat "have_born" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_born" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_born" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_born" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Into] []],
  Cat "have_born" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Into] []],
  Cat "have_born" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Into] []]
  ]
lexicon "has_born" = [
  Cat "has_born" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []],
  Cat "has_born" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Into] []]
  ]
lexicon "will_bear" = [
  Cat "will_bear" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []],
  Cat "will_bear" "VP" [Fut] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Into] []]
  ]

lexicon "bother" = [
  Cat "bother" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "bother" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "bother" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "bother" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "bothers" = [
  Cat "bothers" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "bothered" = [
  Cat "bothered" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_bothered" = [
  Cat "have_bothered" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_bothered" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_bothered" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_bothered" = [
  Cat "has_bothered" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_bother" = [
  Cat "will_bother" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "break" = [
  Cat "break" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "break" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "break" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "break" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "breaks" = [
  Cat "breaks" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "broke" = [
  Cat "broke" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_broken" = [
  Cat "have_broken" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_broken" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_broken" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_broken" = [
  Cat "has_broken" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_break" = [
  Cat "will_break" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "breathe" = [
  Cat "breathe" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "breathe" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "breathe" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "breathe" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "breathes" = [
  Cat "breathes" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "breathed" = [
  Cat "breathed" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_breathed" = [
  Cat "have_breathed" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_breathed" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_breathed" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_breathed" = [
  Cat "has_breathed" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_breathe" = [
  Cat "will_breathe" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "breed" = [
  Cat "breed" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "breed" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "breed" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "breed" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "breeds" = [
  Cat "breeds" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "bred" = [
  Cat "bred" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_bred" = [
  Cat "have_bred" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_bred" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_bred" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_bred" = [
  Cat "has_bred" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_breed" = [
  Cat "will_breed" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "bribe" = [
  Cat "bribe" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "bribe" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "bribe" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "bribe" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "bribes" = [
  Cat "bribes" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "bribed" = [
  Cat "bribed" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_bribed" = [
  Cat "have_bribed" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_bribed" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_bribed" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_bribed" = [
  Cat "has_bribed" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_bribe" = [
  Cat "will_bribe" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "bring" = [
  Cat "bring" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "bring" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "bring" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "bring" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []],
  Cat "bring" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "bring" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "bring" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "bring" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "brings" = [
  Cat "brings" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []],
  Cat "brings" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "brought" = [
  Cat "brought" "VP" [Past] [Cat "_" "NP" [AccOrDat] []],
  Cat "brought" "VP" [Past] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_brought" = [
  Cat "have_brought" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_brought" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_brought" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_brought" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "have_brought" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "have_brought" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_brought" = [
  Cat "has_brought" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []],
  Cat "has_brought" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_bring" = [
  Cat "will_bring" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []],
  Cat "will_bring" "VP" [Fut] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "broach" = [
  Cat "broach" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "broach" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "broach" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "broach" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "broaches" = [
  Cat "broaches" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "broached" = [
  Cat "broached" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_broached" = [
  Cat "have_broached" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_broached" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_broached" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_broached" = [
  Cat "has_broached" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_broach" = [
  Cat "will_broach" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "broadcast" = [
  Cat "broadcast" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "broadcast" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "broadcast" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "broadcast" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []],
  Cat "broadcast" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "broadcast" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "broadcast" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "broadcast" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "broadcasts" = [
  Cat "broadcasts" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []],
  Cat "broadcasts" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "broadcasted" = [
  Cat "broadcasted" "VP" [Past] [Cat "_" "NP" [AccOrDat] []],
  Cat "broadcasted" "VP" [Past] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "have_broadcasted" = [
  Cat "have_broadcasted" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_broadcasted" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_broadcasted" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_broadcasted" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "have_broadcasted" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "have_broadcasted" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "has_broadcasted" = [
  Cat "has_broadcasted" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []],
  Cat "has_broadcasted" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "will_broadcast" = [
  Cat "will_broadcast" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []],
  Cat "will_broadcast" "VP" [Fut] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]

lexicon "build" = [
  Cat "build" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "build" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "build" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "build" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []],
  Cat "build" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "build" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "build" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "build" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "builds" = [
  Cat "builds" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []],
  Cat "builds" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "built" = [
  Cat "built" "VP" [Past] [Cat "_" "NP" [AccOrDat] []],
  Cat "built" "VP" [Past] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_built" = [
  Cat "have_built" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_built" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_built" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_built" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "have_built" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "have_built" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_built" = [
  Cat "has_built" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []],
  Cat "has_built" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_build" = [
  Cat "will_build" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []],
  Cat "will_build" "VP" [Fut] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "buy" = [
  Cat "buy" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "buy" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "buy" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "buy" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []],
  Cat "buy" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "buy" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "buy" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "buy" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "buys" = [
  Cat "buys" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []],
  Cat "buys" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "bought" = [
  Cat "bought" "VP" [Past] [Cat "_" "NP" [AccOrDat] []],
  Cat "bought" "VP" [Past] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_bought" = [
  Cat "have_bought" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_bought" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_bought" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_bought" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "have_bought" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "have_bought" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_bought" = [
  Cat "has_bought" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []],
  Cat "has_bought" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_buy" = [
  Cat "will_buy" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []],
  Cat "will_buy" "VP" [Fut] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "cable" = [
  Cat "cable" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "cable" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "cable" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "cable" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "cables" = [
  Cat "cables" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "cabled" = [
  Cat "cabled" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_cabled" = [
  Cat "have_cabled" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_cabled" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_cabled" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_cabled" = [
  Cat "has_cabled" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_cable" = [
  Cat "will_cable" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "calculate" = [
  Cat "calculate" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "calculate" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "calculate" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "calculate" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "calculates" = [
  Cat "calculates" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "calculated" = [
  Cat "calculated" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_calculated" = [
  Cat "have_calculated" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_calculated" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_calculated" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_calculated" = [
  Cat "has_calculated" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_calculate" = [
  Cat "will_calculate" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "call" = [
  Cat "call" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "call" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "call" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "call" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "calls" = [
  Cat "calls" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "called" = [
  Cat "called" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_called" = [
  Cat "have_called" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_called" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_called" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_called" = [
  Cat "has_called" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_call" = [
  Cat "will_call" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "cancel" = [
  Cat "cancel" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "cancel" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "cancel" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "cancel" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "cancels" = [
  Cat "cancels" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "canceled" = [
  Cat "canceled" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_canceled" = [
  Cat "have_canceled" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_canceled" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_canceled" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_canceled" = [
  Cat "has_canceled" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_cancel" = [
  Cat "will_cancel" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "capture" = [
  Cat "capture" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "capture" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "capture" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "capture" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "captures" = [
  Cat "captures" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "captured" = [
  Cat "captured" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_captured" = [
  Cat "have_captured" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_captured" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_captured" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_captured" = [
  Cat "has_captured" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_capture" = [
  Cat "will_capture" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "catch" = [
  Cat "catch" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "catch" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "catch" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "catch" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "catches" = [
  Cat "catches" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "caught" = [
  Cat "caught" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_caught" = [
  Cat "have_caught" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_caught" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_caught" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_caught" = [
  Cat "has_caught" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_catch" = [
  Cat "will_catch" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "cause" = [
  Cat "cause" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "cause" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "cause" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "cause" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []],
  Cat "cause" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "cause" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "cause" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "cause" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "causes" = [
  Cat "causes" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []],
  Cat "causes" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "caused" = [
  Cat "caused" "VP" [Past] [Cat "_" "NP" [AccOrDat] []],
  Cat "caused" "VP" [Past] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_caused" = [
  Cat "have_caused" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_caused" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_caused" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_caused" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "have_caused" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "have_caused" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_caused" = [
  Cat "has_caused" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []],
  Cat "has_caused" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_cause" = [
  Cat "will_cause" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []],
  Cat "will_cause" "VP" [Fut] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "cease" = [
  Cat "cease" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "cease" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "cease" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "cease" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "ceases" = [
  Cat "ceases" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "ceased" = [
  Cat "ceased" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_ceased" = [
  Cat "have_ceased" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_ceased" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_ceased" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_ceased" = [
  Cat "has_ceased" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_cease" = [
  Cat "will_cease" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "certify" = [
  Cat "certify" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "certify" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "certify" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "certify" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "certifies" = [
  Cat "certifies" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "certified" = [
  Cat "certified" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_certified" = [
  Cat "have_certified" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_certified" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_certified" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_certified" = [
  Cat "has_certified" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_certify" = [
  Cat "will_certify" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "challenge" = [
  Cat "challenge" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "challenge" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "challenge" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "challenge" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "challenges" = [
  Cat "challenges" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "challenged" = [
  Cat "challenged" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_challenged" = [
  Cat "have_challenged" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_challenged" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_challenged" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_challenged" = [
  Cat "has_challenged" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_challenge" = [
  Cat "will_challenge" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "chant" = [
  Cat "chant" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "chant" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "chant" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "chant" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "chants" = [
  Cat "chants" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "chanted" = [
  Cat "chanted" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_chanted" = [
  Cat "have_chanted" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_chanted" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_chanted" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_chanted" = [
  Cat "has_chanted" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_chant" = [
  Cat "will_chant" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "charge" = [
  Cat "charge" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "charge" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "charge" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "charge" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "charges" = [
  Cat "charges" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "charged" = [
  Cat "charged" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_charged" = [
  Cat "have_charged" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_charged" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_charged" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_charged" = [
  Cat "has_charged" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_charge" = [
  Cat "will_charge" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "chase" = [
  Cat "chase" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "chase" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "chase" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "chase" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "chases" = [
  Cat "chases" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "chased" = [
  Cat "chased" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_chased" = [
  Cat "have_chased" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_chased" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_chased" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_chased" = [
  Cat "has_chased" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_chase" = [
  Cat "will_chase" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "cheat" = [
  Cat "cheat" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "cheat" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "cheat" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "cheat" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "cheats" = [
  Cat "cheats" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "cheated" = [
  Cat "cheated" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_cheated" = [
  Cat "have_cheated" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_cheated" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_cheated" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_cheated" = [
  Cat "has_cheated" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_cheat" = [
  Cat "will_cheat" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "check" = [
  Cat "check" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "check" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "check" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "check" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "checks" = [
  Cat "checks" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "checked" = [
  Cat "checked" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_checked" = [
  Cat "have_checked" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_checked" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_checked" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_checked" = [
  Cat "has_checked" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_check" = [
  Cat "will_check" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "cherish" = [
  Cat "cherish" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "cherish" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "cherish" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "cherish" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "cherishes" = [
  Cat "cherishes" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "cherished" = [
  Cat "cherished" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_cherished" = [
  Cat "have_cherished" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_cherished" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_cherished" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_cherished" = [
  Cat "has_cherished" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_cherish" = [
  Cat "will_cherish" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "chew" = [
  Cat "chew" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "chew" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "chew" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "chew" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "chews" = [
  Cat "chews" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "chewed" = [
  Cat "chewed" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_chewed" = [
  Cat "have_chewed" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_chewed" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_chewed" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_chewed" = [
  Cat "has_chewed" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_chew" = [
  Cat "will_chew" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "chill" = [
  Cat "chill" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "chill" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "chill" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "chill" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "chills" = [
  Cat "chills" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "chilled" = [
  Cat "chilled" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_chilled" = [
  Cat "have_chilled" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_chilled" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_chilled" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_chilled" = [
  Cat "has_chilled" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_chill" = [
  Cat "will_chill" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "choose" = [
  Cat "choose" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "choose" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "choose" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "choose" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "chooses" = [
  Cat "chooses" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "chose" = [
  Cat "chose" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_chosen" = [
  Cat "have_chosen" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_chosen" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_chosen" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_chosen" = [
  Cat "has_chosen" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_choose" = [
  Cat "will_choose" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "chop" = [
  Cat "chop" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "chop" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "chop" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "chop" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "chops" = [
  Cat "chops" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "chopped" = [
  Cat "chopped" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_chopped" = [
  Cat "have_chopped" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_chopped" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_chopped" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_chopped" = [
  Cat "has_chopped" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_chop" = [
  Cat "will_chop" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "cite" = [
  Cat "cite" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "cite" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "cite" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "cite" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "cites" = [
  Cat "cites" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "cited" = [
  Cat "cited" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_cited" = [
  Cat "have_cited" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_cited" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_cited" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_cited" = [
  Cat "has_cited" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_cite" = [
  Cat "will_cite" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "claim" = [
  Cat "claim" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "claim" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "claim" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "claim" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "claims" = [
  Cat "claims" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "claimed" = [
  Cat "claimed" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_claimed" = [
  Cat "have_claimed" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_claimed" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_claimed" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_claimed" = [
  Cat "has_claimed" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_claim" = [
  Cat "will_claim" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "close" = [
  Cat "close" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "close" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "close" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "close" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "closes" = [
  Cat "closes" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "closed" = [
  Cat "closed" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_closed" = [
  Cat "have_closed" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_closed" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_closed" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_closed" = [
  Cat "has_closed" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_close" = [
  Cat "will_close" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "clothe" = [
  Cat "clothe" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "clothe" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "clothe" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "clothe" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "clothes" = [
  Cat "clothes" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "clothed" = [
  Cat "clothed" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_clothed" = [
  Cat "have_clothed" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_clothed" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_clothed" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_clothed" = [
  Cat "has_clothed" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_clothe" = [
  Cat "will_clothe" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "coach" = [
  Cat "coach" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "coach" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "coach" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "coach" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "coaches" = [
  Cat "coaches" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "coached" = [
  Cat "coached" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_coached" = [
  Cat "have_coached" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_coached" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_coached" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_coached" = [
  Cat "has_coached" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_coach" = [
  Cat "will_coach" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "coax" = [
  Cat "coax" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "coax" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "coax" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "coax" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "coaxes" = [
  Cat "coaxes" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "coaxed" = [
  Cat "coaxed" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_coaxed" = [
  Cat "have_coaxed" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_coaxed" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_coaxed" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_coaxed" = [
  Cat "has_coaxed" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_coax" = [
  Cat "will_coax" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "coerce" = [
  Cat "coerce" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "coerce" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "coerce" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "coerce" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "coerces" = [
  Cat "coerces" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "coerced" = [
  Cat "coerced" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_coerced" = [
  Cat "have_coerced" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_coerced" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_coerced" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_coerced" = [
  Cat "has_coerced" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_coerce" = [
  Cat "will_coerce" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "collect" = [
  Cat "collect" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "collect" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "collect" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "collect" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "collects" = [
  Cat "collects" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "collected" = [
  Cat "collected" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_collected" = [
  Cat "have_collected" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_collected" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_collected" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_collected" = [
  Cat "has_collected" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_collect" = [
  Cat "will_collect" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "come" = [
  Cat "come" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "come" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "come" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "come" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "comes" = [
  Cat "comes" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "came" = [
  Cat "came" "VP" [Past] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "have_come" = [
  Cat "have_come" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "have_come" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "have_come" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "has_come" = [
  Cat "has_come" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "will_come" = [
  Cat "will_come" "VP" [Fut] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]

lexicon "command" = [
  Cat "command" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "command" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "command" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "command" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "commands" = [
  Cat "commands" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "commanded" = [
  Cat "commanded" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_commanded" = [
  Cat "have_commanded" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_commanded" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_commanded" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_commanded" = [
  Cat "has_commanded" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_command" = [
  Cat "will_command" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "commend" = [
  Cat "commend" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "commend" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "commend" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "commend" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []],
  Cat "commend" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "commend" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "commend" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "commend" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "commends" = [
  Cat "commends" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []],
  Cat "commends" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "commended" = [
  Cat "commended" "VP" [Past] [Cat "_" "NP" [AccOrDat] []],
  Cat "commended" "VP" [Past] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "have_commended" = [
  Cat "have_commended" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_commended" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_commended" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_commended" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "have_commended" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "have_commended" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "has_commended" = [
  Cat "has_commended" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []],
  Cat "has_commended" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "will_commend" = [
  Cat "will_commend" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []],
  Cat "will_commend" "VP" [Fut] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]

lexicon "comment" = [
  Cat "comment" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []],
  Cat "comment" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []],
  Cat "comment" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []],
  Cat "comment" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []]
  ]
lexicon "comments" = [
  Cat "comments" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []]
  ]
lexicon "commented" = [
  Cat "commented" "VP" [Past] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []]
  ]
lexicon "have_commented" = [
  Cat "have_commented" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []],
  Cat "have_commented" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []],
  Cat "have_commented" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []]
  ]
lexicon "has_commented" = [
  Cat "has_commented" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []]
  ]
lexicon "will_comment" = [
  Cat "will_comment" "VP" [Fut] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []]
  ]

lexicon "commision" = [
  Cat "commision" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "commision" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "commision" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "commision" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "commisions" = [
  Cat "commisions" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "commisioned" = [
  Cat "commisioned" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_commisioned" = [
  Cat "have_commisioned" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_commisioned" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_commisioned" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_commisioned" = [
  Cat "has_commisioned" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_commision" = [
  Cat "will_commision" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "commit" = [
  Cat "commit" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "commit" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "commit" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "commit" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "commits" = [
  Cat "commits" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "committed" = [
  Cat "committed" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_committed" = [
  Cat "have_committed" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_committed" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_committed" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_committed" = [
  Cat "has_committed" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_commit" = [
  Cat "will_commit" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "communicate" = [
  Cat "communicate" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [With] []],
  Cat "communicate" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [With] []],
  Cat "communicate" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [With] []],
  Cat "communicate" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [With] []],
  Cat "communicate" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "communicate" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "communicate" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "communicate" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "communicates" = [
  Cat "communicates" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [With] []],
  Cat "communicates" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "communicated" = [
  Cat "communicated" "VP" [Past] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [With] []],
  Cat "communicated" "VP" [Past] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "have_communicated" = [
  Cat "have_communicated" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [With] []],
  Cat "have_communicated" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [With] []],
  Cat "have_communicated" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [With] []],
  Cat "have_communicated" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "have_communicated" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "have_communicated" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "has_communicated" = [
  Cat "has_communicated" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [With] []],
  Cat "has_communicated" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "will_communicate" = [
  Cat "will_communicate" "VP" [Fut] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [With] []],
  Cat "will_communicate" "VP" [Fut] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]

lexicon "complain" = [
  Cat "complain" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [About] []],
  Cat "complain" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [About] []],
  Cat "complain" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [About] []],
  Cat "complain" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [About] []]
  ]
lexicon "complains" = [
  Cat "complains" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [About] []]
  ]
lexicon "complained" = [
  Cat "complained" "VP" [Past] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [About] []]
  ]
lexicon "have_complained" = [
  Cat "have_complained" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [About] []],
  Cat "have_complained" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [About] []],
  Cat "have_complained" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [About] []]
  ]
lexicon "has_complained" = [
  Cat "has_complained" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [About] []]
  ]
lexicon "will_complain" = [
  Cat "will_complain" "VP" [Fut] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [About] []]
  ]

lexicon "complete" = [
  Cat "complete" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "complete" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "complete" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "complete" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "completes" = [
  Cat "completes" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "completed" = [
  Cat "completed" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_completed" = [
  Cat "have_completed" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_completed" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_completed" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_completed" = [
  Cat "has_completed" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_complete" = [
  Cat "will_complete" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "comprehend" = [
  Cat "comprehend" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "comprehend" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "comprehend" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "comprehend" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "comprehends" = [
  Cat "comprehends" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "comprehended" = [
  Cat "comprehended" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_comprehended" = [
  Cat "have_comprehended" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_comprehended" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_comprehended" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_comprehended" = [
  Cat "has_comprehended" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_comprehend" = [
  Cat "will_comprehend" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "conceal" = [
  Cat "conceal" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "conceal" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "conceal" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "conceal" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "conceals" = [
  Cat "conceals" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "concealed" = [
  Cat "concealed" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_concealed" = [
  Cat "have_concealed" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_concealed" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_concealed" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_concealed" = [
  Cat "has_concealed" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_conceal" = [
  Cat "will_conceal" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "concede" = [
  Cat "concede" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "concede" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "concede" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "concede" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []],
  Cat "concede" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "concede" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "concede" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "concede" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "concedes" = [
  Cat "concedes" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []],
  Cat "concedes" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "conceded" = [
  Cat "conceded" "VP" [Past] [Cat "_" "NP" [AccOrDat] []],
  Cat "conceded" "VP" [Past] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "have_conceded" = [
  Cat "have_conceded" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_conceded" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_conceded" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_conceded" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "have_conceded" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "have_conceded" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "has_conceded" = [
  Cat "has_conceded" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []],
  Cat "has_conceded" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "will_concede" = [
  Cat "will_concede" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []],
  Cat "will_concede" "VP" [Fut] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]

lexicon "conceive" = [
  Cat "conceive" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "conceive" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "conceive" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "conceive" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "conceives" = [
  Cat "conceives" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "conceived" = [
  Cat "conceived" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_conceived" = [
  Cat "have_conceived" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_conceived" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_conceived" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_conceived" = [
  Cat "has_conceived" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_conceive" = [
  Cat "will_conceive" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "concentrate" = [
  Cat "concentrate" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []],
  Cat "concentrate" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []],
  Cat "concentrate" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []],
  Cat "concentrate" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []]
  ]
lexicon "concentrates" = [
  Cat "concentrates" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []]
  ]
lexicon "concentrated" = [
  Cat "concentrated" "VP" [Past] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []]
  ]
lexicon "have_concentrated" = [
  Cat "have_concentrated" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []],
  Cat "have_concentrated" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []],
  Cat "have_concentrated" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []]
  ]
lexicon "has_concentrated" = [
  Cat "has_concentrated" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []]
  ]
lexicon "will_concentrate" = [
  Cat "will_concentrate" "VP" [Fut] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []]
  ]

lexicon "conclude" = [
  Cat "conclude" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "conclude" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "conclude" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "conclude" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "concludes" = [
  Cat "concludes" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "concluded" = [
  Cat "concluded" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_concluded" = [
  Cat "have_concluded" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_concluded" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_concluded" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_concluded" = [
  Cat "has_concluded" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_conclude" = [
  Cat "will_conclude" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "condemn" = [
  Cat "condemn" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "condemn" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "condemn" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "condemn" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "condemns" = [
  Cat "condemns" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "condemned" = [
  Cat "condemned" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_condemned" = [
  Cat "have_condemned" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_condemned" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_condemned" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_condemned" = [
  Cat "has_condemned" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_condemn" = [
  Cat "will_condemn" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "condescend" = [
  Cat "condescend" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "condescend" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "condescend" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "condescend" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "condescends" = [
  Cat "condescends" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "condescended" = [
  Cat "condescended" "VP" [Past] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "have_condescended" = [
  Cat "have_condescended" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "have_condescended" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "have_condescended" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "has_condescended" = [
  Cat "has_condescended" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "will_condescend" = [
  Cat "will_condescend" "VP" [Fut] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]

lexicon "condition" = [
  Cat "condition" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "condition" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "condition" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "condition" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "conditions" = [
  Cat "conditions" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "conditioned" = [
  Cat "conditioned" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_conditioned" = [
  Cat "have_conditioned" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_conditioned" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_conditioned" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_conditioned" = [
  Cat "has_conditioned" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_condition" = [
  Cat "will_condition" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "confess" = [
  Cat "confess" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "confess" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "confess" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "confess" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "confesses" = [
  Cat "confesses" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "confessed" = [
  Cat "confessed" "VP" [Past] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "have_confessed" = [
  Cat "have_confessed" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "have_confessed" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "have_confessed" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "has_confessed" = [
  Cat "has_confessed" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "will_confess" = [
  Cat "will_confess" "VP" [Fut] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]

lexicon "confide" = [
  Cat "confide" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [In] []],
  Cat "confide" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [In] []],
  Cat "confide" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [In] []],
  Cat "confide" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [In] []]
  ]
lexicon "confides" = [
  Cat "confides" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [In] []]
  ]
lexicon "confided" = [
  Cat "confided" "VP" [Past] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [In] []]
  ]
lexicon "have_confided" = [
  Cat "have_confided" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [In] []],
  Cat "have_confided" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [In] []],
  Cat "have_confided" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [In] []]
  ]
lexicon "has_confided" = [
  Cat "has_confided" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [In] []]
  ]
lexicon "will_confide" = [
  Cat "will_confide" "VP" [Fut] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [In] []]
  ]

lexicon "confirm" = [
  Cat "confirm" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "confirm" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "confirm" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "confirm" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "confirms" = [
  Cat "confirms" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "confirmed" = [
  Cat "confirmed" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_confirmed" = [
  Cat "have_confirmed" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_confirmed" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_confirmed" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_confirmed" = [
  Cat "has_confirmed" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_confirm" = [
  Cat "will_confirm" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "connect" = [
  Cat "connect" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "connect" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "connect" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "connect" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "connects" = [
  Cat "connects" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "connected" = [
  Cat "connected" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_connected" = [
  Cat "have_connected" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_connected" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_connected" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_connected" = [
  Cat "has_connected" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_connect" = [
  Cat "will_connect" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "consent" = [
  Cat "consent" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "consent" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "consent" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "consent" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "consents" = [
  Cat "consents" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "consented" = [
  Cat "consented" "VP" [Past] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "have_consented" = [
  Cat "have_consented" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "have_consented" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "have_consented" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "has_consented" = [
  Cat "has_consented" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "will_consent" = [
  Cat "will_consent" "VP" [Fut] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]

lexicon "consider" = [
  Cat "consider" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "consider" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "consider" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "consider" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "considers" = [
  Cat "considers" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "considered" = [
  Cat "considered" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_considered" = [
  Cat "have_considered" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_considered" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_considered" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_considered" = [
  Cat "has_considered" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_consider" = [
  Cat "will_consider" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "constrain" = [
  Cat "constrain" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "constrain" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "constrain" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "constrain" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "constrains" = [
  Cat "constrains" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "constrained" = [
  Cat "constrained" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_constrained" = [
  Cat "have_constrained" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_constrained" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_constrained" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_constrained" = [
  Cat "has_constrained" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_constrain" = [
  Cat "will_constrain" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "contemplate" = [
  Cat "contemplate" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "contemplate" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "contemplate" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "contemplate" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "contemplates" = [
  Cat "contemplates" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "contemplated" = [
  Cat "contemplated" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_contemplated" = [
  Cat "have_contemplated" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_contemplated" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_contemplated" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_contemplated" = [
  Cat "has_contemplated" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_contemplate" = [
  Cat "will_contemplate" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "contest" = [
  Cat "contest" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "contest" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "contest" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "contest" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "contests" = [
  Cat "contests" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "contested" = [
  Cat "contested" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_contested" = [
  Cat "have_contested" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_contested" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_contested" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_contested" = [
  Cat "has_contested" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_contest" = [
  Cat "will_contest" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "continue" = [
  Cat "continue" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "continue" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "continue" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "continue" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []],
  Cat "continue" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [With] []],
  Cat "continue" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [With] []],
  Cat "continue" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [With] []],
  Cat "continue" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [With] []]
  ]
lexicon "continues" = [
  Cat "continues" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []],
  Cat "continues" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [With] []]
  ]
lexicon "continued" = [
  Cat "continued" "VP" [Past] [Cat "_" "NP" [AccOrDat] []],
  Cat "continued" "VP" [Past] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [With] []]
  ]
lexicon "have_continued" = [
  Cat "have_continued" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_continued" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_continued" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_continued" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [With] []],
  Cat "have_continued" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [With] []],
  Cat "have_continued" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [With] []]
  ]
lexicon "has_continued" = [
  Cat "has_continued" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []],
  Cat "has_continued" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [With] []]
  ]
lexicon "will_continue" = [
  Cat "will_continue" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []],
  Cat "will_continue" "VP" [Fut] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [With] []]
  ]

lexicon "contract" = [
  Cat "contract" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "contract" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "contract" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "contract" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "contracts" = [
  Cat "contracts" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "contracted" = [
  Cat "contracted" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_contracted" = [
  Cat "have_contracted" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_contracted" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_contracted" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_contracted" = [
  Cat "has_contracted" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_contract" = [
  Cat "will_contract" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "contribute" = [
  Cat "contribute" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "contribute" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "contribute" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "contribute" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []],
  Cat "contribute" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "contribute" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "contribute" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "contribute" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "contributes" = [
  Cat "contributes" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []],
  Cat "contributes" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "contributed" = [
  Cat "contributed" "VP" [Past] [Cat "_" "NP" [AccOrDat] []],
  Cat "contributed" "VP" [Past] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "have_contributed" = [
  Cat "have_contributed" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_contributed" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_contributed" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_contributed" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "have_contributed" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "have_contributed" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "has_contributed" = [
  Cat "has_contributed" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []],
  Cat "has_contributed" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "will_contribute" = [
  Cat "will_contribute" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []],
  Cat "will_contribute" "VP" [Fut] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]

lexicon "contrive" = [
  Cat "contrive" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "contrive" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "contrive" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "contrive" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "contrives" = [
  Cat "contrives" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "contrived" = [
  Cat "contrived" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_contrived" = [
  Cat "have_contrived" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_contrived" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_contrived" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_contrived" = [
  Cat "has_contrived" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_contrive" = [
  Cat "will_contrive" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "control" = [
  Cat "control" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "control" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "control" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "control" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "controls" = [
  Cat "controls" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "controled" = [
  Cat "controled" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_controled" = [
  Cat "have_controled" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_controled" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_controled" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_controled" = [
  Cat "has_controled" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_control" = [
  Cat "will_control" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "convey" = [
  Cat "convey" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "convey" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "convey" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "convey" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []],
  Cat "convey" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "convey" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "convey" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "convey" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "conveys" = [
  Cat "conveys" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []],
  Cat "conveys" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "conveyed" = [
  Cat "conveyed" "VP" [Past] [Cat "_" "NP" [AccOrDat] []],
  Cat "conveyed" "VP" [Past] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "have_conveyed" = [
  Cat "have_conveyed" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_conveyed" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_conveyed" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_conveyed" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "have_conveyed" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "have_conveyed" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "has_conveyed" = [
  Cat "has_conveyed" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []],
  Cat "has_conveyed" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "will_convey" = [
  Cat "will_convey" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []],
  Cat "will_convey" "VP" [Fut] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]

lexicon "convince" = [
  Cat "convince" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "convince" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "convince" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "convince" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "convinces" = [
  Cat "convinces" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "convinced" = [
  Cat "convinced" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_convinced" = [
  Cat "have_convinced" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_convinced" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_convinced" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_convinced" = [
  Cat "has_convinced" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_convince" = [
  Cat "will_convince" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "corroborate" = [
  Cat "corroborate" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "corroborate" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "corroborate" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "corroborate" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "corroborates" = [
  Cat "corroborates" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "corroborated" = [
  Cat "corroborated" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_corroborated" = [
  Cat "have_corroborated" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_corroborated" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_corroborated" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_corroborated" = [
  Cat "has_corroborated" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_corroborate" = [
  Cat "will_corroborate" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "counsel" = [
  Cat "counsel" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "counsel" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "counsel" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "counsel" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "counsels" = [
  Cat "counsels" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "counseled" = [
  Cat "counseled" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_counseled" = [
  Cat "have_counseled" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_counseled" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_counseled" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_counseled" = [
  Cat "has_counseled" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_counsel" = [
  Cat "will_counsel" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "count" = [
  Cat "count" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "count" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "count" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "count" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []],
  Cat "count" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []],
  Cat "count" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []],
  Cat "count" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []],
  Cat "count" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []]
  ]
lexicon "counts" = [
  Cat "counts" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []],
  Cat "counts" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []]
  ]
lexicon "counted" = [
  Cat "counted" "VP" [Past] [Cat "_" "NP" [AccOrDat] []],
  Cat "counted" "VP" [Past] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []]
  ]
lexicon "have_counted" = [
  Cat "have_counted" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_counted" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_counted" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_counted" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []],
  Cat "have_counted" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []],
  Cat "have_counted" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []]
  ]
lexicon "has_counted" = [
  Cat "has_counted" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []],
  Cat "has_counted" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []]
  ]
lexicon "will_count" = [
  Cat "will_count" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []],
  Cat "will_count" "VP" [Fut] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []]
  ]

lexicon "cover" = [
  Cat "cover" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "cover" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "cover" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "cover" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "covers" = [
  Cat "covers" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "covered" = [
  Cat "covered" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_covered" = [
  Cat "have_covered" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_covered" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_covered" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_covered" = [
  Cat "has_covered" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_cover" = [
  Cat "will_cover" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "criticize" = [
  Cat "criticize" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "criticize" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "criticize" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "criticize" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "criticizes" = [
  Cat "criticizes" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "criticized" = [
  Cat "criticized" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_criticized" = [
  Cat "have_criticized" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_criticized" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_criticized" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_criticized" = [
  Cat "has_criticized" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_criticize" = [
  Cat "will_criticize" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "dabble" = [
  Cat "dabble" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [In] []],
  Cat "dabble" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [In] []],
  Cat "dabble" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [In] []],
  Cat "dabble" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [In] []]
  ]
lexicon "dabbles" = [
  Cat "dabbles" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [In] []]
  ]
lexicon "dabbled" = [
  Cat "dabbled" "VP" [Past] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [In] []]
  ]
lexicon "have_dabbled" = [
  Cat "have_dabbled" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [In] []],
  Cat "have_dabbled" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [In] []],
  Cat "have_dabbled" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [In] []]
  ]
lexicon "has_dabbled" = [
  Cat "has_dabbled" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [In] []]
  ]
lexicon "will_dabble" = [
  Cat "will_dabble" "VP" [Fut] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [In] []]
  ]

lexicon "damage" = [
  Cat "damage" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "damage" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "damage" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "damage" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "damages" = [
  Cat "damages" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "damaged" = [
  Cat "damaged" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_damaged" = [
  Cat "have_damaged" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_damaged" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_damaged" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_damaged" = [
  Cat "has_damaged" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_damage" = [
  Cat "will_damage" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "dance" = [
  Cat "dance" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "dance" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "dance" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "dance" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []],
  Cat "dance" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [With] []],
  Cat "dance" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [With] []],
  Cat "dance" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [With] []],
  Cat "dance" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [With] []]
  ]
lexicon "dances" = [
  Cat "dances" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []],
  Cat "dances" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [With] []]
  ]
lexicon "danced" = [
  Cat "danced" "VP" [Past] [Cat "_" "NP" [AccOrDat] []],
  Cat "danced" "VP" [Past] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [With] []]
  ]
lexicon "have_danced" = [
  Cat "have_danced" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_danced" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_danced" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_danced" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [With] []],
  Cat "have_danced" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [With] []],
  Cat "have_danced" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [With] []]
  ]
lexicon "has_danced" = [
  Cat "has_danced" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []],
  Cat "has_danced" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [With] []]
  ]
lexicon "will_dance" = [
  Cat "will_dance" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []],
  Cat "will_dance" "VP" [Fut] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [With] []]
  ]

lexicon "date" = [
  Cat "date" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "date" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "date" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "date" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "dates" = [
  Cat "dates" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "dated" = [
  Cat "dated" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_dated" = [
  Cat "have_dated" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_dated" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_dated" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_dated" = [
  Cat "has_dated" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_date" = [
  Cat "will_date" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "debate" = [
  Cat "debate" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "debate" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "debate" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "debate" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "debates" = [
  Cat "debates" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "debated" = [
  Cat "debated" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_debated" = [
  Cat "have_debated" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_debated" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_debated" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_debated" = [
  Cat "has_debated" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_debate" = [
  Cat "will_debate" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "decide" = [
  Cat "decide" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "decide" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "decide" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "decide" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "decides" = [
  Cat "decides" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "decided" = [
  Cat "decided" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_decided" = [
  Cat "have_decided" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_decided" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_decided" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_decided" = [
  Cat "has_decided" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_decide" = [
  Cat "will_decide" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "declare" = [
  Cat "declare" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "declare" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "declare" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "declare" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "declares" = [
  Cat "declares" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "declared" = [
  Cat "declared" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_declared" = [
  Cat "have_declared" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_declared" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_declared" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_declared" = [
  Cat "has_declared" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_declare" = [
  Cat "will_declare" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "decline" = [
  Cat "decline" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "decline" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "decline" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "decline" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "declines" = [
  Cat "declines" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "declined" = [
  Cat "declined" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_declined" = [
  Cat "have_declined" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_declined" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_declined" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_declined" = [
  Cat "has_declined" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_decline" = [
  Cat "will_decline" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "decorate" = [
  Cat "decorate" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "decorate" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "decorate" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "decorate" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "decorates" = [
  Cat "decorates" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "decorated" = [
  Cat "decorated" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_decorated" = [
  Cat "have_decorated" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_decorated" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_decorated" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_decorated" = [
  Cat "has_decorated" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_decorate" = [
  Cat "will_decorate" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "deduce" = [
  Cat "deduce" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "deduce" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "deduce" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "deduce" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "deduces" = [
  Cat "deduces" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "deduced" = [
  Cat "deduced" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_deduced" = [
  Cat "have_deduced" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_deduced" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_deduced" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_deduced" = [
  Cat "has_deduced" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_deduce" = [
  Cat "will_deduce" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "deduct" = [
  Cat "deduct" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "deduct" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "deduct" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "deduct" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "deducts" = [
  Cat "deducts" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "deducted" = [
  Cat "deducted" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_deducted" = [
  Cat "have_deducted" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_deducted" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_deducted" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_deducted" = [
  Cat "has_deducted" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_deduct" = [
  Cat "will_deduct" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "define" = [
  Cat "define" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "define" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "define" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "define" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "defines" = [
  Cat "defines" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "defined" = [
  Cat "defined" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_defined" = [
  Cat "have_defined" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_defined" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_defined" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_defined" = [
  Cat "has_defined" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_define" = [
  Cat "will_define" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "defy" = [
  Cat "defy" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "defy" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "defy" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "defy" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "defies" = [
  Cat "defies" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "defied" = [
  Cat "defied" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_defied" = [
  Cat "have_defied" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_defied" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_defied" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_defied" = [
  Cat "has_defied" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_defy" = [
  Cat "will_defy" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "delay" = [
  Cat "delay" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "delay" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "delay" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "delay" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "delays" = [
  Cat "delays" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "delayed" = [
  Cat "delayed" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_delayed" = [
  Cat "have_delayed" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_delayed" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_delayed" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_delayed" = [
  Cat "has_delayed" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_delay" = [
  Cat "will_delay" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "delegate" = [
  Cat "delegate" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "delegate" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "delegate" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "delegate" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "delegates" = [
  Cat "delegates" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "delegated" = [
  Cat "delegated" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_delegated" = [
  Cat "have_delegated" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_delegated" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_delegated" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_delegated" = [
  Cat "has_delegated" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_delegate" = [
  Cat "will_delegate" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "deliberate" = [
  Cat "deliberate" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []],
  Cat "deliberate" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []],
  Cat "deliberate" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []],
  Cat "deliberate" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []]
  ]
lexicon "deliberates" = [
  Cat "deliberates" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []]
  ]
lexicon "deliberated" = [
  Cat "deliberated" "VP" [Past] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []]
  ]
lexicon "have_deliberated" = [
  Cat "have_deliberated" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []],
  Cat "have_deliberated" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []],
  Cat "have_deliberated" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []]
  ]
lexicon "has_deliberated" = [
  Cat "has_deliberated" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []]
  ]
lexicon "will_deliberate" = [
  Cat "will_deliberate" "VP" [Fut] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []]
  ]

lexicon "delight" = [
  Cat "delight" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "delight" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "delight" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "delight" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "delights" = [
  Cat "delights" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "delighted" = [
  Cat "delighted" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_delighted" = [
  Cat "have_delighted" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_delighted" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_delighted" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_delighted" = [
  Cat "has_delighted" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_delight" = [
  Cat "will_delight" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "delineate" = [
  Cat "delineate" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "delineate" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "delineate" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "delineate" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "delineates" = [
  Cat "delineates" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "delineated" = [
  Cat "delineated" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_delineated" = [
  Cat "have_delineated" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_delineated" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_delineated" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_delineated" = [
  Cat "has_delineated" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_delineate" = [
  Cat "will_delineate" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "demand" = [
  Cat "demand" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "demand" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "demand" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "demand" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "demands" = [
  Cat "demands" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "demanded" = [
  Cat "demanded" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_demanded" = [
  Cat "have_demanded" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_demanded" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_demanded" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_demanded" = [
  Cat "has_demanded" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_demand" = [
  Cat "will_demand" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "demonstrate" = [
  Cat "demonstrate" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "demonstrate" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "demonstrate" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "demonstrate" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []],
  Cat "demonstrate" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "demonstrate" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "demonstrate" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "demonstrate" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "demonstrates" = [
  Cat "demonstrates" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []],
  Cat "demonstrates" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "demonstrated" = [
  Cat "demonstrated" "VP" [Past] [Cat "_" "NP" [AccOrDat] []],
  Cat "demonstrated" "VP" [Past] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "have_demonstrated" = [
  Cat "have_demonstrated" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_demonstrated" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_demonstrated" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_demonstrated" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "have_demonstrated" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "have_demonstrated" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "has_demonstrated" = [
  Cat "has_demonstrated" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []],
  Cat "has_demonstrated" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "will_demonstrate" = [
  Cat "will_demonstrate" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []],
  Cat "will_demonstrate" "VP" [Fut] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]

lexicon "denote" = [
  Cat "denote" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "denote" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "denote" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "denote" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "denotes" = [
  Cat "denotes" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "denoted" = [
  Cat "denoted" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_denoted" = [
  Cat "have_denoted" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_denoted" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_denoted" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_denoted" = [
  Cat "has_denoted" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_denote" = [
  Cat "will_denote" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "deny" = [
  Cat "deny" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "deny" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "deny" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "deny" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []],
  Cat "deny" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "deny" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "deny" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "deny" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "denies" = [
  Cat "denies" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []],
  Cat "denies" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "denied" = [
  Cat "denied" "VP" [Past] [Cat "_" "NP" [AccOrDat] []],
  Cat "denied" "VP" [Past] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_denied" = [
  Cat "have_denied" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_denied" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_denied" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_denied" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "have_denied" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "have_denied" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_denied" = [
  Cat "has_denied" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []],
  Cat "has_denied" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_deny" = [
  Cat "will_deny" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []],
  Cat "will_deny" "VP" [Fut] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "depend" = [
  Cat "depend" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []],
  Cat "depend" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []],
  Cat "depend" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []],
  Cat "depend" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []]
  ]
lexicon "depends" = [
  Cat "depends" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []]
  ]
lexicon "depended" = [
  Cat "depended" "VP" [Past] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []]
  ]
lexicon "have_depended" = [
  Cat "have_depended" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []],
  Cat "have_depended" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []],
  Cat "have_depended" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []]
  ]
lexicon "has_depended" = [
  Cat "has_depended" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []]
  ]
lexicon "will_depend" = [
  Cat "will_depend" "VP" [Fut] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []]
  ]

lexicon "depict" = [
  Cat "depict" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "depict" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "depict" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "depict" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "depicts" = [
  Cat "depicts" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "depicted" = [
  Cat "depicted" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_depicted" = [
  Cat "have_depicted" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_depicted" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_depicted" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_depicted" = [
  Cat "has_depicted" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_depict" = [
  Cat "will_depict" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "deploy" = [
  Cat "deploy" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "deploy" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "deploy" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "deploy" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "deploys" = [
  Cat "deploys" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "deployed" = [
  Cat "deployed" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_deployed" = [
  Cat "have_deployed" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_deployed" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_deployed" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_deployed" = [
  Cat "has_deployed" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_deploy" = [
  Cat "will_deploy" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "derive" = [
  Cat "derive" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [From] []],
  Cat "derive" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [From] []],
  Cat "derive" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [From] []],
  Cat "derive" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [From] []],
  Cat "derive" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "derive" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "derive" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "derive" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "derives" = [
  Cat "derives" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [From] []],
  Cat "derives" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "derived" = [
  Cat "derived" "VP" [Past] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [From] []],
  Cat "derived" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_derived" = [
  Cat "have_derived" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [From] []],
  Cat "have_derived" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [From] []],
  Cat "have_derived" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [From] []],
  Cat "have_derived" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_derived" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_derived" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_derived" = [
  Cat "has_derived" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [From] []],
  Cat "has_derived" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_derive" = [
  Cat "will_derive" "VP" [Fut] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [From] []],
  Cat "will_derive" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "descend" = [
  Cat "descend" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [From] []],
  Cat "descend" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [From] []],
  Cat "descend" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [From] []],
  Cat "descend" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [From] []],
  Cat "descend" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "descend" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "descend" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "descend" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "descends" = [
  Cat "descends" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [From] []],
  Cat "descends" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "descended" = [
  Cat "descended" "VP" [Past] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [From] []],
  Cat "descended" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_descended" = [
  Cat "have_descended" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [From] []],
  Cat "have_descended" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [From] []],
  Cat "have_descended" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [From] []],
  Cat "have_descended" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_descended" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_descended" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_descended" = [
  Cat "has_descended" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [From] []],
  Cat "has_descended" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_descend" = [
  Cat "will_descend" "VP" [Fut] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [From] []],
  Cat "will_descend" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "describe" = [
  Cat "describe" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "describe" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "describe" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "describe" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []],
  Cat "describe" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "describe" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "describe" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "describe" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "describes" = [
  Cat "describes" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []],
  Cat "describes" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "described" = [
  Cat "described" "VP" [Past] [Cat "_" "NP" [AccOrDat] []],
  Cat "described" "VP" [Past] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "have_described" = [
  Cat "have_described" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_described" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_described" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_described" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "have_described" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "have_described" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "has_described" = [
  Cat "has_described" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []],
  Cat "has_described" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "will_describe" = [
  Cat "will_describe" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []],
  Cat "will_describe" "VP" [Fut] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]

lexicon "descry" = [
  Cat "descry" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "descry" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "descry" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "descry" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "descries" = [
  Cat "descries" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "descried" = [
  Cat "descried" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_descried" = [
  Cat "have_descried" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_descried" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_descried" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_descried" = [
  Cat "has_descried" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_descry" = [
  Cat "will_descry" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "deserve" = [
  Cat "deserve" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "deserve" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "deserve" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "deserve" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "deserves" = [
  Cat "deserves" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "deserved" = [
  Cat "deserved" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_deserved" = [
  Cat "have_deserved" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_deserved" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_deserved" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_deserved" = [
  Cat "has_deserved" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_deserve" = [
  Cat "will_deserve" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "design" = [
  Cat "design" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "design" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "design" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "design" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "designs" = [
  Cat "designs" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "designed" = [
  Cat "designed" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_designed" = [
  Cat "have_designed" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_designed" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_designed" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_designed" = [
  Cat "has_designed" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_design" = [
  Cat "will_design" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "desire" = [
  Cat "desire" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "desire" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "desire" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "desire" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "desires" = [
  Cat "desires" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "desired" = [
  Cat "desired" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_desired" = [
  Cat "have_desired" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_desired" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_desired" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_desired" = [
  Cat "has_desired" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_desire" = [
  Cat "will_desire" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "despise" = [
  Cat "despise" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "despise" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "despise" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "despise" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "despises" = [
  Cat "despises" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "despised" = [
  Cat "despised" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_despised" = [
  Cat "have_despised" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_despised" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_despised" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_despised" = [
  Cat "has_despised" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_despise" = [
  Cat "will_despise" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "detect" = [
  Cat "detect" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "detect" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "detect" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "detect" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "detects" = [
  Cat "detects" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "detected" = [
  Cat "detected" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_detected" = [
  Cat "have_detected" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_detected" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_detected" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_detected" = [
  Cat "has_detected" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_detect" = [
  Cat "will_detect" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "determine" = [
  Cat "determine" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "determine" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "determine" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "determine" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "determines" = [
  Cat "determines" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "determined" = [
  Cat "determined" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_determined" = [
  Cat "have_determined" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_determined" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_determined" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_determined" = [
  Cat "has_determined" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_determine" = [
  Cat "will_determine" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "develop" = [
  Cat "develop" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "develop" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "develop" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "develop" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []],
  Cat "develop" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Into] []],
  Cat "develop" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Into] []],
  Cat "develop" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Into] []],
  Cat "develop" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Into] []]
  ]
lexicon "develops" = [
  Cat "develops" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []],
  Cat "develops" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Into] []]
  ]
lexicon "developed" = [
  Cat "developed" "VP" [Past] [Cat "_" "NP" [AccOrDat] []],
  Cat "developed" "VP" [Past] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Into] []]
  ]
lexicon "have_developed" = [
  Cat "have_developed" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_developed" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_developed" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_developed" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Into] []],
  Cat "have_developed" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Into] []],
  Cat "have_developed" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Into] []]
  ]
lexicon "has_developed" = [
  Cat "has_developed" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []],
  Cat "has_developed" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Into] []]
  ]
lexicon "will_develop" = [
  Cat "will_develop" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []],
  Cat "will_develop" "VP" [Fut] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Into] []]
  ]

lexicon "devise" = [
  Cat "devise" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "devise" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "devise" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "devise" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "devises" = [
  Cat "devises" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "devised" = [
  Cat "devised" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_devised" = [
  Cat "have_devised" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_devised" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_devised" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_devised" = [
  Cat "has_devised" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_devise" = [
  Cat "will_devise" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "devote" = [
  Cat "devote" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "devote" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "devote" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "devote" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "devotes" = [
  Cat "devotes" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "devoted" = [
  Cat "devoted" "VP" [Past] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "have_devoted" = [
  Cat "have_devoted" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "have_devoted" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "have_devoted" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "has_devoted" = [
  Cat "has_devoted" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "will_devote" = [
  Cat "will_devote" "VP" [Fut] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]

lexicon "diagnose" = [
  Cat "diagnose" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "diagnose" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "diagnose" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "diagnose" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "diagnoses" = [
  Cat "diagnoses" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "diagnosed" = [
  Cat "diagnosed" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_diagnosed" = [
  Cat "have_diagnosed" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_diagnosed" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_diagnosed" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_diagnosed" = [
  Cat "has_diagnosed" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_diagnose" = [
  Cat "will_diagnose" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "dictate" = [
  Cat "dictate" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "dictate" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "dictate" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "dictate" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []],
  Cat "dictate" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "dictate" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "dictate" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "dictate" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "dictates" = [
  Cat "dictates" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []],
  Cat "dictates" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "dictated" = [
  Cat "dictated" "VP" [Past] [Cat "_" "NP" [AccOrDat] []],
  Cat "dictated" "VP" [Past] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "have_dictated" = [
  Cat "have_dictated" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_dictated" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_dictated" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_dictated" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "have_dictated" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "have_dictated" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "has_dictated" = [
  Cat "has_dictated" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []],
  Cat "has_dictated" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "will_dictate" = [
  Cat "will_dictate" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []],
  Cat "will_dictate" "VP" [Fut] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]

lexicon "dig" = [
  Cat "dig" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "dig" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "dig" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "dig" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []],
  Cat "dig" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "dig" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "dig" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "dig" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "digs" = [
  Cat "digs" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []],
  Cat "digs" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "dug" = [
  Cat "dug" "VP" [Past] [Cat "_" "NP" [AccOrDat] []],
  Cat "dug" "VP" [Past] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_dug" = [
  Cat "have_dug" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_dug" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_dug" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_dug" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "have_dug" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "have_dug" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_dug" = [
  Cat "has_dug" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []],
  Cat "has_dug" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_dig" = [
  Cat "will_dig" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []],
  Cat "will_dig" "VP" [Fut] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "direct" = [
  Cat "direct" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "direct" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "direct" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "direct" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "directs" = [
  Cat "directs" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "directed" = [
  Cat "directed" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_directed" = [
  Cat "have_directed" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_directed" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_directed" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_directed" = [
  Cat "has_directed" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_direct" = [
  Cat "will_direct" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "disallow" = [
  Cat "disallow" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "disallow" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "disallow" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "disallow" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "disallows" = [
  Cat "disallows" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "disallowed" = [
  Cat "disallowed" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_disallowed" = [
  Cat "have_disallowed" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_disallowed" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_disallowed" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_disallowed" = [
  Cat "has_disallowed" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_disallow" = [
  Cat "will_disallow" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "disapprove" = [
  Cat "disapprove" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []],
  Cat "disapprove" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []],
  Cat "disapprove" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []],
  Cat "disapprove" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []]
  ]
lexicon "disapproves" = [
  Cat "disapproves" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []]
  ]
lexicon "disapproved" = [
  Cat "disapproved" "VP" [Past] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []]
  ]
lexicon "have_disapproved" = [
  Cat "have_disapproved" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []],
  Cat "have_disapproved" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []],
  Cat "have_disapproved" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []]
  ]
lexicon "has_disapproved" = [
  Cat "has_disapproved" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []]
  ]
lexicon "will_disapprove" = [
  Cat "will_disapprove" "VP" [Fut] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []]
  ]

lexicon "disbelieve" = [
  Cat "disbelieve" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "disbelieve" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "disbelieve" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "disbelieve" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "disbelieves" = [
  Cat "disbelieves" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "disbelieved" = [
  Cat "disbelieved" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_disbelieved" = [
  Cat "have_disbelieved" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_disbelieved" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_disbelieved" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_disbelieved" = [
  Cat "has_disbelieved" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_disbelieve" = [
  Cat "will_disbelieve" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "discern" = [
  Cat "discern" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "discern" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "discern" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "discern" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "discerns" = [
  Cat "discerns" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "discerned" = [
  Cat "discerned" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_discerned" = [
  Cat "have_discerned" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_discerned" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_discerned" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_discerned" = [
  Cat "has_discerned" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_discern" = [
  Cat "will_discern" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "discipline" = [
  Cat "discipline" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "discipline" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "discipline" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "discipline" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "disciplines" = [
  Cat "disciplines" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "disciplined" = [
  Cat "disciplined" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_disciplined" = [
  Cat "have_disciplined" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_disciplined" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_disciplined" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_disciplined" = [
  Cat "has_disciplined" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_discipline" = [
  Cat "will_discipline" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "disclose" = [
  Cat "disclose" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "disclose" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "disclose" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "disclose" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "discloses" = [
  Cat "discloses" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "disclosed" = [
  Cat "disclosed" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_disclosed" = [
  Cat "have_disclosed" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_disclosed" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_disclosed" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_disclosed" = [
  Cat "has_disclosed" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_disclose" = [
  Cat "will_disclose" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "discontinue" = [
  Cat "discontinue" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "discontinue" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "discontinue" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "discontinue" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "discontinues" = [
  Cat "discontinues" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "discontinued" = [
  Cat "discontinued" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_discontinued" = [
  Cat "have_discontinued" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_discontinued" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_discontinued" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_discontinued" = [
  Cat "has_discontinued" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_discontinue" = [
  Cat "will_discontinue" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "discover" = [
  Cat "discover" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "discover" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "discover" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "discover" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "discovers" = [
  Cat "discovers" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "discovered" = [
  Cat "discovered" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_discovered" = [
  Cat "have_discovered" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_discovered" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_discovered" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_discovered" = [
  Cat "has_discovered" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_discover" = [
  Cat "will_discover" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "discuss" = [
  Cat "discuss" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "discuss" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "discuss" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "discuss" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "discusses" = [
  Cat "discusses" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "discussed" = [
  Cat "discussed" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_discussed" = [
  Cat "have_discussed" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_discussed" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_discussed" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_discussed" = [
  Cat "has_discussed" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_discuss" = [
  Cat "will_discuss" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "disdain" = [
  Cat "disdain" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "disdain" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "disdain" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "disdain" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "disdains" = [
  Cat "disdains" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "disdained" = [
  Cat "disdained" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_disdained" = [
  Cat "have_disdained" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_disdained" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_disdained" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_disdained" = [
  Cat "has_disdained" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_disdain" = [
  Cat "will_disdain" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "disgrace" = [
  Cat "disgrace" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "disgrace" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "disgrace" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "disgrace" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "disgraces" = [
  Cat "disgraces" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "disgraced" = [
  Cat "disgraced" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_disgraced" = [
  Cat "have_disgraced" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_disgraced" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_disgraced" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_disgraced" = [
  Cat "has_disgraced" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_disgrace" = [
  Cat "will_disgrace" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "disgust" = [
  Cat "disgust" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "disgust" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "disgust" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "disgust" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "disgusts" = [
  Cat "disgusts" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "disgusted" = [
  Cat "disgusted" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_disgusted" = [
  Cat "have_disgusted" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_disgusted" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_disgusted" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_disgusted" = [
  Cat "has_disgusted" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_disgust" = [
  Cat "will_disgust" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "dislike" = [
  Cat "dislike" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "dislike" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "dislike" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "dislike" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "dislikes" = [
  Cat "dislikes" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "disliked" = [
  Cat "disliked" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_disliked" = [
  Cat "have_disliked" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_disliked" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_disliked" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_disliked" = [
  Cat "has_disliked" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_dislike" = [
  Cat "will_dislike" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "dismiss" = [
  Cat "dismiss" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "dismiss" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "dismiss" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "dismiss" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "dismisses" = [
  Cat "dismisses" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "dismissed" = [
  Cat "dismissed" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_dismissed" = [
  Cat "have_dismissed" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_dismissed" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_dismissed" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_dismissed" = [
  Cat "has_dismissed" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_dismiss" = [
  Cat "will_dismiss" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "disobey" = [
  Cat "disobey" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "disobey" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "disobey" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "disobey" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "disobeys" = [
  Cat "disobeys" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "disobeyed" = [
  Cat "disobeyed" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_disobeyed" = [
  Cat "have_disobeyed" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_disobeyed" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_disobeyed" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_disobeyed" = [
  Cat "has_disobeyed" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_disobey" = [
  Cat "will_disobey" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "display" = [
  Cat "display" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "display" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "display" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "display" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "displays" = [
  Cat "displays" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "displayed" = [
  Cat "displayed" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_displayed" = [
  Cat "have_displayed" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_displayed" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_displayed" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_displayed" = [
  Cat "has_displayed" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_display" = [
  Cat "will_display" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "disose" = [
  Cat "disose" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Of] []],
  Cat "disose" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Of] []],
  Cat "disose" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Of] []],
  Cat "disose" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Of] []]
  ]
lexicon "disoses" = [
  Cat "disoses" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Of] []]
  ]
lexicon "disosed" = [
  Cat "disosed" "VP" [Past] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Of] []]
  ]
lexicon "have_disosed" = [
  Cat "have_disosed" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Of] []],
  Cat "have_disosed" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Of] []],
  Cat "have_disosed" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Of] []]
  ]
lexicon "has_disosed" = [
  Cat "has_disosed" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Of] []]
  ]
lexicon "will_disose" = [
  Cat "will_disose" "VP" [Fut] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Of] []]
  ]

lexicon "dispute" = [
  Cat "dispute" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "dispute" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "dispute" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "dispute" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []],
  Cat "dispute" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Of] []],
  Cat "dispute" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Of] []],
  Cat "dispute" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Of] []],
  Cat "dispute" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Of] []]
  ]
lexicon "disputes" = [
  Cat "disputes" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []],
  Cat "disputes" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Of] []]
  ]
lexicon "disputed" = [
  Cat "disputed" "VP" [Past] [Cat "_" "NP" [AccOrDat] []],
  Cat "disputed" "VP" [Past] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Of] []]
  ]
lexicon "have_disputed" = [
  Cat "have_disputed" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_disputed" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_disputed" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_disputed" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Of] []],
  Cat "have_disputed" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Of] []],
  Cat "have_disputed" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Of] []]
  ]
lexicon "has_disputed" = [
  Cat "has_disputed" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []],
  Cat "has_disputed" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Of] []]
  ]
lexicon "will_dispute" = [
  Cat "will_dispute" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []],
  Cat "will_dispute" "VP" [Fut] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Of] []]
  ]

lexicon "disregard" = [
  Cat "disregard" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "disregard" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "disregard" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "disregard" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "disregards" = [
  Cat "disregards" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "disregarded" = [
  Cat "disregarded" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_disregarded" = [
  Cat "have_disregarded" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_disregarded" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_disregarded" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_disregarded" = [
  Cat "has_disregarded" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_disregard" = [
  Cat "will_disregard" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "disturb" = [
  Cat "disturb" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "disturb" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "disturb" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "disturb" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "disturbs" = [
  Cat "disturbs" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "disturbed" = [
  Cat "disturbed" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_disturbed" = [
  Cat "have_disturbed" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_disturbed" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_disturbed" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_disturbed" = [
  Cat "has_disturbed" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_disturb" = [
  Cat "will_disturb" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "divine" = [
  Cat "divine" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "divine" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "divine" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "divine" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "divines" = [
  Cat "divines" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "divined" = [
  Cat "divined" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_divined" = [
  Cat "have_divined" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_divined" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_divined" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_divined" = [
  Cat "has_divined" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_divine" = [
  Cat "will_divine" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "divulge" = [
  Cat "divulge" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "divulge" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "divulge" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "divulge" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "divulges" = [
  Cat "divulges" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "divulged" = [
  Cat "divulged" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_divulged" = [
  Cat "have_divulged" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_divulged" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_divulged" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_divulged" = [
  Cat "has_divulged" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_divulge" = [
  Cat "will_divulge" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "do" = [
  Cat "do" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "do" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "do" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "do" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []],
  Cat "do" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "do" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "do" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "do" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "does" = [
  Cat "does" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []],
  Cat "does" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "did" = [
  Cat "did" "VP" [Past] [Cat "_" "NP" [AccOrDat] []],
  Cat "did" "VP" [Past] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_done" = [
  Cat "have_done" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_done" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_done" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_done" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "have_done" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "have_done" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_done" = [
  Cat "has_done" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []],
  Cat "has_done" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_do" = [
  Cat "will_do" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []],
  Cat "will_do" "VP" [Fut] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "document" = [
  Cat "document" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "document" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "document" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "document" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "documents" = [
  Cat "documents" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "documented" = [
  Cat "documented" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_documented" = [
  Cat "have_documented" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_documented" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_documented" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_documented" = [
  Cat "has_documented" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_document" = [
  Cat "will_document" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "doubt" = [
  Cat "doubt" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "doubt" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "doubt" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "doubt" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "doubts" = [
  Cat "doubts" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "doubted" = [
  Cat "doubted" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_doubted" = [
  Cat "have_doubted" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_doubted" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_doubted" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_doubted" = [
  Cat "has_doubted" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_doubt" = [
  Cat "will_doubt" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "draw" = [
  Cat "draw" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "draw" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "draw" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "draw" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "draws" = [
  Cat "draws" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "drew" = [
  Cat "drew" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_drawn" = [
  Cat "have_drawn" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_drawn" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_drawn" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_drawn" = [
  Cat "has_drawn" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_draw" = [
  Cat "will_draw" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "dread" = [
  Cat "dread" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "dread" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "dread" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "dread" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "dreads" = [
  Cat "dreads" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "dreaded" = [
  Cat "dreaded" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_dreaded" = [
  Cat "have_dreaded" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_dreaded" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_dreaded" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_dreaded" = [
  Cat "has_dreaded" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_dread" = [
  Cat "will_dread" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "drill" = [
  Cat "drill" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "drill" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "drill" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "drill" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "drills" = [
  Cat "drills" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "drilled" = [
  Cat "drilled" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_drilled" = [
  Cat "have_drilled" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_drilled" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_drilled" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_drilled" = [
  Cat "has_drilled" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_drill" = [
  Cat "will_drill" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "drive" = [
  Cat "drive" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "drive" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "drive" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "drive" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "drives" = [
  Cat "drives" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "drove" = [
  Cat "drove" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_driven" = [
  Cat "have_driven" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_driven" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_driven" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_driven" = [
  Cat "has_driven" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_drive" = [
  Cat "will_drive" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "dry" = [
  Cat "dry" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "dry" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "dry" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "dry" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "dries" = [
  Cat "dries" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "dried" = [
  Cat "dried" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_dried" = [
  Cat "have_dried" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_dried" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_dried" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_dried" = [
  Cat "has_dried" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_dry" = [
  Cat "will_dry" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "duplicate" = [
  Cat "duplicate" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "duplicate" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "duplicate" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "duplicate" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "duplicates" = [
  Cat "duplicates" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "duplicated" = [
  Cat "duplicated" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_duplicated" = [
  Cat "have_duplicated" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_duplicated" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_duplicated" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_duplicated" = [
  Cat "has_duplicated" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_duplicate" = [
  Cat "will_duplicate" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "earn" = [
  Cat "earn" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "earn" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "earn" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "earn" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "earns" = [
  Cat "earns" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "earned" = [
  Cat "earned" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_earned" = [
  Cat "have_earned" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_earned" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_earned" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_earned" = [
  Cat "has_earned" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_earn" = [
  Cat "will_earn" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "eat" = [
  Cat "eat" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "eat" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "eat" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "eat" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "eats" = [
  Cat "eats" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "ate" = [
  Cat "ate" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_eaten" = [
  Cat "have_eaten" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_eaten" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_eaten" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_eaten" = [
  Cat "has_eaten" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_eat" = [
  Cat "will_eat" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "echo" = [
  Cat "echo" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "echo" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "echo" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "echo" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "echoes" = [
  Cat "echoes" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "echoed" = [
  Cat "echoed" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_echoed" = [
  Cat "have_echoed" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_echoed" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_echoed" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_echoed" = [
  Cat "has_echoed" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_echo" = [
  Cat "will_echo" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "educate" = [
  Cat "educate" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "educate" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "educate" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "educate" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "educates" = [
  Cat "educates" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "educated" = [
  Cat "educated" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_educated" = [
  Cat "have_educated" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_educated" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_educated" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_educated" = [
  Cat "has_educated" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_educate" = [
  Cat "will_educate" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "elaborate" = [
  Cat "elaborate" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []],
  Cat "elaborate" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []],
  Cat "elaborate" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []],
  Cat "elaborate" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []]
  ]
lexicon "elaborates" = [
  Cat "elaborates" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []]
  ]
lexicon "elaborated" = [
  Cat "elaborated" "VP" [Past] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []]
  ]
lexicon "have_elaborated" = [
  Cat "have_elaborated" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []],
  Cat "have_elaborated" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []],
  Cat "have_elaborated" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []]
  ]
lexicon "has_elaborated" = [
  Cat "has_elaborated" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []]
  ]
lexicon "will_elaborate" = [
  Cat "will_elaborate" "VP" [Fut] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []]
  ]

lexicon "elect" = [
  Cat "elect" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "elect" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "elect" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "elect" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "elects" = [
  Cat "elects" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "elected" = [
  Cat "elected" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_elected" = [
  Cat "have_elected" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_elected" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_elected" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_elected" = [
  Cat "has_elected" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_elect" = [
  Cat "will_elect" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "elicit" = [
  Cat "elicit" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "elicit" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "elicit" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "elicit" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "elicits" = [
  Cat "elicits" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "elicited" = [
  Cat "elicited" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_elicited" = [
  Cat "have_elicited" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_elicited" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_elicited" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_elicited" = [
  Cat "has_elicited" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_elicit" = [
  Cat "will_elicit" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "embarrass" = [
  Cat "embarrass" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "embarrass" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "embarrass" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "embarrass" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "embarrasses" = [
  Cat "embarrasses" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "embarrassed" = [
  Cat "embarrassed" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_embarrassed" = [
  Cat "have_embarrassed" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_embarrassed" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_embarrassed" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_embarrassed" = [
  Cat "has_embarrassed" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_embarrass" = [
  Cat "will_embarrass" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "embolden" = [
  Cat "embolden" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "embolden" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "embolden" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "embolden" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "emboldens" = [
  Cat "emboldens" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "emboldened" = [
  Cat "emboldened" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_emboldened" = [
  Cat "have_emboldened" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_emboldened" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_emboldened" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_emboldened" = [
  Cat "has_emboldened" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_embolden" = [
  Cat "will_embolden" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "emphasize" = [
  Cat "emphasize" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "emphasize" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "emphasize" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "emphasize" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "emphasizes" = [
  Cat "emphasizes" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "emphasized" = [
  Cat "emphasized" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_emphasized" = [
  Cat "have_emphasized" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_emphasized" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_emphasized" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_emphasized" = [
  Cat "has_emphasized" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_emphasize" = [
  Cat "will_emphasize" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "employ" = [
  Cat "employ" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "employ" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "employ" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "employ" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "employs" = [
  Cat "employs" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "employed" = [
  Cat "employed" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_employed" = [
  Cat "have_employed" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_employed" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_employed" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_employed" = [
  Cat "has_employed" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_employ" = [
  Cat "will_employ" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "empower" = [
  Cat "empower" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "empower" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "empower" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "empower" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "empowers" = [
  Cat "empowers" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "empowered" = [
  Cat "empowered" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_empowered" = [
  Cat "have_empowered" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_empowered" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_empowered" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_empowered" = [
  Cat "has_empowered" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_empower" = [
  Cat "will_empower" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "empty" = [
  Cat "empty" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "empty" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "empty" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "empty" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "empties" = [
  Cat "empties" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "emptied" = [
  Cat "emptied" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_emptied" = [
  Cat "have_emptied" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_emptied" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_emptied" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_emptied" = [
  Cat "has_emptied" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_empty" = [
  Cat "will_empty" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "enact" = [
  Cat "enact" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "enact" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "enact" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "enact" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "enacts" = [
  Cat "enacts" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "enacted" = [
  Cat "enacted" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_enacted" = [
  Cat "have_enacted" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_enacted" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_enacted" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_enacted" = [
  Cat "has_enacted" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_enact" = [
  Cat "will_enact" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "encourage" = [
  Cat "encourage" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "encourage" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "encourage" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "encourage" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "encourages" = [
  Cat "encourages" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "encouraged" = [
  Cat "encouraged" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_encouraged" = [
  Cat "have_encouraged" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_encouraged" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_encouraged" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_encouraged" = [
  Cat "has_encouraged" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_encourage" = [
  Cat "will_encourage" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "endure" = [
  Cat "endure" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "endure" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "endure" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "endure" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "endures" = [
  Cat "endures" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "endured" = [
  Cat "endured" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_endured" = [
  Cat "have_endured" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_endured" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_endured" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_endured" = [
  Cat "has_endured" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_endure" = [
  Cat "will_endure" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "engage" = [
  Cat "engage" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [In] []],
  Cat "engage" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [In] []],
  Cat "engage" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [In] []],
  Cat "engage" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [In] []],
  Cat "engage" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "engage" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "engage" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "engage" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "engages" = [
  Cat "engages" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [In] []],
  Cat "engages" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "engaged" = [
  Cat "engaged" "VP" [Past] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [In] []],
  Cat "engaged" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_engaged" = [
  Cat "have_engaged" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [In] []],
  Cat "have_engaged" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [In] []],
  Cat "have_engaged" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [In] []],
  Cat "have_engaged" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_engaged" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_engaged" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_engaged" = [
  Cat "has_engaged" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [In] []],
  Cat "has_engaged" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_engage" = [
  Cat "will_engage" "VP" [Fut] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [In] []],
  Cat "will_engage" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "enjoy" = [
  Cat "enjoy" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "enjoy" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "enjoy" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "enjoy" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "enjoys" = [
  Cat "enjoys" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "enjoyed" = [
  Cat "enjoyed" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_enjoyed" = [
  Cat "have_enjoyed" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_enjoyed" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_enjoyed" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_enjoyed" = [
  Cat "has_enjoyed" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_enjoy" = [
  Cat "will_enjoy" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "enlarge" = [
  Cat "enlarge" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "enlarge" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "enlarge" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "enlarge" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "enlarges" = [
  Cat "enlarges" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "enlarged" = [
  Cat "enlarged" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_enlarged" = [
  Cat "have_enlarged" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_enlarged" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_enlarged" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_enlarged" = [
  Cat "has_enlarged" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_enlarge" = [
  Cat "will_enlarge" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "enlist" = [
  Cat "enlist" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [In] []],
  Cat "enlist" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [In] []],
  Cat "enlist" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [In] []],
  Cat "enlist" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [In] []],
  Cat "enlist" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "enlist" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "enlist" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "enlist" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "enlists" = [
  Cat "enlists" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [In] []],
  Cat "enlists" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "enlisted" = [
  Cat "enlisted" "VP" [Past] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [In] []],
  Cat "enlisted" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_enlisted" = [
  Cat "have_enlisted" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [In] []],
  Cat "have_enlisted" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [In] []],
  Cat "have_enlisted" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [In] []],
  Cat "have_enlisted" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_enlisted" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_enlisted" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_enlisted" = [
  Cat "has_enlisted" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [In] []],
  Cat "has_enlisted" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_enlist" = [
  Cat "will_enlist" "VP" [Fut] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [In] []],
  Cat "will_enlist" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "ensure" = [
  Cat "ensure" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "ensure" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "ensure" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "ensure" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "ensures" = [
  Cat "ensures" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "ensured" = [
  Cat "ensured" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_ensured" = [
  Cat "have_ensured" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_ensured" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_ensured" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_ensured" = [
  Cat "has_ensured" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_ensure" = [
  Cat "will_ensure" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "entail" = [
  Cat "entail" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "entail" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "entail" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "entail" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "entails" = [
  Cat "entails" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "entailed" = [
  Cat "entailed" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_entailed" = [
  Cat "have_entailed" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_entailed" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_entailed" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_entailed" = [
  Cat "has_entailed" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_entail" = [
  Cat "will_entail" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "enter" = [
  Cat "enter" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "enter" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "enter" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "enter" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "enters" = [
  Cat "enters" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "entered" = [
  Cat "entered" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_entered" = [
  Cat "have_entered" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_entered" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_entered" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_entered" = [
  Cat "has_entered" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_enter" = [
  Cat "will_enter" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "entice" = [
  Cat "entice" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "entice" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "entice" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "entice" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "entices" = [
  Cat "entices" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "enticed" = [
  Cat "enticed" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_enticed" = [
  Cat "have_enticed" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_enticed" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_enticed" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_enticed" = [
  Cat "has_enticed" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_entice" = [
  Cat "will_entice" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "enunciate" = [
  Cat "enunciate" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "enunciate" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "enunciate" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "enunciate" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "enunciates" = [
  Cat "enunciates" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "enunciated" = [
  Cat "enunciated" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_enunciated" = [
  Cat "have_enunciated" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_enunciated" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_enunciated" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_enunciated" = [
  Cat "has_enunciated" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_enunciate" = [
  Cat "will_enunciate" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "envisage" = [
  Cat "envisage" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "envisage" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "envisage" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "envisage" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "envisages" = [
  Cat "envisages" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "envisaged" = [
  Cat "envisaged" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_envisaged" = [
  Cat "have_envisaged" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_envisaged" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_envisaged" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_envisaged" = [
  Cat "has_envisaged" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_envisage" = [
  Cat "will_envisage" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "envy" = [
  Cat "envy" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "envy" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "envy" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "envy" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []],
  Cat "envy" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "envy" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "envy" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "envy" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "envies" = [
  Cat "envies" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []],
  Cat "envies" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "envied" = [
  Cat "envied" "VP" [Past] [Cat "_" "NP" [AccOrDat] []],
  Cat "envied" "VP" [Past] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_envied" = [
  Cat "have_envied" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_envied" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_envied" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_envied" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "have_envied" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "have_envied" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_envied" = [
  Cat "has_envied" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []],
  Cat "has_envied" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_envy" = [
  Cat "will_envy" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []],
  Cat "will_envy" "VP" [Fut] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "escape" = [
  Cat "escape" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "escape" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "escape" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "escape" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "escapes" = [
  Cat "escapes" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "escaped" = [
  Cat "escaped" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_escaped" = [
  Cat "have_escaped" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_escaped" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_escaped" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_escaped" = [
  Cat "has_escaped" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_escape" = [
  Cat "will_escape" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "establish" = [
  Cat "establish" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "establish" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "establish" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "establish" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "establishes" = [
  Cat "establishes" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "established" = [
  Cat "established" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_established" = [
  Cat "have_established" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_established" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_established" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_established" = [
  Cat "has_established" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_establish" = [
  Cat "will_establish" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "esteem" = [
  Cat "esteem" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "esteem" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "esteem" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "esteem" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "esteems" = [
  Cat "esteems" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "esteemed" = [
  Cat "esteemed" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_esteemed" = [
  Cat "have_esteemed" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_esteemed" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_esteemed" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_esteemed" = [
  Cat "has_esteemed" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_esteem" = [
  Cat "will_esteem" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "estimate" = [
  Cat "estimate" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "estimate" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "estimate" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "estimate" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "estimates" = [
  Cat "estimates" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "estimated" = [
  Cat "estimated" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_estimated" = [
  Cat "have_estimated" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_estimated" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_estimated" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_estimated" = [
  Cat "has_estimated" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_estimate" = [
  Cat "will_estimate" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "evade" = [
  Cat "evade" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "evade" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "evade" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "evade" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "evades" = [
  Cat "evades" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "evaded" = [
  Cat "evaded" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_evaded" = [
  Cat "have_evaded" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_evaded" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_evaded" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_evaded" = [
  Cat "has_evaded" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_evade" = [
  Cat "will_evade" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "examine" = [
  Cat "examine" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "examine" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "examine" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "examine" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "examines" = [
  Cat "examines" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "examined" = [
  Cat "examined" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_examined" = [
  Cat "have_examined" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_examined" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_examined" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_examined" = [
  Cat "has_examined" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_examine" = [
  Cat "will_examine" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "exclude" = [
  Cat "exclude" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "exclude" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "exclude" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "exclude" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "excludes" = [
  Cat "excludes" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "excluded" = [
  Cat "excluded" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_excluded" = [
  Cat "have_excluded" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_excluded" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_excluded" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_excluded" = [
  Cat "has_excluded" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_exclude" = [
  Cat "will_exclude" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "excuse" = [
  Cat "excuse" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "excuse" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "excuse" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "excuse" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "excuses" = [
  Cat "excuses" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "excused" = [
  Cat "excused" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_excused" = [
  Cat "have_excused" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_excused" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_excused" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_excused" = [
  Cat "has_excused" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_excuse" = [
  Cat "will_excuse" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "exhale" = [
  Cat "exhale" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "exhale" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "exhale" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "exhale" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "exhales" = [
  Cat "exhales" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "exhaled" = [
  Cat "exhaled" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_exhaled" = [
  Cat "have_exhaled" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_exhaled" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_exhaled" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_exhaled" = [
  Cat "has_exhaled" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_exhale" = [
  Cat "will_exhale" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "exhibit" = [
  Cat "exhibit" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "exhibit" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "exhibit" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "exhibit" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "exhibits" = [
  Cat "exhibits" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "exhibited" = [
  Cat "exhibited" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_exhibited" = [
  Cat "have_exhibited" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_exhibited" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_exhibited" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_exhibited" = [
  Cat "has_exhibited" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_exhibit" = [
  Cat "will_exhibit" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "expect" = [
  Cat "expect" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "expect" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "expect" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "expect" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "expects" = [
  Cat "expects" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "expected" = [
  Cat "expected" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_expected" = [
  Cat "have_expected" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_expected" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_expected" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_expected" = [
  Cat "has_expected" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_expect" = [
  Cat "will_expect" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "explain" = [
  Cat "explain" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "explain" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "explain" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "explain" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []],
  Cat "explain" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "explain" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "explain" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "explain" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "explains" = [
  Cat "explains" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []],
  Cat "explains" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "explained" = [
  Cat "explained" "VP" [Past] [Cat "_" "NP" [AccOrDat] []],
  Cat "explained" "VP" [Past] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "have_explained" = [
  Cat "have_explained" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_explained" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_explained" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_explained" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "have_explained" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "have_explained" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "has_explained" = [
  Cat "has_explained" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []],
  Cat "has_explained" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "will_explain" = [
  Cat "will_explain" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []],
  Cat "will_explain" "VP" [Fut] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]

lexicon "explode" = [
  Cat "explode" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "explode" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "explode" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "explode" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "explodes" = [
  Cat "explodes" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "exploded" = [
  Cat "exploded" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_exploded" = [
  Cat "have_exploded" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_exploded" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_exploded" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_exploded" = [
  Cat "has_exploded" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_explode" = [
  Cat "will_explode" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "exploit" = [
  Cat "exploit" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "exploit" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "exploit" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "exploit" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "exploits" = [
  Cat "exploits" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "exploited" = [
  Cat "exploited" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_exploited" = [
  Cat "have_exploited" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_exploited" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_exploited" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_exploited" = [
  Cat "has_exploited" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_exploit" = [
  Cat "will_exploit" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "explore" = [
  Cat "explore" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "explore" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "explore" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "explore" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "explores" = [
  Cat "explores" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "explored" = [
  Cat "explored" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_explored" = [
  Cat "have_explored" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_explored" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_explored" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_explored" = [
  Cat "has_explored" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_explore" = [
  Cat "will_explore" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "express" = [
  Cat "express" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "express" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "express" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "express" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "expresses" = [
  Cat "expresses" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "expressed" = [
  Cat "expressed" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_expressed" = [
  Cat "have_expressed" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_expressed" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_expressed" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_expressed" = [
  Cat "has_expressed" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_express" = [
  Cat "will_express" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "extend" = [
  Cat "extend" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "extend" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "extend" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "extend" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "extends" = [
  Cat "extends" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "extended" = [
  Cat "extended" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_extended" = [
  Cat "have_extended" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_extended" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_extended" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_extended" = [
  Cat "has_extended" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_extend" = [
  Cat "will_extend" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "extrapolate" = [
  Cat "extrapolate" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "extrapolate" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "extrapolate" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "extrapolate" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "extrapolates" = [
  Cat "extrapolates" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "extrapolated" = [
  Cat "extrapolated" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_extrapolated" = [
  Cat "have_extrapolated" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_extrapolated" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_extrapolated" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_extrapolated" = [
  Cat "has_extrapolated" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_extrapolate" = [
  Cat "will_extrapolate" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "fail" = [
  Cat "fail" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "fail" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "fail" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "fail" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "fails" = [
  Cat "fails" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "failed" = [
  Cat "failed" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_failed" = [
  Cat "have_failed" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_failed" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_failed" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_failed" = [
  Cat "has_failed" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_fail" = [
  Cat "will_fail" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "fancy" = [
  Cat "fancy" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "fancy" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "fancy" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "fancy" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "fancies" = [
  Cat "fancies" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "fancied" = [
  Cat "fancied" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_fancied" = [
  Cat "have_fancied" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_fancied" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_fancied" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_fancied" = [
  Cat "has_fancied" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_fancy" = [
  Cat "will_fancy" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "favor" = [
  Cat "favor" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "favor" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "favor" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "favor" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "favors" = [
  Cat "favors" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "favored" = [
  Cat "favored" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_favored" = [
  Cat "have_favored" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_favored" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_favored" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_favored" = [
  Cat "has_favored" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_favor" = [
  Cat "will_favor" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "fear" = [
  Cat "fear" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "fear" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "fear" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "fear" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []],
  Cat "fear" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [For] []],
  Cat "fear" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [For] []],
  Cat "fear" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [For] []],
  Cat "fear" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [For] []]
  ]
lexicon "fears" = [
  Cat "fears" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []],
  Cat "fears" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [For] []]
  ]
lexicon "feared" = [
  Cat "feared" "VP" [Past] [Cat "_" "NP" [AccOrDat] []],
  Cat "feared" "VP" [Past] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [For] []]
  ]
lexicon "have_feared" = [
  Cat "have_feared" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_feared" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_feared" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_feared" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [For] []],
  Cat "have_feared" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [For] []],
  Cat "have_feared" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [For] []]
  ]
lexicon "has_feared" = [
  Cat "has_feared" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []],
  Cat "has_feared" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [For] []]
  ]
lexicon "will_fear" = [
  Cat "will_fear" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []],
  Cat "will_fear" "VP" [Fut] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [For] []]
  ]

lexicon "feel" = [
  Cat "feel" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "feel" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "feel" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "feel" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "feels" = [
  Cat "feels" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "felt" = [
  Cat "felt" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_felt" = [
  Cat "have_felt" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_felt" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_felt" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_felt" = [
  Cat "has_felt" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_feel" = [
  Cat "will_feel" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "feign" = [
  Cat "feign" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "feign" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "feign" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "feign" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "feigns" = [
  Cat "feigns" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "feigned" = [
  Cat "feigned" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_feigned" = [
  Cat "have_feigned" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_feigned" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_feigned" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_feigned" = [
  Cat "has_feigned" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_feign" = [
  Cat "will_feign" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "fell" = [
  Cat "fell" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "fell" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "fell" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "fell" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "falls" = [
  Cat "falls" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "fell" = [
  Cat "fell" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_fallen" = [
  Cat "have_fallen" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_fallen" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_fallen" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_fallen" = [
  Cat "has_fallen" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_fall" = [
  Cat "will_fall" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "fight" = [
  Cat "fight" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "fight" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "fight" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "fight" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "fights" = [
  Cat "fights" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "fought" = [
  Cat "fought" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_fought" = [
  Cat "have_fought" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_fought" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_fought" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_fought" = [
  Cat "has_fought" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_fight" = [
  Cat "will_fight" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "find" = [
  Cat "find" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "find" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "find" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "find" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "finds" = [
  Cat "finds" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "found" = [
  Cat "found" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_found" = [
  Cat "have_found" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_found" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_found" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_found" = [
  Cat "has_found" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_find" = [
  Cat "will_find" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "finish" = [
  Cat "finish" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "finish" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "finish" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "finish" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "finishes" = [
  Cat "finishes" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "finished" = [
  Cat "finished" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_finished" = [
  Cat "have_finished" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_finished" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_finished" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_finished" = [
  Cat "has_finished" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_finish" = [
  Cat "will_finish" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "fix" = [
  Cat "fix" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "fix" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "fix" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "fix" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "fixes" = [
  Cat "fixes" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "fixed" = [
  Cat "fixed" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_fixed" = [
  Cat "have_fixed" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_fixed" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_fixed" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_fixed" = [
  Cat "has_fixed" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_fix" = [
  Cat "will_fix" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "flatter" = [
  Cat "flatter" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "flatter" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "flatter" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "flatter" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "flatters" = [
  Cat "flatters" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "flattered" = [
  Cat "flattered" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_flattered" = [
  Cat "have_flattered" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_flattered" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_flattered" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_flattered" = [
  Cat "has_flattered" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_flatter" = [
  Cat "will_flatter" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "fly" = [
  Cat "fly" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "fly" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "fly" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "fly" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []],
  Cat "fly" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "fly" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []],
  Cat "fly" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Onto] []],
  Cat "fly" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [In] []],
  Cat "fly" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Into] []],
  Cat "fly" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "fly" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []],
  Cat "fly" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Onto] []],
  Cat "fly" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [In] []],
  Cat "fly" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Into] []],
  Cat "fly" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "fly" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []],
  Cat "fly" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Onto] []],
  Cat "fly" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [In] []],
  Cat "fly" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Into] []],
  Cat "fly" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "fly" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []],
  Cat "fly" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Onto] []],
  Cat "fly" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [In] []],
  Cat "fly" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Into] []]
  ]
lexicon "flies" = [
  Cat "flies" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []],
  Cat "flies" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "flies" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []],
  Cat "flies" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Onto] []],
  Cat "flies" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [In] []],
  Cat "flies" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Into] []]
  ]
lexicon "flew" = [
  Cat "flew" "VP" [Past] [Cat "_" "NP" [AccOrDat] []],
  Cat "flew" "VP" [Past] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "flew" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []],
  Cat "flew" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Onto] []],
  Cat "flew" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [In] []],
  Cat "flew" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Into] []]
  ]
lexicon "have_flown" = [
  Cat "have_flown" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_flown" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_flown" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_flown" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "have_flown" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []],
  Cat "have_flown" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Onto] []],
  Cat "have_flown" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [In] []],
  Cat "have_flown" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Into] []],
  Cat "have_flown" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "have_flown" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []],
  Cat "have_flown" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Onto] []],
  Cat "have_flown" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [In] []],
  Cat "have_flown" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Into] []],
  Cat "have_flown" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "have_flown" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []],
  Cat "have_flown" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Onto] []],
  Cat "have_flown" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [In] []],
  Cat "have_flown" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Into] []]
  ]
lexicon "has_flown" = [
  Cat "has_flown" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []],
  Cat "has_flown" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "has_flown" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []],
  Cat "has_flown" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Onto] []],
  Cat "has_flown" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [In] []],
  Cat "has_flown" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Into] []]
  ]
lexicon "will_fly" = [
  Cat "will_fly" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []],
  Cat "will_fly" "VP" [Fut] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "will_fly" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []],
  Cat "will_fly" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Onto] []],
  Cat "will_fly" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [In] []],
  Cat "will_fly" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Into] []]
  ]

lexicon "fold" = [
  Cat "fold" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "fold" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "fold" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "fold" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "folds" = [
  Cat "folds" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "folded" = [
  Cat "folded" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_folded" = [
  Cat "have_folded" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_folded" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_folded" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_folded" = [
  Cat "has_folded" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_fold" = [
  Cat "will_fold" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "follow" = [
  Cat "follow" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "follow" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "follow" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "follow" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "follows" = [
  Cat "follows" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "followed" = [
  Cat "followed" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_followed" = [
  Cat "have_followed" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_followed" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_followed" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_followed" = [
  Cat "has_followed" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_follow" = [
  Cat "will_follow" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "force" = [
  Cat "force" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "force" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "force" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "force" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "forces" = [
  Cat "forces" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "forced" = [
  Cat "forced" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_forced" = [
  Cat "have_forced" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_forced" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_forced" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_forced" = [
  Cat "has_forced" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_force" = [
  Cat "will_force" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "forecast" = [
  Cat "forecast" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "forecast" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "forecast" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "forecast" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "forecasts" = [
  Cat "forecasts" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "forecasted" = [
  Cat "forecasted" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_forecasted" = [
  Cat "have_forecasted" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_forecasted" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_forecasted" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_forecasted" = [
  Cat "has_forecasted" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_forecast" = [
  Cat "will_forecast" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "foresee" = [
  Cat "foresee" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "foresee" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "foresee" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "foresee" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "foresees" = [
  Cat "foresees" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "foreseed" = [
  Cat "foreseed" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_foreseed" = [
  Cat "have_foreseed" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_foreseed" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_foreseed" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_foreseed" = [
  Cat "has_foreseed" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_foresee" = [
  Cat "will_foresee" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "foretell" = [
  Cat "foretell" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "foretell" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "foretell" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "foretell" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "foretells" = [
  Cat "foretells" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "foretelled" = [
  Cat "foretelled" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_foretelled" = [
  Cat "have_foretelled" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_foretelled" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_foretelled" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_foretelled" = [
  Cat "has_foretelled" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_foretell" = [
  Cat "will_foretell" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "forewarn" = [
  Cat "forewarn" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "forewarn" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "forewarn" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "forewarn" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "forewarns" = [
  Cat "forewarns" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "forewarned" = [
  Cat "forewarned" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_forewarned" = [
  Cat "have_forewarned" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_forewarned" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_forewarned" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_forewarned" = [
  Cat "has_forewarned" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_forewarn" = [
  Cat "will_forewarn" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "forget" = [
  Cat "forget" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "forget" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "forget" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "forget" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "forgets" = [
  Cat "forgets" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "forgot" = [
  Cat "forgot" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_forgotten" = [
  Cat "have_forgotten" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_forgotten" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_forgotten" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_forgotten" = [
  Cat "has_forgotten" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_forget" = [
  Cat "will_forget" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "forgive" = [
  Cat "forgive" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "forgive" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "forgive" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "forgive" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []],
  Cat "forgive" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "forgive" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "forgive" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "forgive" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "forgives" = [
  Cat "forgives" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []],
  Cat "forgives" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "forgave" = [
  Cat "forgave" "VP" [Past] [Cat "_" "NP" [AccOrDat] []],
  Cat "forgave" "VP" [Past] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_forgiven" = [
  Cat "have_forgiven" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_forgiven" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_forgiven" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_forgiven" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "have_forgiven" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "have_forgiven" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_forgiven" = [
  Cat "has_forgiven" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []],
  Cat "has_forgiven" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_forgive" = [
  Cat "will_forgive" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []],
  Cat "will_forgive" "VP" [Fut] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "freeze" = [
  Cat "freeze" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "freeze" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "freeze" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "freeze" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "freezes" = [
  Cat "freezes" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "froze" = [
  Cat "froze" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_frozen" = [
  Cat "have_frozen" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_frozen" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_frozen" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_frozen" = [
  Cat "has_frozen" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_freeze" = [
  Cat "will_freeze" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "frustrate" = [
  Cat "frustrate" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "frustrate" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "frustrate" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "frustrate" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "frustrates" = [
  Cat "frustrates" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "frustrated" = [
  Cat "frustrated" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_frustrated" = [
  Cat "have_frustrated" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_frustrated" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_frustrated" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_frustrated" = [
  Cat "has_frustrated" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_frustrate" = [
  Cat "will_frustrate" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "fulfill" = [
  Cat "fulfill" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "fulfill" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "fulfill" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "fulfill" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "fulfills" = [
  Cat "fulfills" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "fulfilled" = [
  Cat "fulfilled" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_fulfilled" = [
  Cat "have_fulfilled" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_fulfilled" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_fulfilled" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_fulfilled" = [
  Cat "has_fulfilled" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_fulfill" = [
  Cat "will_fulfill" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "furnish" = [
  Cat "furnish" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "furnish" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "furnish" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "furnish" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "furnishes" = [
  Cat "furnishes" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "furnished" = [
  Cat "furnished" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_furnished" = [
  Cat "have_furnished" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_furnished" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_furnished" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_furnished" = [
  Cat "has_furnished" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_furnish" = [
  Cat "will_furnish" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "gamble" = [
  Cat "gamble" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "gamble" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "gamble" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "gamble" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "gambles" = [
  Cat "gambles" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "gambled" = [
  Cat "gambled" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_gambled" = [
  Cat "have_gambled" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_gambled" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_gambled" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_gambled" = [
  Cat "has_gambled" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_gamble" = [
  Cat "will_gamble" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "gather" = [
  Cat "gather" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "gather" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "gather" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "gather" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "gathers" = [
  Cat "gathers" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "gathered" = [
  Cat "gathered" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_gathered" = [
  Cat "have_gathered" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_gathered" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_gathered" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_gathered" = [
  Cat "has_gathered" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_gather" = [
  Cat "will_gather" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "get" = [
  Cat "get" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "get" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "get" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "get" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []],
  Cat "get" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "get" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "get" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "get" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "gets" = [
  Cat "gets" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []],
  Cat "gets" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "got" = [
  Cat "got" "VP" [Past] [Cat "_" "NP" [AccOrDat] []],
  Cat "got" "VP" [Past] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_gotten" = [
  Cat "have_gotten" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_gotten" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_gotten" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_gotten" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "have_gotten" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "have_gotten" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_gotten" = [
  Cat "has_gotten" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []],
  Cat "has_gotten" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_get" = [
  Cat "will_get" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []],
  Cat "will_get" "VP" [Fut] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "give" = [
  Cat "give" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "give" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "give" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "give" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "give" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "give" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "give" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "give" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "gives" = [
  Cat "gives" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "gives" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "gave" = [
  Cat "gave" "VP" [Past] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "gave" "VP" [Past] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "have_given" = [
  Cat "have_given" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "have_given" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "have_given" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "have_given" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "have_given" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "have_given" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "has_given" = [
  Cat "has_given" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "has_given" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "will_give" = [
  Cat "will_give" "VP" [Fut] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "will_give" "VP" [Fut] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]

lexicon "goad" = [
  Cat "goad" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "goad" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "goad" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "goad" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "goads" = [
  Cat "goads" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "goaded" = [
  Cat "goaded" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_goaded" = [
  Cat "have_goaded" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_goaded" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_goaded" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_goaded" = [
  Cat "has_goaded" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_goad" = [
  Cat "will_goad" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "grant" = [
  Cat "grant" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "grant" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "grant" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "grant" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []],
  Cat "grant" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "grant" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "grant" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "grant" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "grants" = [
  Cat "grants" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []],
  Cat "grants" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "granted" = [
  Cat "granted" "VP" [Past] [Cat "_" "NP" [AccOrDat] []],
  Cat "granted" "VP" [Past] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_granted" = [
  Cat "have_granted" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_granted" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_granted" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_granted" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "have_granted" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "have_granted" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_granted" = [
  Cat "has_granted" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []],
  Cat "has_granted" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_grant" = [
  Cat "will_grant" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []],
  Cat "will_grant" "VP" [Fut] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "grasp" = [
  Cat "grasp" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "grasp" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "grasp" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "grasp" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []],
  Cat "grasp" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Onto] []],
  Cat "grasp" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Onto] []],
  Cat "grasp" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Onto] []],
  Cat "grasp" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Onto] []]
  ]
lexicon "grasps" = [
  Cat "grasps" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []],
  Cat "grasps" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Onto] []]
  ]
lexicon "grasped" = [
  Cat "grasped" "VP" [Past] [Cat "_" "NP" [AccOrDat] []],
  Cat "grasped" "VP" [Past] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Onto] []]
  ]
lexicon "have_grasped" = [
  Cat "have_grasped" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_grasped" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_grasped" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_grasped" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Onto] []],
  Cat "have_grasped" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Onto] []],
  Cat "have_grasped" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Onto] []]
  ]
lexicon "has_grasped" = [
  Cat "has_grasped" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []],
  Cat "has_grasped" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Onto] []]
  ]
lexicon "will_grasp" = [
  Cat "will_grasp" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []],
  Cat "will_grasp" "VP" [Fut] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Onto] []]
  ]

lexicon "grow" = [
  Cat "grow" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "grow" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "grow" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "grow" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "grows" = [
  Cat "grows" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "grew" = [
  Cat "grew" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_grown" = [
  Cat "have_grown" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_grown" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_grown" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_grown" = [
  Cat "has_grown" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_grow" = [
  Cat "will_grow" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "guarantee" = [
  Cat "guarantee" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "guarantee" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "guarantee" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "guarantee" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []],
  Cat "guarantee" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "guarantee" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "guarantee" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "guarantee" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "guarantees" = [
  Cat "guarantees" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []],
  Cat "guarantees" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "guaranteed" = [
  Cat "guaranteed" "VP" [Past] [Cat "_" "NP" [AccOrDat] []],
  Cat "guaranteed" "VP" [Past] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_guaranteed" = [
  Cat "have_guaranteed" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_guaranteed" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_guaranteed" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_guaranteed" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "have_guaranteed" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "have_guaranteed" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_guaranteed" = [
  Cat "has_guaranteed" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []],
  Cat "has_guaranteed" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_guarantee" = [
  Cat "will_guarantee" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []],
  Cat "will_guarantee" "VP" [Fut] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "guide" = [
  Cat "guide" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "guide" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "guide" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "guide" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "guides" = [
  Cat "guides" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "guided" = [
  Cat "guided" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_guided" = [
  Cat "have_guided" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_guided" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_guided" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_guided" = [
  Cat "has_guided" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_guide" = [
  Cat "will_guide" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "handle" = [
  Cat "handle" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "handle" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "handle" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "handle" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "handles" = [
  Cat "handles" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "handled" = [
  Cat "handled" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_handled" = [
  Cat "have_handled" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_handled" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_handled" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_handled" = [
  Cat "has_handled" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_handle" = [
  Cat "will_handle" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "harass" = [
  Cat "harass" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "harass" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "harass" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "harass" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "harasses" = [
  Cat "harasses" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "harassed" = [
  Cat "harassed" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_harassed" = [
  Cat "have_harassed" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_harassed" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_harassed" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_harassed" = [
  Cat "has_harassed" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_harass" = [
  Cat "will_harass" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "harden" = [
  Cat "harden" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "harden" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "harden" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "harden" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []],
  Cat "harden" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Into] []],
  Cat "harden" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Into] []],
  Cat "harden" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Into] []],
  Cat "harden" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Into] []]
  ]
lexicon "hardens" = [
  Cat "hardens" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []],
  Cat "hardens" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Into] []]
  ]
lexicon "hardened" = [
  Cat "hardened" "VP" [Past] [Cat "_" "NP" [AccOrDat] []],
  Cat "hardened" "VP" [Past] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Into] []]
  ]
lexicon "have_hardened" = [
  Cat "have_hardened" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_hardened" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_hardened" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_hardened" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Into] []],
  Cat "have_hardened" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Into] []],
  Cat "have_hardened" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Into] []]
  ]
lexicon "has_hardened" = [
  Cat "has_hardened" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []],
  Cat "has_hardened" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Into] []]
  ]
lexicon "will_harden" = [
  Cat "will_harden" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []],
  Cat "will_harden" "VP" [Fut] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Into] []]
  ]

lexicon "harm" = [
  Cat "harm" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "harm" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "harm" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "harm" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "harms" = [
  Cat "harms" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "harmed" = [
  Cat "harmed" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_harmed" = [
  Cat "have_harmed" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_harmed" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_harmed" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_harmed" = [
  Cat "has_harmed" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_harm" = [
  Cat "will_harm" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "hasten" = [
  Cat "hasten" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "hasten" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "hasten" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "hasten" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "hastens" = [
  Cat "hastens" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "hastened" = [
  Cat "hastened" "VP" [Past] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "have_hastened" = [
  Cat "have_hastened" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "have_hastened" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "have_hastened" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "has_hastened" = [
  Cat "has_hastened" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "will_hasten" = [
  Cat "will_hasten" "VP" [Fut] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]

lexicon "hate" = [
  Cat "hate" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "hate" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "hate" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "hate" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "hates" = [
  Cat "hates" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "hated" = [
  Cat "hated" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_hated" = [
  Cat "have_hated" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_hated" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_hated" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_hated" = [
  Cat "has_hated" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_hate" = [
  Cat "will_hate" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "have" = [
  Cat "have" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "have" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has" = [
  Cat "has" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "had" = [
  Cat "had" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_had" = [
  Cat "have_had" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_had" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_had" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_had" = [
  Cat "has_had" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_have" = [
  Cat "will_have" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "hear" = [
  Cat "hear" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "hear" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "hear" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "hear" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "hears" = [
  Cat "hears" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "heard" = [
  Cat "heard" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_heard" = [
  Cat "have_heard" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_heard" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_heard" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_heard" = [
  Cat "has_heard" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_hear" = [
  Cat "will_hear" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "help" = [
  Cat "help" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "help" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "help" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "help" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "helps" = [
  Cat "helps" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "helped" = [
  Cat "helped" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_helped" = [
  Cat "have_helped" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_helped" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_helped" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_helped" = [
  Cat "has_helped" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_help" = [
  Cat "will_help" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "hire" = [
  Cat "hire" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "hire" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "hire" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "hire" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []],
  Cat "hire" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "hire" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "hire" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "hire" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "hires" = [
  Cat "hires" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []],
  Cat "hires" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "hired" = [
  Cat "hired" "VP" [Past] [Cat "_" "NP" [AccOrDat] []],
  Cat "hired" "VP" [Past] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_hired" = [
  Cat "have_hired" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_hired" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_hired" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_hired" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "have_hired" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []],
  Cat "have_hired" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_hired" = [
  Cat "has_hired" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []],
  Cat "has_hired" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_hire" = [
  Cat "will_hire" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []],
  Cat "will_hire" "VP" [Fut] [Cat "_" "NP" [AccOrDat] [], Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "hit" = [
  Cat "hit" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "hit" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "hit" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "hit" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "hits" = [
  Cat "hits" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "hit" = [
  Cat "hit" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_hit" = [
  Cat "have_hit" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_hit" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_hit" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_hit" = [
  Cat "has_hit" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_hit" = [
  Cat "will_hit" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "hold" = [
  Cat "hold" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "hold" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "hold" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "hold" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []],
  Cat "hold" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [For] []],
  Cat "hold" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [For] []],
  Cat "hold" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [For] []],
  Cat "hold" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [For] []]
  ]
lexicon "holds" = [
  Cat "holds" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []],
  Cat "holds" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [For] []]
  ]
lexicon "held" = [
  Cat "held" "VP" [Past] [Cat "_" "NP" [AccOrDat] []],
  Cat "held" "VP" [Past] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [For] []]
  ]
lexicon "have_held" = [
  Cat "have_held" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_held" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_held" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_held" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [For] []],
  Cat "have_held" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [For] []],
  Cat "have_held" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [For] []]
  ]
lexicon "has_held" = [
  Cat "has_held" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []],
  Cat "has_held" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [For] []]
  ]
lexicon "will_hold" = [
  Cat "will_hold" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []],
  Cat "will_hold" "VP" [Fut] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [For] []]
  ]

lexicon "hope" = [
  Cat "hope" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [For] []],
  Cat "hope" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [For] []],
  Cat "hope" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [For] []],
  Cat "hope" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [For] []]
  ]
lexicon "hopes" = [
  Cat "hopes" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [For] []]
  ]
lexicon "hoped" = [
  Cat "hoped" "VP" [Past] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [For] []]
  ]
lexicon "have_hoped" = [
  Cat "have_hoped" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [For] []],
  Cat "have_hoped" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [For] []],
  Cat "have_hoped" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [For] []]
  ]
lexicon "has_hoped" = [
  Cat "has_hoped" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [For] []]
  ]
lexicon "will_hope" = [
  Cat "will_hope" "VP" [Fut] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [For] []]
  ]

lexicon "hound" = [
  Cat "hound" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "hound" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "hound" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "hound" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "hounds" = [
  Cat "hounds" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "hounded" = [
  Cat "hounded" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_hounded" = [
  Cat "have_hounded" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_hounded" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_hounded" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_hounded" = [
  Cat "has_hounded" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_hound" = [
  Cat "will_hound" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "hurry" = [
  Cat "hurry" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "hurry" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []],
  Cat "hurry" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Onto] []],
  Cat "hurry" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [In] []],
  Cat "hurry" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Into] []],
  Cat "hurry" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "hurry" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []],
  Cat "hurry" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Onto] []],
  Cat "hurry" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [In] []],
  Cat "hurry" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Into] []],
  Cat "hurry" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "hurry" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []],
  Cat "hurry" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Onto] []],
  Cat "hurry" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [In] []],
  Cat "hurry" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Into] []],
  Cat "hurry" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "hurry" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []],
  Cat "hurry" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Onto] []],
  Cat "hurry" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [In] []],
  Cat "hurry" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Into] []]
  ]
lexicon "hurries" = [
  Cat "hurries" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "hurries" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []],
  Cat "hurries" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Onto] []],
  Cat "hurries" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [In] []],
  Cat "hurries" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Into] []]
  ]
lexicon "hurried" = [
  Cat "hurried" "VP" [Past] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "hurried" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []],
  Cat "hurried" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Onto] []],
  Cat "hurried" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [In] []],
  Cat "hurried" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Into] []]
  ]
lexicon "have_hurried" = [
  Cat "have_hurried" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "have_hurried" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []],
  Cat "have_hurried" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Onto] []],
  Cat "have_hurried" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [In] []],
  Cat "have_hurried" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Into] []],
  Cat "have_hurried" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "have_hurried" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []],
  Cat "have_hurried" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Onto] []],
  Cat "have_hurried" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [In] []],
  Cat "have_hurried" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Into] []],
  Cat "have_hurried" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "have_hurried" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []],
  Cat "have_hurried" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Onto] []],
  Cat "have_hurried" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [In] []],
  Cat "have_hurried" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Into] []]
  ]
lexicon "has_hurried" = [
  Cat "has_hurried" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "has_hurried" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []],
  Cat "has_hurried" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Onto] []],
  Cat "has_hurried" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [In] []],
  Cat "has_hurried" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Into] []]
  ]
lexicon "will_hurry" = [
  Cat "will_hurry" "VP" [Fut] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "will_hurry" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [On] []],
  Cat "will_hurry" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Onto] []],
  Cat "will_hurry" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [In] []],
  Cat "will_hurry" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [Into] []]
  ]

lexicon "hypothesize" = [
  Cat "hypothesize" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "hypothesize" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "hypothesize" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "hypothesize" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "hypothesizes" = [
  Cat "hypothesizes" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "hypothesized" = [
  Cat "hypothesized" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_hypothesized" = [
  Cat "have_hypothesized" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_hypothesized" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_hypothesized" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_hypothesized" = [
  Cat "has_hypothesized" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_hypothesize" = [
  Cat "will_hypothesize" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "idealize" = [
  Cat "idealize" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "idealize" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "idealize" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "idealize" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "idealizes" = [
  Cat "idealizes" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "idealized" = [
  Cat "idealized" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_idealized" = [
  Cat "have_idealized" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_idealized" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_idealized" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_idealized" = [
  Cat "has_idealized" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_idealize" = [
  Cat "will_idealize" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "identify" = [
  Cat "identify" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "identify" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "identify" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "identify" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []],
  Cat "identify" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [With] []],
  Cat "identify" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [With] []],
  Cat "identify" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [With] []],
  Cat "identify" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [With] []]
  ]
lexicon "identifies" = [
  Cat "identifies" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []],
  Cat "identifies" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [With] []]
  ]
lexicon "identified" = [
  Cat "identified" "VP" [Past] [Cat "_" "NP" [AccOrDat] []],
  Cat "identified" "VP" [Past] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [With] []]
  ]
lexicon "have_identified" = [
  Cat "have_identified" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_identified" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_identified" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_identified" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [With] []],
  Cat "have_identified" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [With] []],
  Cat "have_identified" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [With] []]
  ]
lexicon "has_identified" = [
  Cat "has_identified" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []],
  Cat "has_identified" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [With] []]
  ]
lexicon "will_identify" = [
  Cat "will_identify" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []],
  Cat "will_identify" "VP" [Fut] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [With] []]
  ]

lexicon "ignore" = [
  Cat "ignore" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "ignore" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "ignore" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "ignore" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "ignores" = [
  Cat "ignores" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "ignored" = [
  Cat "ignored" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_ignored" = [
  Cat "have_ignored" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_ignored" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_ignored" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_ignored" = [
  Cat "has_ignored" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_ignore" = [
  Cat "will_ignore" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "illustrate" = [
  Cat "illustrate" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "illustrate" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "illustrate" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "illustrate" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "illustrates" = [
  Cat "illustrates" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "illustrated" = [
  Cat "illustrated" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_illustrated" = [
  Cat "have_illustrated" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_illustrated" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_illustrated" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_illustrated" = [
  Cat "has_illustrated" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_illustrate" = [
  Cat "will_illustrate" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "imagine" = [
  Cat "imagine" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "imagine" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "imagine" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "imagine" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "imagines" = [
  Cat "imagines" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "imagined" = [
  Cat "imagined" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_imagined" = [
  Cat "have_imagined" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_imagined" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_imagined" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_imagined" = [
  Cat "has_imagined" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_imagine" = [
  Cat "will_imagine" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "imitate" = [
  Cat "imitate" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "imitate" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "imitate" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "imitate" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "imitates" = [
  Cat "imitates" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "imitated" = [
  Cat "imitated" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_imitated" = [
  Cat "have_imitated" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_imitated" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_imitated" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_imitated" = [
  Cat "has_imitated" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_imitate" = [
  Cat "will_imitate" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "immerse" = [
  Cat "immerse" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "immerse" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "immerse" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "immerse" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "immerses" = [
  Cat "immerses" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "immersed" = [
  Cat "immersed" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_immersed" = [
  Cat "have_immersed" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_immersed" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_immersed" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_immersed" = [
  Cat "has_immersed" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_immerse" = [
  Cat "will_immerse" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "immigrate" = [
  Cat "immigrate" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "immigrate" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "immigrate" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "immigrate" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "immigrates" = [
  Cat "immigrates" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "immigrated" = [
  Cat "immigrated" "VP" [Past] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "have_immigrated" = [
  Cat "have_immigrated" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "have_immigrated" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "have_immigrated" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "has_immigrated" = [
  Cat "has_immigrated" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "will_immigrate" = [
  Cat "will_immigrate" "VP" [Fut] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]

lexicon "impair" = [
  Cat "impair" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "impair" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "impair" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "impair" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "impairs" = [
  Cat "impairs" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "impaired" = [
  Cat "impaired" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_impaired" = [
  Cat "have_impaired" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_impaired" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_impaired" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_impaired" = [
  Cat "has_impaired" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_impair" = [
  Cat "will_impair" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "impart" = [
  Cat "impart" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "impart" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "impart" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "impart" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []],
  Cat "impart" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "impart" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "impart" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "impart" "VP" [Infl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "imparts" = [
  Cat "imparts" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []],
  Cat "imparts" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "imparted" = [
  Cat "imparted" "VP" [Past] [Cat "_" "NP" [AccOrDat] []],
  Cat "imparted" "VP" [Past] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "have_imparted" = [
  Cat "have_imparted" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_imparted" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_imparted" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_imparted" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "have_imparted" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []],
  Cat "have_imparted" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "has_imparted" = [
  Cat "has_imparted" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []],
  Cat "has_imparted" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]
lexicon "will_impart" = [
  Cat "will_impart" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []],
  Cat "will_impart" "VP" [Fut] [Cat "_" "NP" [AccOrDat] [], Cat "_" "PP" [To] []]
  ]

lexicon "imply" = [
  Cat "imply" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "imply" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "imply" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "imply" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "implies" = [
  Cat "implies" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "implied" = [
  Cat "implied" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_implied" = [
  Cat "have_implied" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_implied" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_implied" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_implied" = [
  Cat "has_implied" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_imply" = [
  Cat "will_imply" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "import" = [
  Cat "import" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "import" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "import" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "import" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "imports" = [
  Cat "imports" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "imported" = [
  Cat "imported" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_imported" = [
  Cat "have_imported" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_imported" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_imported" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_imported" = [
  Cat "has_imported" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_import" = [
  Cat "will_import" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "impress" = [
  Cat "impress" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "impress" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "impress" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "impress" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "impresses" = [
  Cat "impresses" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "impressed" = [
  Cat "impressed" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_impressed" = [
  Cat "have_impressed" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_impressed" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_impressed" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_impressed" = [
  Cat "has_impressed" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_impress" = [
  Cat "will_impress" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "improve" = [
  Cat "improve" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "improve" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "improve" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "improve" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "improves" = [
  Cat "improves" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "improved" = [
  Cat "improved" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_improved" = [
  Cat "have_improved" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_improved" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_improved" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_improved" = [
  Cat "has_improved" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_improve" = [
  Cat "will_improve" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "incite" = [
  Cat "incite" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "incite" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "incite" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "incite" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "incites" = [
  Cat "incites" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "incited" = [
  Cat "incited" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_incited" = [
  Cat "have_incited" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_incited" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_incited" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_incited" = [
  Cat "has_incited" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_incite" = [
  Cat "will_incite" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "include" = [
  Cat "include" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "include" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "include" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "include" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "includes" = [
  Cat "includes" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "included" = [
  Cat "included" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_included" = [
  Cat "have_included" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_included" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_included" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_included" = [
  Cat "has_included" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_include" = [
  Cat "will_include" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "increase" = [
  Cat "increase" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "increase" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "increase" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "increase" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "increases" = [
  Cat "increases" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "increased" = [
  Cat "increased" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_increased" = [
  Cat "have_increased" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_increased" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_increased" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_increased" = [
  Cat "has_increased" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_increase" = [
  Cat "will_increase" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "indicate" = [
  Cat "indicate" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "indicate" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "indicate" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "indicate" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "indicates" = [
  Cat "indicates" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "indicated" = [
  Cat "indicated" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_indicated" = [
  Cat "have_indicated" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_indicated" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_indicated" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_indicated" = [
  Cat "has_indicated" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_indicate" = [
  Cat "will_indicate" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "infest" = [
  Cat "infest" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "infest" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "infest" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "infest" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "infests" = [
  Cat "infests" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "infested" = [
  Cat "infested" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_infested" = [
  Cat "have_infested" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_infested" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_infested" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_infested" = [
  Cat "has_infested" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_infest" = [
  Cat "will_infest" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "inflate" = [
  Cat "inflate" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "inflate" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "inflate" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "inflate" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "inflates" = [
  Cat "inflates" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "inflated" = [
  Cat "inflated" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_inflated" = [
  Cat "have_inflated" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_inflated" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_inflated" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_inflated" = [
  Cat "has_inflated" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_inflate" = [
  Cat "will_inflate" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "inform" = [
  Cat "inform" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "inform" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "inform" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "inform" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "informs" = [
  Cat "informs" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "informed" = [
  Cat "informed" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_informed" = [
  Cat "have_informed" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_informed" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_informed" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_informed" = [
  Cat "has_informed" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_inform" = [
  Cat "will_inform" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]

lexicon "inhabit" = [
  Cat "inhabit" "VP" [Pres,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "inhabit" "VP" [Pres,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "inhabit" "VP" [Pres,Pl] [Cat "_" "NP" [AccOrDat] []],
  Cat "inhabit" "VP" [Infl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "inhabits" = [
  Cat "inhabits" "VP" [Pres,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "inhabited" = [
  Cat "inhabited" "VP" [Past] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "have_inhabited" = [
  Cat "have_inhabited" "VP" [Perf,Sg,Fst] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_inhabited" "VP" [Perf,Sg,Snd] [Cat "_" "NP" [AccOrDat] []],
  Cat "have_inhabited" "VP" [Perf,Pl] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "has_inhabited" = [
  Cat "has_inhabited" "VP" [Perf,Sg,Thrd] [Cat "_" "NP" [AccOrDat] []]
  ]
lexicon "will_inhabit" = [
  Cat "will_inhabit" "VP" [Fut] [Cat "_" "NP" [AccOrDat] []]
  ]-}

lexicon _ = []