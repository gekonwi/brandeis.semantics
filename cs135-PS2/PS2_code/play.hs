{-# LANGUAGE MultiParamTypeClasses #-}

class Playing a b where
	play :: a -> b -> String

-- like Georg Bush, Angela Merkeö
data Human = Human { humanName :: String } deriving (Show)

-- like piano, violin
data Instrument = Instrument { instrumentName :: String } deriving (Show)

-- like chess, football
data Game = Game { gameName :: String } deriving (Show)

-- in a movie, like Batman, Hulk
data Role = Role { roleName :: String } deriving (Show)

-- e.g. "Let it Be from the Beatles"
data Song = Song { songName :: String } deriving (Show)


-- humans could "play other humans", e.g. in a movie, but then it is a role which I modelled to be a separate data type (see below).

-- e.g. "John" plays the "violin"
instance Playing Human Instrument where
	play x y = (humanName x) ++ " plays the " ++ (instrumentName y)


-- e.g. "John" plays "chess"
instance Playing Human Game where
	play x y = (humanName x) ++ " plays " ++ (gameName y)

-- e.g. "John" plays "Mercucio in Romeo and Juliet"
instance Playing Human Role where
	play x y = (humanName x) ++ " plays " ++ (roleName y)

-- e.g. "John" plays "the American anthem"
instance Playing Human Song where
	play x y = (humanName x) ++ " plays " ++ (songName y)


-- e.g. the "piano" plays "Yesterday from the Beatles" (while the violin plays something from Bach, whatever ...) 
instance Playing Instrument Song where
	play x y = "the " ++ (instrumentName x) ++ " plays " ++ (songName y)

-- in my foreign student English a game cannot play anything

-- "Batman", as a role, could play "the bad guy", which is another role
instance Playing Role Role where
	play x y = (roleName x) ++ " plays " ++ (roleName y)

-- "Max Payne", the game character (= role), plays "the May Payne theme" on a piano if the player lets him interact with a piano, thus the following two are possible:

instance Playing Role Instrument where
	play x y = (roleName x) ++ " plays the " ++ (instrumentName y)

instance Playing Role Song where
	play x y = (roleName x) ++ " plays " ++ (songName y)

-- but a song can't play neither a human, nor an instrument, nor a game, nor a role, nor another song