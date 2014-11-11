-- A class Foo containing instances that can accept either an Int or a String.  If input is Int, shows the number, if input is String, shows the length of the string

-- Try the following:
--     foo "Hello"
--     foo (10::Int)
--     foo 10.0
--     If one of these doesn't work, think about why

class Foo a where 
      foo :: a -> String

instance Foo Int where
	 foo i = show(i)

instance Foo String where
	 foo s = show(length s)

-- For more on typeclasses, review chapter 8 of Learn You A Haskell: http://learnyouahaskell.com/making-our-own-types-and-typeclasses