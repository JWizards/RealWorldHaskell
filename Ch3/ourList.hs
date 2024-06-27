-- Cons is our version of the : list constructor
data CommuList a = Cons a (CommuList a)
                 | Nil
                  deriving Show


-- OurList Maker!
fromList (x:xs) = Cons x (fromList xs)
fromList []     = Nil


-- OurList to Lister :C
toList (Cons x xs) = x:(toList xs) 
toList Nil = []
