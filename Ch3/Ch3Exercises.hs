--Imports
import Data.List

-- Going to add a double line comment before any exercise I do
-- Exercise 1 + 2
-- 1) Length of a list function- compare against standard length function
-- 2) Add a type signature
myLength :: [a] -> Int
myLength (x:xs) = 1 + myLength xs
myLength _ = 0

-- Exercise 3
-- Write a function that computes the mean of a list
mean :: Fractional t => [t] -> t
mean xs = (sum xs) / fromIntegral (length xs)

-- Exercise 4
-- Turn a list into a palindrome
myReverse (x:xs) =  reverse xs ++ [x]
myReverse _ = []

palindrome xs = xs ++ myReverse xs

-- Exercise 5
-- Check whether a list is a palindrome
checkPalindrome xs = xs == myReverse xs

-- Exercise 6
-- Sort a list of lists by length of sublist
sortLists xs = sortOn length xs

-- Exercise 7
-- Intersperse an element between lists of the element type and concatenate the
-- lists
myIntersperse :: a -> [[a]] -> [a] 
myIntersperse elt (x:y:xs) = x ++ [elt] ++ (myIntersperse elt (y:xs))
myIntersperse elt (x:xs) = x
myIntersperse elt _ = []

--Exercise 8
-- Using the binary tree type defined earlier find the height of the tree
data Tree a = Node a (Tree a) (Tree a)
            | Empty

height (Node _ a b) = 1 + max (height a) (height b)
height Empty = 0

--Exercise 9
-- Take 3 two-dimensional points a b c and deduce whether the angle is
-- clockwise or anticlockwise or whether the points collinear
data Direction = Clockwise
               | AntiClockwise
               | Collinear
                  deriving (Show, Eq)
directional (a1, a2) (b1, b2) (c1, c2)
            | det == 0 = Collinear
            | det > 0 = AntiClockwise
            | otherwise = Clockwise
            where det = (a1-b1)*(c2 - b2) - (a2 - b2)*(c1 - b1)

--Exercise 10
--Define a function that takes a list of 2D points [a,b,c,d,e] and returns
--Direction of succesive triples [a,b,c] [b, c, d] etc.
directionals (x:y:z:xs) = [directional x y z] ++ directionals (y:z:xs)
directionals _ = []

--Exercise 11
--Implement Graham's Convex Hull Scan Algorithm
-- The algorithm takes in a list of points and returns a list of vertices that
-- define the convex hull of the input list. See Wikipedia\Graham_Scan
--
-- Step 1 is to identify the root.
-- This is the point with the smallest y coordinate. (if there are multiple
-- then the point with smallest x coordinate is chosen next).
--
-- Step 2 is to sort the remaining points in increasing order of angle of
-- segment formed by point and root with x axis. For collinear points sort in
-- increasing distance to root
--
-- Step 3
-- Now Apply Graham's Scan Algorithm lmao

--Helper Functions
--
dist (a, b) (c, d) = sqrt ((c-a)**2 + (d-b)**2)
--pointOrder outputs an Ordering (GT, LT, EQ) as in Step 2
pointOrder :: (Num t, Ord t, Floating t) => (t, t) -> (t, t) -> (t, t) -> Ordering
pointOrder (rootX, rootY) (a1, a2) (b1, b2)
            | dir == Collinear = compare (dist (rootX, rootY) (a1, a2)) (dist (rootX, rootY) (b1, b2))  
            | dir == AntiClockwise = GT
            | otherwise = LT
              where dir = directional (a1, a2) (rootX, rootY) (b1, b2)

--findRoot finds root as in step 1
findRoot Nothing (x:xs) = findRoot (Just x) xs
findRoot (Just (rootx, rooty)) ((x1, x2):xs)
          | rooty < x2 = findRoot (Just (rootx, rooty)) xs
          | rooty == x2 && rootx < x1 = findRoot (Just (rootx, rooty)) xs
          | otherwise = findRoot (Just (x1, x2)) xs
findRoot (Just x) [] = x
findRoot Nothing [] = (0, 0)
