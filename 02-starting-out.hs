{-
 -Once you've installed Haskell from http://www.haskell.org/platform/, load the interpreter with the command ghci.
 -
 -You can load (and reload) this file in the interpreter with the command: ":l 2-starting-out.hs"
 -
 -The first function has been completed as an example. All the other functions are undefined.
 -They can be implemented in one line using the material covered in http://learnyouahaskell.com/starting-out
 -
 -All indices are zero based.
 -}

-- Find the penultimate element in list l
penultimate l = last (init l)

-- Find the element at index k in list l
-- For example: "findK 2 [0,0,1,0,0,0]" returns 1
-- |
-- >>> findK 3 [5,6,7,2,3,4]
-- 2
--
-- >>> findK 0 [3,2,1]
-- 3
--
-- >>> findK 3 [3,2,1]
-- *** Exception: Prelude.!!: index too large
findK k l = l !! k

-- Determine if list l is a palindrome
-- |
-- >>> isPalindrome "not"
-- False
--
-- >>> isPalindrome "abcba"
-- True
--
-- >>> isPalindrome "abccba"
-- True
--
-- >>> isPalindrome ""
-- True
isPalindrome l = l == reverse l

{-
 - Duplicate the elements in list xs, for example "duplicate [1,2,3]" would give the list [1,1,2,2,3,3]
 - Hint: The "concat [l]" function flattens a list of lists into a single list. 
 - (You can see the function definition by typing ":t concat" into the interpreter. Perhaps try this with other variables and functions)
 -
 - For example: concat [[1,2,3],[3,4,5]] returns [1,2,3,3,4,5]
 -}
-- |
-- >>> duplicate [1,2,3]
-- [1,1,2,2,3,3]
duplicate xs = concat [[x,x] | x <- xs]

{-
 - Imitate the functinality of zip
 - The function "min x y" returns the lower of values x and y
 - For example "ziplike [1,2,3] ['a', 'b', 'c', 'd']" returns [(1,'a'), (2, 'b'), (3, 'c')]
 -}
-- |
-- >>> ziplike [1,2,3] ['a', 'b', 'c', 'd']
-- [(1,'a'),(2,'b'),(3,'c')]
ziplike xs ys = [ (xs !! idx, ys !! idx) | idx <- [0..(min (length xs) (length ys))-1]]
-- i had to cheat for this one and took a peak at someone elses solution.

-- Split a list l at element k into a tuple: The first part up to and including k, the second part after k
-- For example "splitAtIndex 3 [1,1,1,2,2,2]" returns ([1,1,1],[2,2,2])
-- |
-- >>> splitAtIndex 3 [1,1,1,2,2,2]
-- ([1,1,1],[2,2,2])
--
-- >>> splitAtIndex 5 [1,1,1,2,2,2]
-- ([1,1,1,2,2],[2])
splitAtIndex k l = (take k l, drop k l)

-- Drop the element at index k in list l
-- For example "dropK 3 [0,0,0,1,0,0,0]" returns [0,0,0,0,0,0]
-- |
-- >>> dropK 3 [1,2,3,4,5,6,7]
-- [1,2,3,5,6,7]
dropK k l = (take k l) ++ (drop (k+1) l)

-- Extract elements between ith and kth element in list l. Including i, but not k
-- For example, "slice 3 6 [0,0,0,1,2,3,0,0,0]" returns [1,2,3]
-- |
-- >>> slice 3 6 [0,0,0,1,2,3,0,0,0]
-- [1,2,3]
slice i k l = take (k-i) (drop i l)

-- Insert element x in list l at index k
-- For example, "insertElem 2 5 [0,0,0,0,0,0]" returns [0,0,0,0,0,2,0]
-- |
-- >>> insertElem 2 5 [0,0,0,0,0,0]
-- [0,0,0,0,0,2,0]
insertElem x k l = (take k l) ++ [x] ++ (drop k l)

-- Rotate list l n places left.
-- For example, "rotate 2 [1,2,3,4,5]" gives [3,4,5,1,2]
-- |
-- >>> rotate 2 [1,2,3,4,5]
-- [3,4,5,1,2]
rotate n l = (drop n l) ++ (take n l)

main = pure ()