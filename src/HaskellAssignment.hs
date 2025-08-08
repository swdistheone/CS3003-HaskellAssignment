module HaskellAssignment where
 import Data.List (group, sort)
------------------------------------------------
-- findFirst
------------------------------------------------
data Found = Match Int | NoMatch deriving Eq
instance Show Found where
  show (Match index) = "Found match at " ++ show index
  show NoMatch = "No match found!"
findFirst :: Eq a => (a -> Bool) -> [a] -> Found
findFirst p xs = go 0 xs
  where
    go _ [] = NoMatch
    go n (x:xs)
      | p x       = Match n
      | otherwise = go (n + 1) xs
------------------------------------------------
-- palindrome
------------------------------------------------
palindrome :: [Char] -> Bool
palindrome xs = xs == reverse xs
------------------------------------------------  
-- mergeSort
------------------------------------------------  
mergeSort :: (a -> a -> Bool) -> [a] -> [a]
mergeSort _ [] = []
mergeSort _ [x] = [x]
mergeSort cmp xs = merge (mergeSort cmp left) (mergeSort cmp right)
  where
    (left, right) = splitAt (length xs `div` 2) xs
    merge [] ys = ys
    merge xs [] = xs
    merge (x:xs) (y:ys)
      | cmp x y   = x : merge xs (y:ys)
      | otherwise = y : merge (x:xs) ys
------------------------------------------------
-- lengthEncode
------------------------------------------------
data RunLength = Span Int Char deriving Eq

instance Show RunLength where
  show (Span n c) = show n ++ " " ++ [c]

lengthEncode :: [Char] -> [RunLength]
lengthEncode = map (\grp -> Span (length grp) (head grp)) . group . sort