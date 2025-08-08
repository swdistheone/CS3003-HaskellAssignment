module HaskellAssignment where
 import Data.List (group)
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
-- mergesort
------------------------------------------------  
mergesort :: (a -> a -> Bool) -> [a] -> [a]
mergesort _ [] = []
mergesort _ [x] = [x]
mergesort cmp xs = merge (mergesort cmp left) (mergesort cmp right)
  where
    (left, right) = splitAt (length xs `div` 2) xs
    merge [] ys = ys
    merge xs [] = xs
    merge (x:xs) (y:ys)
      | cmp x y   = x : merge xs (y:ys)
      | otherwise = y : merge (x:xs) ys
------------------------------------------------
-- runLengthEncode
------------------------------------------------
data RunLength = Span Int Char 
  deriving (Eq, Show)

instance Show RunLength where
  show (Span n c) = "(span " ++ show n ++ " " ++ show c ++ ")"

runLengthEncode :: String -> [RunLength]
runLengthEncode = map (\grp -> Span (length grp) (head grp)) . group 