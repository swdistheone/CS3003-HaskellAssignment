module HaskellAssignment where
  import Data.List (group)
------------------------------------------------
-- findFirst
------------------------------------------------
data Found = Match Int | NoMatch
 deriving Eq

instance Show Found where
  show (Match i) = "Found match at " ++ show i
  show NoMatch = "No match found!"

findFirst :: a => (a -> Bool) -> [a] -> Found
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
mergesort cmp xs = 
  let (l, r) = splitAt (length xs `div` 2) xs
  in merge (mergesort cmp l) (mergesort cmp r)
  where
    merge [] ys = ys
    merge xs [] = xs
    merge xa@(x:xs') ya@(y:ys')
      | cmp x y   = x : merge xs' ya
      | otherwise = y : merge xa ys'
------------------------------------------------
-- runLengthEncode
------------------------------------------------
data RunLength = Span Int Char 
  deriving (Eq, Show)

instance Show RunLength where
  show (Span n c) = "(span " ++ show n ++ " " ++ show c ++ ")"

runLengthEncode :: String -> [RunLength]
runLengthEncode = map (\grp -> Span (length grp) (head grp)) . group 