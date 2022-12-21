module Main where

import Data.Function
import Data.List
import Data.Char


on3 :: (b -> b -> b -> c) -> (a -> b) -> a -> a -> a -> c
on3 op f x y z = op (f x) (f y) (f z)

main :: IO ()
main = putStrLn "Hello, Haskell!"

class Printable p where
    toString :: p -> [Char]

instance Printable Bool where
    toString True = "true"
    toString False = "false"

instance Printable () where
    toString () = "unit type"

instance (Printable a, Printable b) => Printable (a, b) where
    toString p = "(" ++ toString (fst p) ++ "," ++ toString (snd p) ++ ")"

class KnownToGork a where
    stomp :: a -> a
    doesEnrageGork :: a -> Bool

class KnownToMork a where
    stab :: a -> a
    doesEnrageMork :: a -> Bool

class (KnownToGork a, KnownToMork a) => KnownToGorkAndMork a where
    stompOrStab :: a -> a
    stompOrStab a | doesEnrageMork a && doesEnrageGork a = (stomp . stab) a
                  | doesEnrageMork a = stomp a
                  | doesEnrageGork a = stab a
                  | otherwise = a

class (Enum a, Bounded a, Eq a) => SafeEnum a where
    ssucc :: a -> a
    ssucc a | maxBound == a = minBound
            | otherwise = succ a

    spred :: a -> a
    spred a | minBound == a = maxBound
            | otherwise = pred a

instance SafeEnum Bool where
   {- ssucc a | maxBound == a = minBound
            | otherwise = succ a
    
    spred a | minBound == a = maxBound
            | otherwise = pred a-}

instance SafeEnum Int where

avg :: Int -> Int -> Int -> Double
avg a b c = (fromIntegral a + fromIntegral b + fromIntegral c) / 3.0


nTimes:: a -> Int -> [a]
nTimes _ 0 = []
nTimes a n = helper [a] a (n - 1)
    where
        helper :: [a] -> a -> Int -> [a]
        helper as a 0 = as
        helper as a n = helper (a : as) a (n - 1)

oddsOnly :: Integral a => [a] -> [a]
oddsOnly [] = []
oddsOnly (a:as) | odd a = a : oddsOnly as
                | otherwise = oddsOnly as

isPalindrome :: Eq a => [a] -> Bool
isPalindrome as =
    let
        half = length as `div` 2
        l = take half as
        r = rev as
        rr = take half r
    in
            l == rr

rev :: [a] -> [a]
rev as = hlpr as []
    where
        hlpr :: [a] -> [a] -> [a]
        hlpr [] sas = sas
        hlpr (a:as) sas = hlpr as (a : sas)


sum3 :: Num a => [a] -> [a] -> [a] -> [a]
sum3 as bs cs = map sum $ zip3' as bs cs

zip3' :: Num a => [a] -> [a] -> [a] -> [[a]]
zip3' as bs cs = hlpr as bs cs []
    where
        hlpr :: Num a => [a] -> [a] -> [a] -> [[a]] -> [[a]]
        hlpr [] (b:bs) (c:cs) l = hlpr [] bs cs (l ++ [[0,b,c]])
        hlpr (a:as) [] (c:cs) l = hlpr as [] cs (l ++ [[a,0,c]])
        hlpr (a:as) (b:bs) [] l = hlpr as bs [] (l ++ [[a,b,0]])
        hlpr (a:as) (b:bs) (c:cs) l = hlpr as bs cs (l ++ [[a,b,c]])
        hlpr [] [] [] l = l
        hlpr [] [] (c:cs) l = hlpr [] [] cs (l ++ [[0,0,c]])
        hlpr (a:as) [] [] l = hlpr as [] [] (l ++ [[a,0,0]])
        hlpr [] (b:bs) [] l = hlpr [] bs [] (l ++ [[0,b,0]])

{-
    GHCi> groupElems []
        []

    GHCi> groupElems [1,2]
        [[1],[2]]

    GHCi> groupElems [1,2,2,2,4]
        [[1],[2,2,2],[4]]

    GHCi> groupElems [1,2,3,2,4]
        [[1],[2],[3],[2],[4]]
-}

groupElems :: Eq a => [a] -> [[a]]
groupElems [] = []
groupElems lst@(a:_) =
            let (eq, other) = span (==a) lst
            in eq : groupElems other


{-
    GHCi> readDigits "365ads"
        ("365","ads")

    GHCi> readDigits "365"
        ("365","")
-}

readDigits :: String -> (String, String)
readDigits = span isDigit

{-
    GHCi> filterDisj (< 10) odd [7,8,10,11,12]
        [7,8,11]
-}

filterDisj :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
filterDisj p1 p2 = filter (p1 `or'` p2) 

or' :: (a -> Bool) -> (a -> Bool) -> a -> Bool
or' p1 p2 a = p1 a || p2 a

{-
    GHCi> qsort [1,3,2,5]
        [1,2,3,5]
-}

qsort :: Ord a => [a] -> [a]
qsort [a] = [a]
qsort as  = qsrt as 0 (length as)
    where 
        qsrt :: Ord a => [a] -> Int -> Int -> [a]
        qsrt [] _ _             = []
        qsrt as l r | l >= r    = []
                    | otherwise = 
                        let m   = prtition as l r
                        in qsrt (qsrt as l (m - 1)) (m + 1) r
        
prtition :: Ord a => [a] -> Int -> Int -> Int
prtition as l r = 

swap :: Int -> Int -> [a] -> [a]
swap i j as | i == j    = as
            | i > j     = swap j i as
            | otherwise = before ++ [jel] ++ btw ++ [iel] ++ rest
                where
                    before = take (i - 1) as
                    jel    = as !! (j - 1)
                    iel    = as !! (i - 1)
                    btw    = between i j as
                    rest   = drop j as

between :: Int -> Int -> [a] -> [a]
between i j []             = []
between i j as | i >= j    = []
               | otherwise = take (j - i - 1) (drop i as)
