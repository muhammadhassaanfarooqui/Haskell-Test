import Data.List
import System.IO

factorial :: Integer -> Integer
factorial 1 = 1
factorial 0 = 1
factorial x = x * factorial(x-1)


evenOrOdd :: Int -> String
evenOrOdd x 
    | x `mod` 2 == 0 = "Even"
    | otherwise = "Odd"


areStringsEqual :: String -> String -> Bool
areStringsEqual [] [] = True
areStringsEqual (x:xs) (y:ys) = (x == y) && areStringsEqual xs ys


quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort ([a | a <- xs, a < x]) ++ [x] ++ quicksort ([b | b <- xs, b >= x])

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (reverse xs) ++ [x]

myUnionHelper :: (Ord a) => [a] -> [a] -> [a]
myUnionHelper [] ls = ls
myUnionHelper ls [] = ls
myUnionHelper (x:xs) (y:ys) = [x, y] ++ (myUnionHelper xs ys)

myUnion :: (Ord a) => [a]-> [a]-> [a]
myUnion xs ys = removeDuplicates(sorted) where
    grouped = myUnionHelper xs ys
    sorted = quicksort(grouped)

removeDuplicates :: (Ord a) => [a] -> [a]
removeDuplicates [] = []
removeDuplicates [x] = [x]
removeDuplicates (x:xs) | (x == head xs) = removeDuplicates xs
                        | otherwise = [x] ++ removeDuplicates xs  