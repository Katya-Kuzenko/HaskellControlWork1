{- PROBLEM 1
It is often needed to convert a number written in arabic symbols,
to a string of its textual representation, i.e. for financial documents.
Write a function intToWords that transcribes an integer into its 
textual representation in format "digit-digit-digit-...". 
-}
import Data.List (intersperse, group, sort, maximumBy)
import Data.Function (on)

intToWords :: (Num a, Show a) => a -> String
intToWords x = concat $ intersperse "-" $ map digitToWord (show x)
 where 
   digitToWord '0' = "zero"
   digitToWord '1' = "one"
   digitToWord '2' = "two"
   digitToWord '3' = "three"
   digitToWord '4' = "four"
   digitToWord '5' = "five"
   digitToWord '6' = "six"
   digitToWord '7' = "seven"
   digitToWord '8' = "eight"
   digitToWord '9' = "nine"
   digitToWord '-' = "minus"

problem1 = do
  print "Problem 1"
  print $ intToWords 150  
  print $ intToWords 0  
  print $ intToWords (-10) 


{- PROBLEM 2
Write a function findMaxFrequency that for a given homogenous list of type a
returns a pair (a, Int) of the most frequent element (any, if there are more than one) and its frequecy. For an empty list throw an error. 
-}

findMaxFrequency :: (Ord a) => [a] -> (a, Int)
findMaxFrequency lst = if null lst
                        then error "Empty lst"
                        else maximumBy (compare `on` snd) $ couples lst
                       where
                        couples = map (\xs -> (head xs, length xs)) . group . sort


problem2 = do
  print "Problem 2"
  print $ findMaxFrequency [1,2,1,3,1,4]   -- (1, 3)
  print $ findMaxFrequency [1,1,2,2]       -- (1, 2) or (2, 2)
  print $ findMaxFrequency "some sentence" -- ('e', 4)
  print $ findMaxFrequency ([] :: String)  -- error

{-  
{- PROBLEM 3
For a given system of types that represent a file system structure
write a function search that given a name returns a list of all paths
that correspond to that name.
-}

type Name   = String
type Path   = String
data FSNode = File Name | Dir Name [FSNode]

search :: Name -> FSNode -> [Path]
search name root = -- ???

root = Dir "/"
  [
    Dir "folder1" 
    [
      File "file1",
      Dir  "folder2" 
      [
        File "file2",
        File "file3"
      ],
      Dir  "folder3" 
      [
        File "file3",
        File "file4"
      ],
      File "file5"
    ]
  ]

problem3 = do
  print "Problem 3"
  print $ search "file1" root -- ["//folder1/file1"]
  print $ search "file3" root -- ["//folder1/folder2/file3", "//folder1/folder3/file3"]
  print $ search "file4" root -- ["//folder1/folder3/file4"]
  print $ search "file6" root -- []

-- please, make sure your code runs without errors
-- comment out unsolved tasks here
main = do
  problem1
  problem2
  problem3
-}