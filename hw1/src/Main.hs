module Main where

import           Dummy
import           System.Random            (newStdGen, randomRs)
import           Data.List                (sort)
import           Data.Semigroup           (Semigroup ((<>)))
import           Data.Monoid              (Sum (Sum, getSum))

import qualified Data.List.NonEmpty as NE (NonEmpty ((:|)))

main :: IO ()
main = putStrLn (test1 ++ test2 ++ test3)

randomIntList :: Int -> Int -> Int -> IO [Int]
randomIntList n from to = take n . randomRs (from, to) <$> newStdGen

order3Tests :: [((Int, Int, Int), (Int, Int, Int))]
order3Tests = [(1, 2, 3), (1, 3, 2), (2, 1, 3), (2, 3, 1), (3, 1, 2), (3, 2, 1)] `zip` (replicate 6 (1, 2, 3))

checkOneArg :: Eq b => (a -> b) -> [(a, b)] -> String -> String
checkOneArg f argAns name = if all (\(args, ans) -> f args == ans) argAns 
                            then name ++ " -> Correct\n" 
                            else name ++ " -> InCorrect " ++ show (map (\(args, ans) -> f args == ans) argAns) ++ "\n"

checkTwoArgs :: Eq b => (a1 -> a2 -> b) -> [((a1, a2), b)] -> String -> String
checkTwoArgs f argAns name = if all (\((first, second), ans) -> f first second == ans) argAns 
                            then name ++ " -> Correct\n" 
                            else name ++ " -> InCorrect " ++ show (map (\((first, second), ans) -> f first second == ans) argAns) ++ "\n"

checkOrder3 :: String
checkOrder3 = checkOneArg order3 order3Tests "order3"

smartReplicateTests :: [([Int], [Int])]
smartReplicateTests = [[1, 2, 3, 4], [0, 1, 2], [1], [3, 3, 3, 3]] `zip` [[1, 2, 2, 3, 3, 3, 4, 4, 4, 4], [1, 2, 2], [1], [3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3]]

checkSmartReplicate :: String
checkSmartReplicate = checkOneArg smartReplicate smartReplicateTests "smartReplicate"

containsTests :: [((Int, [[Int]]), [[Int]])]
containsTests = [(1, [[1, 2, 3, 4, 5]]), (2, [[1, 2, 3, 4, 5]]), (3, [[1, 2, 3, 4, 5]]), (4, [[1, 2, 3, 4, 5]]), (5, [[1, 2, 3, 4, 5]])] `zip` (replicate 5 [[1, 2, 3, 4, 5]])

checkContains :: String
checkContains = checkTwoArgs contains containsTests "contains"

mergeSortTests :: [([Int], [Int])]
mergeSortTests = let now = [[1, 2, 3, 4, 5], [], [5, 4, 3, 2, 1]] in now `zip` map sort now

checkMergeSort :: String
checkMergeSort = checkOneArg mergeSort mergeSortTests "mergeSort"

erasingTests :: [((Int, [Int]), (Maybe Int, [Int]))]
erasingTests = [(1, [1, 2, 3, 4, 5]), (2, [1, 2, 3, 4, 5]), (5, [1, 2, 3, 4, 5]), (0, [1, 2, 3, 4, 5]), (-1, [1, 2, 3, 4, 5])] `zip` [(Just 2, [1, 3, 4, 5]), (Just 3, [1, 2, 4, 5]), (Nothing, [1, 2, 3, 4, 5]), (Just 1, [2, 3, 4, 5]), (Nothing, [1, 2, 3, 4, 5])]

checkErasing :: String
checkErasing = checkTwoArgs erasing erasingTests "erasing"

days :: [Week]
days = [Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday]

nextDayTests :: [(Week, Week)]
nextDayTests = days `zip` [Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday]

checkNextDay :: String
checkNextDay = checkOneArg nextDay nextDayTests "nextDay"

test1 :: String 
test1 = foldr1 (++) [checkOrder3, checkSmartReplicate, checkContains, checkNextDay, checkMergeSort, checkErasing, checkAfterDays, checkIsWeekend, checkDaysToParty]

afterDaysTests :: [((Week, Int), Week)]
afterDaysTests = [(Sunday, 7), (Sunday, 6), (Saturday, 0), (Saturday, 7), (Saturday, 6), (Monday, 4)] `zip` [Sunday, Saturday, Saturday, Saturday, Friday, Friday]

checkAfterDays :: String
checkAfterDays = checkTwoArgs afterDays afterDaysTests "afterDays"

isWeekendTest :: [(Week, Bool)]
isWeekendTest =  days `zip` [True, False, False, False, False, False, True]

checkIsWeekend :: String
checkIsWeekend = checkOneArg isWeekend isWeekendTest "isWeekend"

daysToPartyTests :: [(Week, Int)]
daysToPartyTests = days `zip` [5, 4, 3, 2, 1, 0, 6]

checkDaysToParty :: String
checkDaysToParty = checkOneArg daysToParty daysToPartyTests "daysToParty"

-- Natural numbers
test2 :: String
test2 = foldr1 (++) [checkSum, checkMul, checkAbs, checkSignum, checkFromInteger, checkSub, checkQuotRem, checkIsTreeEmpty, checkSizeOfTree, checkInsertIntoTree, checkEraseInTree]

sumTests :: [((Nat, Nat), Nat)]
sumTests = [(0, 1), (2, 3), (10, 1000), (1, 1)] `zip` [1, 5, 1010, 2]

checkSum :: String
checkSum = checkTwoArgs (+) sumTests "Nat.+"

mulTests :: [((Nat, Nat), Nat)]
mulTests = [(0, 1), (2, 3), (10, 1000), (1, 1)] `zip` [0, 6, 10000, 1]

checkMul :: String
checkMul = checkTwoArgs (*) mulTests "Nat.*"

absTests :: [(Nat, Nat)]
absTests = [0, 1, 2, 3, 4, 5] `zip` [0, 1, 2, 3, 4, 5]

checkAbs :: String
checkAbs = checkOneArg abs absTests "Nat.abs"

signumTests :: [(Nat, Nat)]
signumTests = [0, 1, 2, 3, 4, 5] `zip` [0, 1, 1, 1, 1, 1]

checkSignum :: String
checkSignum = checkOneArg signum signumTests "Nat.signum"

fromIntegerTests :: [(Integer, Nat)]
fromIntegerTests = [0, 1, 2, 3, 4, 5] `zip` [0, 1, 2, 3, 4, 5]

checkFromInteger :: String
checkFromInteger = checkOneArg fromInteger fromIntegerTests "Nat.fromInteger"

subTests :: [((Nat, Nat), Nat)]
subTests = ([0, 1, 2, 3, 10, 10, 10] `zip` [0, 1, 2, 2, 5, 1, 0]) `zip` [0, 0, 0, 1, 5, 9, 10]

checkSub :: String
checkSub = checkTwoArgs (-) subTests "Nat.-"

quotRemTests :: [((Nat, Nat), (Nat, Nat))]
quotRemTests = ([0, 2, 2, 3, 5, 7] `zip` [1, 1, 2, 2, 3, 3]) `zip` [(0, 0), (2, 0), (1, 0), (1, 1), (1, 2), (2, 1)]

checkQuotRem :: String
checkQuotRem = checkTwoArgs quotRem quotRemTests "quotRem"

isTreeEmptyTests :: [(Tree Int, Bool)]
isTreeEmptyTests = [Leaf, Node (1 NE.:| []) Leaf Leaf] `zip` [True, False]

checkIsTreeEmpty :: String
checkIsTreeEmpty = checkOneArg isTreeEmpty isTreeEmptyTests "isTreeEmpty"

trees :: [Tree Int]
trees = [Leaf, Node (1 NE.:| [1, 1, 1]) Leaf Leaf, Node (2 NE.:| []) (Node (1 NE.:| []) Leaf Leaf) (Node (3 NE.:| []) Leaf Leaf)]

sizeOfTreeTests :: [(Tree Int, Int)]
sizeOfTreeTests = trees `zip` [0, 4, 3]

checkSizeOfTree :: String
checkSizeOfTree = checkOneArg sizeOfTree sizeOfTreeTests "sizeOfTree"

insertIntoTreeTests :: [((Int, Tree Int), Tree Int)]
insertIntoTreeTests = ([0, 1, 2, 1, 2, 4] `zip` (trees ++ trees)) `zip` [Node (0 NE.:| []) Leaf Leaf, 
                                                                         Node (1 NE.:| [1, 1, 1, 1]) Leaf Leaf,
                                                                         Node (2 NE.:| [2]) (Node (1 NE.:| []) Leaf Leaf) (Node (3 NE.:| []) Leaf Leaf),
                                                                         Node (1 NE.:| []) Leaf Leaf, 
                                                                         Node (1 NE.:| [1, 1, 1]) Leaf (Node (2 NE.:| []) Leaf Leaf),
                                                                         Node (2 NE.:| []) (Node (1 NE.:| []) Leaf Leaf) (Node (3 NE.:| []) Leaf (Node (4 NE.:| []) Leaf Leaf))]

checkInsertIntoTree :: String
checkInsertIntoTree = checkTwoArgs insertIntoTree insertIntoTreeTests "insertIntoTree"

eraseInTreeTests :: [((Int, Tree Int), Tree Int)]
eraseInTreeTests = ([0, 1, 1, 1, 2, 2] `zip` (trees ++ trees)) `zip` [Leaf, 
                                                                      Node (1 NE.:| [1, 1]) Leaf Leaf,
                                                                      Node (2 NE.:| []) Leaf (Node (3 NE.:| []) Leaf Leaf),
                                                                      Leaf, 
                                                                      Node (1 NE.:| [1, 1, 1]) Leaf Leaf,
                                                                      Node (1 NE.:| []) Leaf (Node (3 NE.:| []) Leaf Leaf)]

checkEraseInTree :: String
checkEraseInTree = checkTwoArgs insertIntoTree insertIntoTreeTests "eraseInTree"

test3 :: String
test3 = foldr1 (++) [checkPairFoldr, checkPairFoldMap, checkNonEmptyFoldr, checkTreeFoldMap, checkSplitOn, checkJoinWith, checkMaybeConcat, checkEitherConcat, checkNonEmptySemigroup]

pairFoldrTests :: [(Pair Int, Int)]
pairFoldrTests = [Pair 1 2, Pair 5 6, Pair 0 6] `zip` [5, 17, 12]

checkPairFoldr :: String
checkPairFoldr = checkOneArg (foldr (\x y -> x + 2 * y) 0) pairFoldrTests "pairFoldr"

pairFoldMapTests :: [(Pair Int, [Int])]
pairFoldMapTests = [Pair 1 2, Pair 5 6, Pair 0 6] `zip` [[1, 2], [5, 6], [0, 6]]

checkPairFoldMap :: String
checkPairFoldMap = checkOneArg (foldMap (\x -> [x])) pairFoldMapTests "pairFoldMap"

nonEmptyFoldrTests :: [(NonEmpty Int, Int)]
nonEmptyFoldrTests = [1 :| [2, 3, 4], 5 :| [6], 12 :| []] `zip` [49, 17, 12]

checkNonEmptyFoldr :: String
checkNonEmptyFoldr = checkOneArg (foldr (\x y -> x + 2 * y) 0) nonEmptyFoldrTests "nonEmptyFoldr"

nonEmptyFoldMapTests :: [(NonEmpty Int, [Int])]
nonEmptyFoldMapTests = [1 :| [2, 3, 4], 5 :| [6], 12 :| []] `zip` [[1, 2, 3, 4], [5, 6], [12]]

checkNonEmptyFoldMap :: String
checkNonEmptyFoldMap = checkOneArg (foldMap (\x -> [x])) nonEmptyFoldMapTests "nonEmptyFoldMap"

treeFoldMapTests :: [(Tree Int, [Int])]
treeFoldMapTests = ((fromList [5, 1, 4, 3, 2]) : trees) `zip` [[1, 2, 3, 4, 5], [], [1, 1, 1, 1], [1, 2, 3]]

checkTreeFoldMap :: String
checkTreeFoldMap = checkOneArg (foldMap (\x -> [x])) treeFoldMapTests "treeFoldMap"

splitOnTests :: [((Char, String), NonEmpty String)]
splitOnTests = [('/', "path/to/file"), (' ', "My name is Nikita"), (',', "Hello, World")] `zip` ["path" :| ["to", "file"], "My" :| ["name", "is", "Nikita"], "Hello" :| [" World"]]

checkSplitOn :: String
checkSplitOn = checkTwoArgs splitOn splitOnTests "splitOn"

joinWithTests :: [((Char, NonEmpty String), String)]
joinWithTests = [('/', "path" :| ["to", "file"]), (' ', "My" :| ["name", "is", "Nikita"]), (',', "Hello" :| [" World"])] `zip` ["path/to/file", "My name is Nikita", "Hello, World"]

checkJoinWith :: String
checkJoinWith = checkTwoArgs joinWith joinWithTests "joinWith"

maybeConcatTests :: [([Maybe [Int]], [Int])]
maybeConcatTests = [[Just [1,2,3], Nothing, Just [4,5]], [Nothing, Nothing, Nothing], [Just [1, 2, 3, 4, 5]]] `zip` [[1, 2, 3, 4, 5], [], [1, 2, 3, 4, 5]]

checkMaybeConcat :: String
checkMaybeConcat = checkOneArg maybeConcat maybeConcatTests "maybeConcat"

eitherConcatTests :: [([Either (Sum Int) [Int]], (Sum Int, [Int]))]
eitherConcatTests = [[Left (Sum 3), Right [1,2,3], Left (Sum 5), Right [4,5]]] `zip` [(Sum {getSum = 8}, [1, 2, 3, 4, 5])]

checkEitherConcat :: String
checkEitherConcat = checkOneArg eitherConcat eitherConcatTests "eitherConcat"

nonEmptySemigroupTests :: [((NonEmpty Int, NonEmpty Int), NonEmpty Int)]
nonEmptySemigroupTests = [(1 :| [], 2 :| []), (1 :| [2], 2 :| [3])] `zip` [1 :| [2], 1 :| [2, 2, 3]]

checkNonEmptySemigroup :: String
checkNonEmptySemigroup = checkTwoArgs (<>) nonEmptySemigroupTests "nonEmpty.<>"

thisOrThatSemigroupTests :: [((ThisOrThat Int Int, ThisOrThat Int Int), ThisOrThat Int Int)]
thisOrThatSemigroupTests = ([This 1, That 2, Both 1 2, This 1] `zip` [That 2, This 1, Both 3 4, This 2]) `zip` [Both 1 2, Both 1 2, Both 1 2, This 1]

checkThisOrThat :: String
checkThisOrThat = checkTwoArgs (<>) thisOrThatSemigroupTests "thisOrThat.<>"

stringSumPassTests :: [String]
stringSumPassTests = [ "1", "1 2 3", " 1", "1 ", "\t1\t", "\t12345\t", "010 020 030", " 123 456 789 ", "-1", "-1 -2 -3", "\t-12345\t", " -123 -456 -789 ", "\n1\t\n3   555  -1\n\n\n-5", "123\t\n\t\n\t\n321 -4 -40"]
stringSumFailTests :: [String]
stringSumFailTests  = ["asd", "1-1", "1.2", "--2", "+1", "1+"]
