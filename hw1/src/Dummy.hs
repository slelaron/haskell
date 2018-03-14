module Dummy where

import           Data.List                (sort)
import           Data.Ratio               ((%))
import           Data.Either              (partitionEithers)
import           Data.Semigroup           (Semigroup ((<>)))

import qualified Data.List.NonEmpty as NE (NonEmpty ((:|)))

order3 :: Ord a => (a, a, a) -> (a, a, a)
order3 = (\(a : b : c : []) -> (a, b, c)) . sort . (\(a, b, c) -> (a : b : [c]))

smartReplicate :: Enum a => [a] -> [a]
smartReplicate (x : xs) = replicate (fromEnum x) x ++ smartReplicate xs
smartReplicate [] = []

contains :: Eq a => a -> [[a]] -> [[a]]
contains = filter . elem

stringSum :: String -> Int
stringSum = foldr1 (+) . map (\x -> read x :: Int) . words

mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort array = internalSort $ map (\x -> [x]) array
  where
    merge :: Ord a => [a] -> [a] -> [a]
    merge [] ys             = ys
    merge xs []             = xs
    merge (x : xs) (y : ys) = if x < y then x : (merge xs (y : ys)) else y : (merge (x : xs) ys)
    internalSort :: Ord a => [[a]] -> [a]
    internalSort x = case len of
         1 -> head x
         _ -> internalSort $ map (uncurry merge) $ uncurry zip $ splitAt (div (len + 1) 2) $ if even len then x else [] : x
      where
        len :: Int
        len = length x

erasing :: Int -> [a] -> (Maybe a, [a])
erasing _ []      = (Nothing, [])
erasing 0 (b : y) = (Just b, y)
erasing x (b : y) = if x < 0 
                    then (Nothing, b : y)
                    else let (el, rest) = erasing (x - 1) y 
                            in (el, b : rest)

data Week = Sunday 
          | Monday 
          | Tuesday 
          | Wednesday 
          | Thursday 
          | Friday 
          | Saturday
          deriving (Enum, Bounded, Show, Eq)

afterDays :: Week -> Int -> Week
afterDays day amount = toEnum $ rem (fromEnum day + amount) $ (+1) $ fromEnum (maxBound :: Week)

nextDay :: Week -> Week
nextDay = (`afterDays` 1)

isWeekend :: Week -> Bool
isWeekend Sunday   = True
isWeekend Saturday = True
isWeekend _        = False

daysToParty :: Week -> Int
daysToParty day = (amount + fromEnum Friday - fromEnum day) `rem` amount
  where
    amount :: Int
    amount = fromEnum (maxBound :: Week) + 1

data SpiritualBuildings = Library 
                        | Church

data Walls = Walls

data Lord = Lord

data Castle = Castle 
    { walls :: (Maybe Walls),
      lord  :: (Maybe Lord)
    }

data Citizens = OneCitizen 
              | TwoCitizens 
              | ThreeCitizens
              | FourCitizens
              deriving Enum

type Houses = NE.NonEmpty Citizens

data Town = Town
    { castle    :: (Maybe Castle),
      buildings :: (Maybe SpiritualBuildings),
      houses    :: Houses
    }

buildCastle :: Town -> (Town, Bool)
buildCastle town@Town { castle = Nothing } = (town { castle = Just (Castle Nothing Nothing) }, True)
buildCastle x                              = (x, False)

buildSpiritual :: Town -> (Town, Bool) 
buildSpiritual town@Town { buildings = Nothing } = (town { buildings = Just Library }, True)
buildSpiritual x                                 = (x, False)

buildHouse :: Town -> Citizens -> Town
buildHouse town@Town { houses = (house NE.:| another) } citizens
    = town { houses = (house NE.:| (citizens : another)) }

populateCastle :: Town -> Either String Town
populateCastle Town { castle = Nothing }
    = Left "There is no castle"
populateCastle Town { castle = (Just (Castle { lord = (Just Lord) })) }
    = Left "Castle is already populated"
populateCastle town@Town { castle = (Just townCastle) }
    = Right town { castle = Just townCastle { lord = Just Lord } }

buildWalls :: Town -> Town
buildWalls town@Town { castle = Just (townCastle@Castle { walls = Nothing }), houses = townHouses }
    = if countCitizens townHouses >= 10 
      then town { castle = Just townCastle { walls = (Just Walls) } } 
      else town
  where
    countCitizens :: Houses -> Int
    countCitizens = foldr (\x y -> fromEnum x + 1 + y) 0
buildWalls x = x

data Nat = Z | S Nat

instance Eq Nat where
    (==) :: Nat -> Nat -> Bool
    Z == Z     = True
    S a == S b = a == b
    _ == _     = False


instance Ord Nat where
    (<=) :: Nat -> Nat -> Bool
    Z   <= _   = True
    S a <= S b = a <= b
    _   <= _   = False

instance Num Nat where
    (+) :: Nat -> Nat -> Nat
    Z   + a   = a
    a   + Z   = a
    S a + S b = S $ S $ a + b

    (*) :: Nat -> Nat -> Nat
    Z   * _ = Z 
    S a * b = a * b + b

    abs :: Nat -> Nat
    abs = id

    signum :: Nat -> Nat
    signum Z = Z
    signum _ = S Z

    fromInteger :: Integer -> Nat
    fromInteger 0 = Z
    fromInteger a = S $ fromInteger $ a - 1

    (-) :: Nat -> Nat -> Nat
    Z   - _   = Z
    a   - Z   = a
    S a - S b = a - b

instance Enum Nat where
    toEnum :: Int -> Nat 
    toEnum 0 = Z
    toEnum a = S $ toEnum $ a - 1

    fromEnum :: Nat -> Int
    fromEnum Z     = 0
    fromEnum (S a) = fromEnum a + 1


instance Real Nat where
    toRational :: Nat -> Rational
    toRational a = (internal a) % 1
      where
        internal :: Nat -> Integer
        internal Z     = 0
        internal (S b) = internal b + 1


instance Integral Nat where
    toInteger :: Nat -> Integer
    toInteger Z     = 0
    toInteger (S a) = toInteger a + 1

    quot :: Nat -> Nat -> Nat
    quot a b = internal Z (S a)
      where
        divideInMiddle :: Nat -> Nat
        divideInMiddle (S (S c)) = S (divideInMiddle c)
        divideInMiddle (S Z)     = Z
        divideInMiddle Z         = Z

        internal :: Nat -> Nat -> Nat
        internal l r
            | S l == r  = l
            | otherwise = let m = divideInMiddle (l + r) in
                if m * b <= a 
                then internal m r 
                else internal l m

    rem :: Nat -> Nat -> Nat
    rem a b = a - (a `quot` b) * b

    quotRem :: Nat -> Nat -> (Nat, Nat)
    quotRem a b = (a `quot` b, a `rem` b)

data Tree a = Node (NE.NonEmpty a) (Tree a) (Tree a)
                  | Leaf
                  deriving Eq

instance Show a => Show (Tree a) where
    show :: Tree a -> String
    show Leaf                = "#"
    show (Node a left right) = "(" ++ (show a) ++ " " ++ (show left) ++ " " ++ (show right) ++ ")"

isTreeEmpty :: Tree a -> Bool
isTreeEmpty Leaf = True
isTreeEmpty _    = False

sizeOfTree :: Tree a -> Int
sizeOfTree Leaf            = 0
sizeOfTree (Node keys l r) = sizeOfTree l + (sizeOfTree r) + (length keys)

findInTree :: forall a. Ord a => a -> Tree a -> Maybe (NE.NonEmpty a)
findInTree need tree = internal tree
  where
    internal :: Tree a -> Maybe (NE.NonEmpty a)
    internal Leaf = Nothing
    internal (Node keys@(key NE.:| _) left right) 
        | key == need  = Just keys
        | key < need   = internal right
        | otherwise    = internal left

insertIntoTree :: forall a. Ord a => a -> Tree a -> Tree a
insertIntoTree need tree = internal tree
  where
    internal :: Tree a -> Tree a
    internal Leaf = Node (need NE.:| [])  Leaf Leaf
    internal (Node args@(key NE.:| keys) left right)
        | key == need = Node (need NE.:| (key : keys)) left right
        | key > need  = Node args (internal left) right
        | otherwise   = Node args left (internal right)

eraseInTree :: forall a. Ord a => a -> Tree a -> Tree a
eraseInTree need tree = internal tree
  where
    transform :: Tree a -> Maybe (Tree a, NE.NonEmpty a)
    transform (Node keys left Leaf)  = Just (left, keys)
    transform (Node keys left right) = let result = transform right in
        case result of 
            Just (first, second) ->
                Just (Node keys left first, second)
            Nothing              ->
                Nothing 
    transform Leaf = Nothing

    internal :: Tree a -> Tree a
    internal Leaf = Leaf
    internal (Node args@(key NE.:| keys) left right)
        | key >  need      = Node args (internal left) right
        | key <  need      = Node args left (internal right)
        | length keys > 0  = Node (key NE.:| (tail keys)) left right
        | otherwise        = let result = transform left in 
            case result of
                Nothing              -> right
                Just (first, second) -> Node second first right

fromList :: Ord a => [a] -> Tree a
fromList = foldr insertIntoTree Leaf

data Pair a = Pair a a

data NonEmpty a = a :| [a]
    deriving Show

instance Eq a => Eq (NonEmpty a) where
    (==) :: NonEmpty a -> NonEmpty a -> Bool
    a :| b == c :| d = a == c && b == d

instance Foldable Pair where
    foldr :: (a -> b -> b) -> b -> Pair a -> b
    foldr f st (Pair first second) = f first $ f second st

    foldMap :: Monoid m => (a -> m) -> Pair a -> m
    foldMap f (Pair first second) = f first `mappend` (f second)

instance Foldable NonEmpty where
    foldr :: (a -> b -> b) -> b -> NonEmpty a -> b
    foldr f st (first :| other) = f first $ foldr f st other

    foldMap :: Monoid m => (a -> m) -> NonEmpty a -> m
    foldMap f (first :| other) = f first `mappend` (foldMap f other)

instance Foldable Tree where
    foldMap :: forall a m. Monoid m => (a -> m) -> Tree a -> m
    foldMap f a = internal a mempty
      where
        internal :: Tree a -> m -> m
        internal Leaf up = up
        internal (Node (key NE.:| keys) left right) up 
            = internal right $ mconcat $ internal left up : (f key) : (map f keys)

splitOn :: forall a. Eq a => a -> [a] -> NonEmpty [a]
splitOn a = foldr internal ([] :| [])
  where
    internal :: a -> NonEmpty [a] -> NonEmpty [a]
    internal need (x :| xs)
        | need == a = ([] :| (x : xs))
        | otherwise = ((need : x) :| xs)

joinWith :: a -> NonEmpty [a] -> [a]
joinWith a (q :| qs) = foldr1 (\x y -> x ++ (a : y)) (q : qs)

maybeConcat :: [Maybe [a]] -> [a]
maybeConcat = internal . mconcat
  where
    internal :: Maybe [a] -> [a]
    internal (Just a)  = a
    internal Nothing   = mempty

eitherConcat :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
eitherConcat a = let (f, s) = partitionEithers a in (mconcat f, mconcat s)

instance Semigroup (NonEmpty a) where
    (<>) :: NonEmpty a -> NonEmpty a -> NonEmpty a
    a :| as <> b :| bs = a :| (as ++ (b : bs))

data ThisOrThat a b = This a
                    | That b 
                    | Both a b
                    deriving Eq

instance Semigroup (ThisOrThat a b) where
    (<>) :: ThisOrThat a b -> ThisOrThat a b -> ThisOrThat a b
    This a <> That b   = Both a b
    This a <> Both _ c = Both a c
    That b <> This a   = Both a b
    That b <> Both a _ = Both a b
    that   <> _        = that

data Name = Name String
          | Noname

instance Semigroup Name where
    (<>) :: Name -> Name -> Name
    Name a <> Name b = Name $ a ++ ('.' : b)
    Noname <> Name a = Name a
    a      <> Noname = a

instance Monoid Name where
    mappend :: Name -> Name -> Name 
    mappend = (<>)
    mempty = Noname

newtype Endo a = Endo {getEndo :: a -> a}

instance Semigroup (Endo a) where
    (<>) :: Endo a -> Endo a -> Endo a
    Endo f <> Endo g = Endo $ f . g

instance Monoid (Endo a) where
    mappend :: Endo a -> Endo a -> Endo a
    mappend = (<>)
    mempty = Endo id

data Builder = One Char 
             | Many [Builder]
             deriving Show

instance Semigroup Builder where
    (<>) :: Builder -> Builder -> Builder
    a <> b = Many [a, b]

instance Monoid Builder where
    mappend :: Builder -> Builder -> Builder
    mappend = (<>)
    mempty = Many []

fromString :: String -> Builder
fromString = Many . map (\x -> One x)

toString :: Builder -> String
toString = internal ""
  where
    internal :: String -> Builder -> String
    internal up (Many a) = foldr (\c b -> toString c `mappend` b) up a
    internal up (One a) = a : up