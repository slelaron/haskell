module Implementation where


import Control.Applicative
import Control.Monad
import Text.Read           (readMaybe)
import Data.List           (stripPrefix)
import Data.Char           (isDigit, digitToInt)


data Expr = Number Int 
          | Sum Expr Expr 
          | Mul Expr Expr 
          | Sub Expr Expr 
          | Div Expr Expr 
          | Pow Expr Expr
        deriving Show

data ArithmeticError = DivizionByZero
                     | RaisingToNegativePower
                deriving (Show, Eq)


evaluate :: Expr -> Either ArithmeticError Int
evaluate (Sum a b) = liftM2 (+) (evaluate a) $ evaluate b
evaluate (Mul a b) = liftM2 (*) (evaluate a) $ evaluate b
evaluate (Sub a b) = liftM2 (-) (evaluate a) $ evaluate b
evaluate (Div a b) = liftM2 div (evaluate a) $ evaluate b >>= bad
  where
    bad :: Int -> Either ArithmeticError Int
    bad c
      | c == 0    = Left DivizionByZero
      | otherwise = return c
evaluate (Pow a b) = liftM2 (^) (evaluate a) $ evaluate b >>= bad
  where
    bad :: Int -> Either ArithmeticError Int
    bad c
      | c < 0     = Left RaisingToNegativePower
      | otherwise = return c
evaluate (Number a) = return a


bin :: Int -> [[Int]]
bin n
  | n <  0    = []
  | n == 0    = [[]]
  | otherwise = bin (n - 1) >>= \a -> [(0 : a), (1 : a)]


stringSum :: String -> Maybe Int
stringSum a = sum <$> traverse readMaybe (words a)

data Optional a = Optional (Maybe (Maybe a))
    deriving Show

instance Functor Optional where
    fmap :: (q -> w) -> Optional q -> Optional w
    fmap f (Optional a) = Optional $ (\b -> f <$> b) <$> a


instance Applicative Optional where
    pure :: a -> Optional a
    pure = Optional . Just . Just

    (<*>) :: Optional (q -> w) -> Optional q -> Optional w
    Optional Nothing         <*> _ = Optional Nothing
    Optional (Just Nothing)  <*> _ = Optional $ Just Nothing
    Optional (Just (Just f)) <*> a = fmap f a

instance Monad Optional where
    return :: a -> Optional a
    return = pure

    (>>=) :: Optional q -> (q -> Optional w) -> Optional w
    Optional Nothing         >>= _ = Optional Nothing
    Optional (Just Nothing)  >>= _ = Optional $ Just Nothing
    Optional (Just (Just a)) >>= f = f a

-- 1. Left identity

-- return a >>= f = 
-- pure a >>= f = 
-- ((Optional . Just . Just) a) >>= f = 
-- ((Optional . (\x -> Just (Just x))) a) >>= f =
-- ((\y -> Optional ((\x -> Just (Just x)) y)) a) >>= f = 
-- (Optional ((\x -> Just (Just x)) a)) >>= f = 
-- (Optional (Just (Just a))) >>= f = 
-- f a

-- 2. Right identity

-- Optional Nothing >>= return = 
-- Optional Nothing

-- Optional (Just Nothing) >>= return = 
-- Optional (Just Nothing)

-- Optional (Just (Just a)) >>= return = 
-- return a =
-- ... proved in the previous section ... =
-- (Optional (Just (Just a)))

-- 3. Associativity

-- (Optional Nothing >>= f) >>= g = 
-- Optional Nothing >>= g = 
-- Optional Nothing

-- Optional Nothing >>= (\x -> f x >>= g) = 
-- Optional Nothing

-- (Optional (Just Nothing) >>= f) >>= g = 
-- Optional (Just Nothing) >>= g =
-- Optional (Just Nothing)

-- Optional (Just Nothing) >>= (\x -> f x >>= g) = 
-- Optional (Just Nothing)

-- (Optional (Just (Just a)) >>= f) >>= g = 
-- f a >>= g

-- (Optional (Just (Just a))) >>= (\x -> f x >>= g) =
-- (\x -> f x >>= g) a = 
-- f a >>= g

instance Foldable Optional where
    foldr :: (a -> b -> b) -> b -> Optional a -> b
    foldr f start (Optional (Just (Just obj))) = f obj start
    foldr _ start _                            = start

instance Traversable Optional where
    traverse :: Applicative f => (a -> f b) -> Optional a -> f (Optional b)
    traverse _ (Optional Nothing)         = pure $ Optional Nothing
    traverse _ (Optional (Just Nothing))  = pure $ Optional $ Just Nothing
    traverse f (Optional (Just (Just a))) = (Optional . Just . Just) <$> f a

data NonEmpty a = a :| [a]
    deriving (Show, Eq)

instance Functor NonEmpty where
    fmap :: (q -> w) -> NonEmpty q -> NonEmpty w
    fmap f (a :| b) = f a :| (f <$> b)

instance Applicative NonEmpty where
    pure :: a -> NonEmpty a
    pure = (:| [])

    (<*>) :: NonEmpty (q -> w) -> NonEmpty q -> NonEmpty w
    a :| c <*> b :| d = (a b :|) $ (a <$> d) ++ (c <*> b : d)

    (*>) :: NonEmpty a -> NonEmpty b -> NonEmpty b
    _ *> b = b

    (<*) :: NonEmpty a -> NonEmpty b -> NonEmpty a
    a <* _ = a

    liftA2 :: (a -> b -> c) -> NonEmpty a -> NonEmpty b -> NonEmpty c
    liftA2 f (a :| as) (b :| bs) = (f a b :|) $ (f a <$> bs) ++ (liftA2 f as $ b : bs)

instance Monad NonEmpty where
    return :: a -> NonEmpty a
    return = pure

    (>>=) :: NonEmpty a -> (a -> NonEmpty b) -> NonEmpty b
    a :| as >>= f = let b :| bs = f a in
        (b :|) $ bs ++ (as >>= (\(y :| ys) -> y : ys) . f)

    (>>) :: NonEmpty a -> NonEmpty b -> NonEmpty b
    _ >> b = b

-- 1. Left identity

-- return a >>= f = 
-- pure a >>= f = 
-- ((:| []) a) >>= f = 
-- (a :| []) >>= f =
-- let b :| bs = f a in (b :|) $ bs ++ ([] >>= (\y :|ys) -> y : ys) . f) = 
-- let b :| bs = f a in (b :|) $ bs ++ [] = 
-- let b :| bs = f a in (b :|) $ bs = 
-- let b :| bs = f a in (b :| bs) =
-- f a

-- 2. Right identity

-- (m :| ms) >= return = 
-- (m :| ms) >= pure = 
-- (m :| ms) >= (:| [])
-- let b :| bs = (:| []) m in (b :|) $ bs ++ (ms >>= (\(y :| ys) -> y : ys) . (:| [])) =
-- let b :| bs = (:| []) m in (b :|) $ bs ++ (ms >>= \z -> (\(y :| ys) -> y : ys) (z :| [])) =
-- let b :| bs = (:| []) m in (b :|) $ bs ++ (ms >>= \z -> (z : []))
-- let b :| bs = (:| []) m in (b :|) $ bs ++ (ms >>= (: [])) = 
-- let b :| bs = (:| []) m in (b :|) $ bs ++ ms = 
-- let b :| bs = (m :| []) in (b :|) $ bs ++ ms = 
-- (m :|) $ [] ++ ms = 
-- (m :|) ms = 
-- (m :| ms)

-- 3. Associativity

-- ((m :| ms) >>= f) >>= g === (m :| ms) >>= (\x -> f x >>= g)
-- ((m :| ms) >>= f) >>= g =
-- (let b :| bs = f m in (b :|) $ bs ++ (ms >>= (\(y :| ys) -> y : ys) . f))) >>= g =
-- (let b :| bs = f m in (b :| bs ++ (ms >>= (\(y :| ys) -> y : ys) . f))) >>= g
-- | f m = (t :| ts) | = 
-- (let b :| bs = (t :| ts) in (b :| bs ++ (ms >>= (\(y :| ys) -> y : ys) . f))) >>= g = 
-- (t :| ts ++ (ms >>= (\(y :| ys) -> y : ys) . f))) >>= g
-- let b :| bs = g t in (b :|) $ bs ++ (ts ++ (ms >>= (\(y :| ys) -> y : ys) . f)) >>= (\(y :| ys) -> y : ys) . g =
-- | g t = (k :| ks) | = 
-- let b :| bs = (k :| ks) in (b :|) $ bs ++ (ts ++ (ms >>= (\(y :| ys) -> y : ys) . f)) >>= (\(y :| ys) -> y : ys) . g =
-- (k :| ks ++ (ts ++ (ms >>= (\(y :| ys) -> y : ys) . f)) >>= (\(y :| ys) -> y : ys) . g)) =
-- .. by properties of list | (ts ++ (ms >>= (\(y :| ys) -> y : ys) . f)) >>= (\(y :| ys) -> y : ys) . g)) = 
--                            (ts >>= (\(y :| ys) -> y : ys) . g) ++ (ms >>= (\(y :| ys) -> y : ys) . f) >>= (\(y :| ys) -> y : ys) . g)) | .. =
-- (k :| ks ++ (ts >>= (\(y :| ys) -> y : ys) . g) ++ (ms >>= (\(y :| ys) -> y : ys) . f) >>= (\(y :| ys) -> y : ys) . g))) =
-- .. Associativity law for list is proved | ((ms >>= (\(y :| ys) -> y : ys) . f) >>= (\(y :| ys) -> y : ys) . g) = 
--                                           (ms >>= (\x -> ((\(y :| ys) -> y : ys) . f) x >>= (\(y :| ys) -> y : ys) . g)).. = 
-- (k :| ks ++ (ts >>= (\(y :| ys) -> y : ys) . g) ++ (ms >>= (\x -> ((\(y :| ys) -> y : ys) . f) x >>= (\(y :| ys) -> y : ys) . g))) =
-- (k :| ks ++ (ts >>= (\(y :| ys) -> y : ys) . g) ++ (ms >>= (\x -> (\(y :| ys) -> y : ys) (f x) >>= (\(y :| ys) -> y : ys) . g)))
-- (m :| ms) >>= (\x -> f x >>= g) = 
-- let b :| bs = (\x -> f x >>= g) m in (b :|) $ bs ++ (ms >>= (\(y :| ys) -> y : ys) . (\x -> f x >>= g)) = 
-- let b :| bs = (f m >>= g) in (b :|) $ bs ++ (ms >>= (\(y :| ys) -> y : ys) . (\x -> f x >>= g)) = 
-- | f m = (t :| ts) | = 
-- let b :| bs = ((t :| ts) >>= g) in (b :| bs ++ (ms >>= (\(y :| ys) -> y : ys) . (\x -> f x >>= g))) = 
-- let b :| bs = (let b :| bs = g t in (b :|) $ bs ++ (ts >>= (\(y :| ys) -> y : ys) . g)) in (b :| bs ++ (ms >>= (\(y :| ys) -> y : ys) . (\x -> f x >>= g))) =
-- | g t = (k :| ks) | =
-- let b :| bs = (let b :| bs = (k :| ks) in (b :| bs ++ (ts >>= (\(y :| ys) -> y : ys) . g))) in (b :| bs ++ (ms >>= (\(y :| ys) -> y : ys) . (\x -> f x >>= g))) =
-- let b :| bs = (k :| ks ++ (ts >>= (\(y :| ys) -> y : ys) . g)) in (b :| bs ++ (ms >>= (\(y :| ys) -> y : ys) . (\x -> f x >>= g))) =
-- (k :| ks ++ (ts >>= (\(y :| ys) -> y : ys) . g) ++ (ms >>= (\(y :| ys) -> y : ys) . (\x -> f x >>= g))) =
-- (k :| ks ++ (ts >>= (\(y :| ys) -> y : ys) . g) ++ (ms >>= \z -> (\(y :| ys) -> y : ys) (f z >>= g))) =



instance Foldable NonEmpty where
    foldr :: (a -> b -> b) -> b -> NonEmpty a -> b
    foldr f start (first :| other) = f first $ foldr f start other

    foldMap :: Monoid m => (a -> m) -> NonEmpty a -> m
    foldMap f (first :| other) = f first `mappend` foldMap f other

    foldl :: (b -> a -> b) -> b -> NonEmpty a -> b
    foldl f start (first :| other) = foldl f start (first : other)

instance Traversable NonEmpty where
    traverse :: Applicative f => (a -> f b) -> NonEmpty a -> f (NonEmpty b)
    traverse f (a :| as) = liftA2 (:|) (f a) $ traverse f as

    sequenceA :: Applicative f => NonEmpty (f a) -> f (NonEmpty a)
    sequenceA (a :| as) = liftA2 (:|) a $ sequenceA as

    mapM :: Monad m => (a -> m b) -> NonEmpty a -> m (NonEmpty b)
    mapM = traverse

    sequence :: Monad m => NonEmpty (m a) -> m (NonEmpty a)
    sequence = sequenceA

data Parser s a = Parser { runParser :: [s] -> Maybe (a, [s]) }

instance Functor (Parser s) where
    fmap :: (a -> b) -> Parser s a -> Parser s b
    fmap f p = Parser $ \x -> (\(q, w) -> (f q, w)) <$> runParser p x

instance Applicative (Parser s) where
    pure :: a -> Parser s a
    pure x = Parser $ (\s -> return (x, s))

    (<*>) :: Parser s (a -> b) -> Parser s a -> Parser s b
    pf <*> ps = Parser $ \x      -> runParser pf x >>= 
                         \(a, b) -> runParser ps b >>= 
                         \(f, s) -> return (a f, s)


instance Monad (Parser s) where
    return :: a -> Parser s a
    return x = pure x

    (>>=) :: Parser s a -> (a -> Parser s b) -> Parser s b
    p >>= f = Parser $ \x -> runParser p x >>= \(a, b) -> runParser (f a) b

instance Alternative (Parser s) where
    empty :: Parser s a
    empty = Parser $ const empty

    (<|>) :: Parser s a -> Parser s a -> Parser s a
    pf <|> ps = Parser $ \x -> runParser pf x <|> runParser ps x

ok :: Parser s ()
ok = Parser $ \x -> return ((), x)

eof :: Parser s ()
eof = Parser $ \x -> case x of
    [] -> return ((), x)
    _  -> empty

satisfy :: (s -> Bool) -> Parser s s
satisfy p = Parser $ \x -> case x of
    a : b -> case p a of
        True -> return (a, b)
        _    -> empty
    _     -> empty

element :: Eq s => s -> Parser s s
element s = satisfy (== s) 

pmany :: Eq s => Parser s s -> Parser s [s]
pmany p = Parser $ \x -> case runParser (psome p) x of
    Nothing           -> return ([]    , x)
    Just (b :| bs, a) -> return (b : bs, a)

psome :: Eq s => Parser s s -> Parser s (NonEmpty s)
psome p = Parser $ \x       -> runParser p x >>= 
                   \(y, ys) -> runParser (pmany p) ys >>= 
                   \(z, zs) -> return (y :| z, zs)

stream :: Eq s => [s] -> Parser s [s]
stream s = Parser $ \x -> stripPrefix s x >>= \b -> return (s, b)

parenthesis :: Parser Char ()
parenthesis = let inter = (element '(' >>= const inter >>= const (element ')') >>= const inter) <|> ok in
    inter >>= const eof

integer :: Parser Char Int
integer = (satisfy isSign <|> (pure '+')) >>= \x -> transform x <$> pnumber
  where
    pnumber :: Parser Char Int
    pnumber   = foldl (\b a -> 10 * b + (digitToInt a)) 0 <$> (psome (satisfy isDigit))

    isSign :: Char -> Bool
    isSign x = x == '-' || x == '+'

    transform :: Char -> Int -> Int
    transform c num = case c of 
        '-' -> -num
        _   -> num

pinteger :: Parser Char Int
pinteger = integer >>= \x -> eof >>= const (return x)

listOflist :: Parser Char [[Int]]
listOflist = wrap integer >>= 
    \x -> ntimes x >>= 
    \y -> (y :) <$> ((wrap (element ',') >>= const listOflist) <|> (wrap eof >>= const (pure [])))
  where
    ntimes :: Int -> Parser Char [Int]
    ntimes n 
      | n == 0    = pure []
      | otherwise = wrap (element ',') >>= 
                    const (wrap integer) >>= 
                    \x -> (x :) <$> ntimes (n - 1)

    blink :: Parser Char String
    blink = pmany (satisfy $ \x -> elem x " \t\n")

    wrap :: Parser Char a -> Parser Char a
    wrap p = blink >>= const p