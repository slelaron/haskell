module Property where

import Hedgehog

import qualified Implementation     as Impl
import           Data.List                    (group)
import           Control.Monad                (join)
import qualified Hedgehog.Gen       as Gen
import qualified Hedgehog.Range     as Range
import qualified Data.List.NonEmpty as NE     (NonEmpty ((:|)))

genInt :: Gen Int
genInt = Gen.int (Range.linear 0 20)

prop_bin :: Property
prop_bin = property $
    forAll genInt >>=
    \x -> let res = Impl.bin x in
        (length (group res) == 2 ^ x, length res == 2 ^ x) === (True, True)

genString :: Gen String
genString = Gen.string (Range.linear 0 15) (Gen.element "-   0123456789")

rightToMaybe :: Either a b -> Maybe b
rightToMaybe (Right a) = Just a
rightToMaybe _         = Nothing

prop_stringSum :: Property
prop_stringSum = property $
    forAll genString >>=
    \x -> let res     = Impl.stringSum x
              toCheck = foldr Impl.Sum (Impl.Number 0) <$> 
                        (sequenceA $ map (\y -> Impl.Number . (\(z, _) -> z) <$> 
                            Impl.runParser Impl.pinteger y) $ words x) in
        res === (join $ rightToMaybe $ sequenceA (Impl.evaluate <$> toCheck))

genIntNonEmpty :: Gen (NE.NonEmpty Int)
genIntNonEmpty =
  let listLength = Range.linear 0 10000
  in  Gen.nonEmpty listLength Gen.enumBounded

convertNonEmpty :: NE.NonEmpty a -> Impl.NonEmpty a
convertNonEmpty (a NE.:| b) = a Impl.:| b

prop_monad_leftId :: Property
prop_monad_leftId = property $
    forAll genInt >>= 
        \x -> (return x >>= (Impl.:| []), return x >>= (\z -> (-z) Impl.:| []), return x >>= (\z -> (z * 2) Impl.:| [])) === (x Impl.:| [], (-x) Impl.:| [] , (2 * x) Impl.:| [])

prop_monad_rightId :: Property
prop_monad_rightId = property $
    forAll genIntNonEmpty >>= 
        \x -> let y = convertNonEmpty x in
            (y >>= return) === y

prop_monad_associativity :: Property
prop_monad_associativity = property $
    forAll genIntNonEmpty >>= 
        \y -> let x     = convertNonEmpty y
                  funcs :: [(Int -> Impl.NonEmpty Int)]
                  funcs = [(Impl.:| []), (\z -> (-z) Impl.:| []), (\z -> (z * 2) Impl.:| []), (\z -> z Impl.:| [z])] in
            [(x >>= f) >>= g | f <- funcs, g <- funcs] === [x >>= (\z -> f z >>= g) | f <- funcs, g <- funcs]