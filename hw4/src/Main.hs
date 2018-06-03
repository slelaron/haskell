module Main where

import Language.Haskell.TH (runQ, Q, Exp)
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Syntax
import Data.Number.Nat
import Control.Exception
import Control.Monad.State.Lazy
import Control.Monad.Identity
import Control.Applicative
import Control.Lens.Traversal
import Control.Lens.Fold
import Control.Lens.Each
import System.FilePath.Posix
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified System.File.Tree as SFT
import qualified Data.Lens.Light as L
import qualified Control.Lens.Lens as LL

data TemplateException = TemplateException
    deriving Show

instance Exception TemplateException

chooseByIndices :: Nat -> [Nat] -> Q Exp
chooseByIndices size indices = if any (>= size) indices 
    then throw TemplateException
    else do 
        s  <- Map.fromList <$> (sequenceA $ map (\a -> (a,) <$> newName "x") indices)
        let prefix = map (\x -> let el = Map.lookup x s in case el of
                                                         Just q  -> varP q 
                                                         Nothing -> wildP) [0..(size - 1)]
        let suffix = map (\x -> varE $ (Map.!) s x) indices
        lamE [tupP prefix] (tupE suffix)

class ShowText a where
    showText :: a -> T.Text

generateShowText :: Name -> Q [Dec]
generateShowText name = do
    TyConI (DataD _ _ _ _ cs _) <- reify name

    let showCns = let
            func name len = do
                names <- sequenceA $ Prelude.replicate len (newName "x") 
                let nameSt = nameBase name
                match (conP name (varP <$> names)) (normalB [|(T.pack ('(' : nameSt)) `T.append` (if len == 0 then "" else " ") `T.append` (T.intercalate " " $(listE $ map ((appE [|showText|]) . varE) names)) `T.append` ")"|]) []

            rhs = map (\(NormalC name types) -> func name $ Prelude.length types) cs in
                [| \x -> $(caseE [|x|] rhs) |]

    [d|instance ShowText $(conT name) where showText = $showCns|]

type Lens s t a b = forall f . Functor f => (a -> f b) -> s -> f t
type Lens' s a  = Lens s s a a

--type Traversal s t a b = forall f . Applicative f => (a -> f b) -> s -> f t
--type Traversal' s a  = Traversal s s a a

set  :: Lens' s a -> a -> s -> s         -- set    value (setter)
set lns a s = runIdentity $ lns (Identity . const a) s
view :: Lens' s a -> s -> a              -- lookup value (getter)
view lns s = getConst $ lns Const s
over :: Lens' s a -> (a -> a) -> s -> s  -- change value (modifier)
over lns fn s = runIdentity $ lns (Identity . fn) s

(.~) :: Lens' s a -> a -> s -> s
(.~) lns a s = set lns a s
(^.) :: s -> Lens' s a -> a
s ^. lns = view lns s
(%~) :: Lens' s a -> (a -> a) -> s -> s
(%~) lns fn s = over lns fn s

-- _1 :: Functor f => (a -> f b) -> (a, x) -> f (b, x)
_1 :: Lens (a, x) (b, x) a b
_1 fn (a, x) = (,x) <$> fn a

-- _2 :: Functor f => (a -> f b) -> (x, a) -> f (x, b)
_2 :: Lens (x, a) (x, b) a b
_2 fn (x, a) = (x,) <$> fn a

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens get set fn s = set s <$> fn (get s)

-- Объединить две линзы в одну, которая работает с Either.
choosing :: Lens s1 t1 a b 
         -> Lens s2 t2 a b
         -> Lens (Either s1 s2) (Either t1 t2) a b
choosing l1 l2 fn (Left  s) = Left  <$> l1 fn s
choosing l1 l2 fn (Right s) = Right <$> l2 fn s

-- Изменить цель линзы и вернуть новый результат. Постарайтесь
-- реализовать без анонимных функций и определений своих функций
(<%~) :: Lens s t a b -> (a -> b) -> s -> (b, t)
(<%~) l f s = l (\x -> (f x, f x)) s 

-- Изменить цель линзы, но вернуть старый результат.
(<<%~) :: Lens s t a b -> (a -> b) -> s -> (a, t)
(<<%~) l f s = l (\x -> (x, f x)) s


data FS 
    = Dir 
          { name     :: FilePath  -- название папки, не полный путь
          , contents :: [FS]
          }
    | File
          { name     :: FilePath  -- название файла, не полный путь
          }
    deriving Show

getMyFS :: FilePath -> IO FS
getMyFS path = SFT.getDirectory' path >>= (helper "")
  where
    helper :: FilePath -> SFT.FSTree -> IO FS
    helper path cur = do let nm = L.getL SFT.label cur
                         let curPath = path </> nm
                         is <- SFT.isDir curPath
                         if not is then return (File nm)
                                   else Dir nm <$> mapM (helper curPath) (L.getL SFT.children cur)

chLens :: Lens' FS [FS]
chLens = lens (\case File _ -> []; other -> contents other) (\obj ch -> case obj of
                                                                          File _ -> obj
                                                                          obj    -> obj { contents = ch })

nameLens :: Lens' FS FilePath
nameLens = lens name (\obj nm -> obj { name = nm })

cd :: FilePath -> Traversal' FS FS
cd path fn = chLens (sequenceA . map (\case x@(Dir _ _) -> if (x ^. nameLens) == path then fn x else pure x; other -> pure other))

ls :: Traversal' FS FilePath
ls = chLens . traversed . nameLens

file :: FilePath -> Traversal' FS FS
file path fn = chLens (sequenceA . map (\case x@(File _) -> if (x ^. nameLens) == path then fn x else pure x; other -> pure other))

isFile :: Traversal' FS FS
isFile fn = \case x@(File _) -> fn x; other -> pure other

isEmptyDir :: Traversal' FS FS
isEmptyDir fn = \case x@(Dir _ []) -> fn x; other -> pure other

-- Can't find that functions
setT  :: Traversal' s a -> a -> s -> s         -- set    value (setter)
setT lns a s = runIdentity $ lns (Identity . const a) s
overT :: Traversal' s a -> (a -> a) -> s -> s  -- change value (modifier)
overT lns fn s = runIdentity $ lns (Identity . fn) s

changeExt :: FS -> String -> FS
changeExt fs ext = overT (chLens . traversed . isFile . nameLens) (flip replaceExtension ext) fs

removeEmpty :: FS -> FilePath -> [FS]
removeEmpty fs dir = ((cd dir) . isEmptyDir) (const []) fs