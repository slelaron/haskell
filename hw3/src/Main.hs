module Main where

import qualified Data.Map.Lazy              as Map
import           Control.Monad.State.Lazy
import           Control.Monad.Reader
import           Control.Monad.Cont
import qualified Control.Exception          as Exc
import qualified Text.Megaparsec            as P
import qualified Text.Megaparsec.Char.Lexer as L
import           Text.Megaparsec.Char
import           Text.Megaparsec.Expr
import qualified Data.Void                  as Void  
import           System.IO
import           System.Environment
import           Data.Foldable
import           Data.Text                  as T


data Expr = Lit Int             
          | Var T.Text          
          | Add Expr   Expr      
          | Sub Expr   Expr      
          | Mul Expr   Expr      
          | Div Expr   Expr      
          | Let T.Text Expr Expr
          deriving Show

data ExprException = DivisionByZero         
                   | UnknownVariable T.Text
                   deriving Show

instance Exc.Exception ExprException

evaluate :: Expr -> Reader (Map.Map T.Text Int) Int
evaluate (Lit c)               = return c
evaluate (Var name)            = do result <- asks (Map.lookup name)
                                    case result of
                                      Nothing -> Exc.throw (UnknownVariable name)
                                      Just x  -> return x
evaluate (Add first second)    = liftM2 (+) (evaluate first) (evaluate second)
evaluate (Sub first second)    = liftM2 (-) (evaluate first) (evaluate second)
evaluate (Mul first second)    = liftM2 (*) (evaluate first) (evaluate second)
evaluate (Div first second)    = do a <- evaluate first
                                    b <- evaluate second
                                    case b of
                                      0 -> Exc.throw DivisionByZero
                                      _ -> return (a `div` b)
evaluate (Let name expr expr1) = do a <- evaluate expr
                                    local (Map.insert name a) (evaluate expr1)

newtype Program = Program [Statement]
                deriving Show

data Statement = Define T.Text Expr 
               | Update T.Text Expr 
               | For T.Text Expr Expr [Statement]
               | Break 
               | Read T.Text 
               | Write Expr
               | Pure Expr
               deriving Show

type Parser = P.Parsec Void.Void T.Text

spaceConsumer :: Parser ()
spaceConsumer = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

symbol :: T.Text -> Parser T.Text
symbol a = P.try $ L.symbol spaceConsumer a

parenthesis :: Parser a -> Parser a
parenthesis = P.between (symbol "(") (symbol ")")

integer :: Parser Int
integer = P.try $ lexeme L.decimal

signedInteger :: Parser Int
signedInteger = P.try $ L.signed spaceConsumer integer

rword :: T.Text -> Parser ()
rword w = (lexeme . P.try) (string w *> P.notFollowedBy alphaNumChar)

reservedWords :: [T.Text]
reservedWords = ["let", "in", "=", "mut", "for", "{", "}", ";", "break", "until", "<", ">"]

identifier :: Parser T.Text
identifier = P.try $ (lexeme . P.try) (p >>= check)
  where
    p :: Parser T.Text
    p = T.pack <$> ((:) <$> letterChar <*> P.many alphaNumChar)
    check :: T.Text -> Parser T.Text
    check x = if x `elem` reservedWords 
              then fail $ "keyword " ++ show x ++ " cannot be an identifier"
              else return x

parseStatement :: Parser Statement
parseStatement = P.try forStatement P.<|> statementsWithSemi

statementsWithSemi :: Parser Statement
statementsWithSemi = P.try $ Data.Foldable.foldr1 (P.<|>) (Prelude.map (P.try . parseSemi) 
     [updateStatement,
      defineStatement,  
      readStatement,     
      writeStatement,    
      breakStatement,
      pureStatement])
  where
    parseSemi a = a >>= \b -> P.try (symbol ";") >> return b

programParser :: Parser Program
programParser = P.try $ Program <$> P.between spaceConsumer P.eof (P.many parseStatement)

pureStatement :: Parser Statement
pureStatement = P.try $ Pure <$> parseExpr

breakStatement :: Parser Statement
breakStatement = P.try $ const Break <$> symbol "break"

forStatement :: Parser Statement
forStatement = do rword "for"
                  name <- identifier
                  rword "="
                  expr <- parseExpr
                  rword "until"
                  expr1 <- parseExpr
                  rword "{"
                  statements <- P.many parseStatement
                  rword "}"
                  return (For name expr expr1 statements)

readStatement :: Parser Statement
readStatement = do rword ">"
                   name <- identifier
                   return (Read name)

writeStatement :: Parser Statement
writeStatement = do rword "<"
                    expr <- parseExpr
                    return (Write expr)

updateStatement :: Parser Statement
updateStatement = do name <- identifier
                     rword "="
                     expr <- arithmeticExpr
                     return (Update name expr)

defineStatement :: Parser Statement
defineStatement = do rword "mut"
                     name <- identifier
                     rword "="
                     expr <- arithmeticExpr
                     return (Define name expr)

letExpr :: Parser Expr
letExpr = do rword "let"
             name <- identifier
             rword "="
             expF <- parseExpr
             rword "in"
             expS <- parseExpr
             return (Let name expF expS)

whileExpr :: Parser Expr
whileExpr = P.between spaceConsumer P.eof parseExpr

parseExpr :: Parser Expr
parseExpr = Data.Foldable.foldr1 (P.<|>)
           [arithmeticExpr,
            variable,
            literal,               
            letExpr, 
            parenthesis parseExpr]

parseExpr1 :: Parser Expr
parseExpr1 = Data.Foldable.foldr1 (P.<|>)
            [variable,
             literal,              
             letExpr,              
             parenthesis parseExpr]

arithmeticExpr :: Parser Expr
arithmeticExpr = makeExprParser (P.try parseExpr1) arithmeticOperators

arithmeticOperators :: [[Operator Parser Expr]]
arithmeticOperators = [[InfixL (Mul <$ symbol "*"),
                        InfixL (Div <$ symbol "/")],
                       [InfixL (Add <$ symbol "+"),
                        InfixL (Sub <$ symbol "-")]]

variable :: Parser Expr
variable = Var <$> identifier

literal :: Parser Expr
literal = Lit <$> signedInteger

data ContextException = VariableNotInContext T.Text
                      | VariableIsAlreadyInContext T.Text
                      deriving Show

instance Exc.Exception ContextException

updateCtx :: T.Text -> Int -> StateT (Map.Map T.Text Int) IO ()
updateCtx key val = do truth <- gets (Map.member key) 
                       {-(lift . putStrLn) $ "updating " ++ key-}
                       if truth then modify (Map.insert key val)
                       else     Exc.throw $ VariableNotInContext key

setCtx :: T.Text -> Int -> StateT (Map.Map T.Text Int) IO ()
setCtx key val = do truth <- gets (Map.member key)
                    {-(lift . putStrLn) $ "setting " ++ key-}
                    if truth then Exc.throw $ VariableIsAlreadyInContext key
                    else     modify (Map.insert key val)

removeCtx :: T.Text -> StateT (Map.Map T.Text Int) IO ()
removeCtx key = do truth <- gets (Map.member key)
                   {-(lift . putStrLn) $ "removing " ++ key-}
                   if truth then modify (Map.delete key)
                   else     Exc.throw $ VariableNotInContext key

data BreakInfo = NeedBreak
               | NeedNotBreak

evaluateState :: [Statement] -> ContT BreakInfo (StateT (Map.Map T.Text Int) IO) ()
evaluateState statements = Data.Foldable.foldl (>>=) (return ()) $ Prelude.map func statements
  where
    func :: Statement -> () -> ContT BreakInfo (StateT (Map.Map T.Text Int) IO) ()
    func statement _ = ContT $ \f -> 
      case statement of
        Define name expr -> do
          context <- get
          setCtx name $ runReader (evaluate expr) context
          result <- f ()
          removeCtx name
          return result
        Update name expr -> do
          context <- get
          updateCtx name $ runReader (evaluate expr) context
          f ()
        Write expr -> do
          context <- get
          (lift . putStrLn) $ show (runReader (evaluate expr) context)
          f ()
        Read name -> do
          value <- lift getLine
          let num = read value :: Int
          updateCtx name num
          f ()
        For name from untiL st -> do
          context <- get
          let first = runReader (evaluate from) context
          setCtx name first
          let monad = runContT (evaluateState st) $ const (return NeedNotBreak)
          let recursive = do ctx <- get
                             let second = runReader (evaluate untiL) ctx
                             curVal <- gets (Map.! name)
                             if curVal >= second
                             then return NeedNotBreak
                             else monad >>= \a ->
                               case a of
                                 NeedBreak -> return NeedNotBreak
                                 _ -> gets (Map.! name) >>= \val -> updateCtx name (val + 1) >> recursive
          _ <- recursive
          removeCtx name
          f ()
        Pure _ -> f ()
        Break  -> return NeedBreak

main :: IO ()
main = do fileName   <- Prelude.head <$> getArgs
          fileHandle <- openFile fileName ReadMode
          content    <- hGetContents fileHandle
          let parsed = P.parse programParser "" (T.pack content)
          {-print parsed-}
          case parsed of
            Right (Program statements) -> Control.Monad.Cont.void (runStateT (runContT (evaluateState statements) $ const (return NeedNotBreak)) Map.empty)
            Left  _                    -> return ()
