module Analysis (analyze) where

import           Control.Monad        (forM, forM_, mapM_, when)
import           Control.Monad.State
import           Control.Monad.Writer

import           Data.List            (intercalate)
import qualified Data.Map.Strict      as Map

import           Types

-- core monad
type Analysis = WriterT [SemanticError] (State Scope)


-- | adds name to the scope without checking for existing declarations.
-- This is useful shadowing declarations (e.g. function parameters or loop
-- variables). For actual declarations use @declare which will emit an error for
-- declaring a already declared variable.
addName :: Ident -> CType -> Analysis ()
addName n t = modify $ \(Scope m) -> (Scope (Map.insert n t m))

lookupType :: Ident -> Analysis (Maybe CType)
lookupType n = do
  (Scope m) <- get
  return $ Map.lookup n m

declare :: Ident -> CType -> Analysis ()
declare n t = do
  x <- lookupType n
  case x of
    Nothing  -> addName n t
    (Just _) -> tell [AlreadyDeclaredName n]

-- some data types
-- TODO: Move to Types.hs
newtype Path = Path [Ident]
  deriving (Show, Eq)

data CType = Primitive Type
           | Pointer CType
           | Tuple [CType]
           | Function CType [CType]
           | Bottom -- c.f. undefined in Haskell
           deriving (Eq)


newtype Scope = Scope (Map.Map Ident CType)

instance Show Scope where
  show (Scope m) = unlines $ map f (Map.toList m)
    where f (k,v) = show k ++ " :: " ++ show v

instance Show CType where
  show (Primitive t) = show t
  show (Pointer t) = "*" ++ show t
  show (Function t p) = "(" ++ pl ++ ") -> " ++ show t
    where pl = intercalate ", " $ map show p
  show (Tuple p) = "(" ++ pl ++ ")"
    where pl = intercalate ", " $ map show p

emptyScope :: Scope
emptyScope = Scope Map.empty

-- TODO: Add locations
data SemanticError = UndeclaredName Ident
                   | AlreadyDeclaredName Ident
                   | TypeMismatch CType CType
               deriving (Show)
--------------------------------------------------------------------------------

-- | either returns a list of errors or the "global scope"
analyze :: TranslationUnit -> ([SemanticError], Scope)
analyze u = let ((_,w),s) = runState (runWriterT (translationUnit u)) emptyScope
            in (w, s)

runAnalyzer :: Analysis a -> Scope ->(a, [SemanticError], Scope)
runAnalyzer a s = let ((b,w),s') = runState (runWriterT a) s
                  in (b,w,s')

-- | runs an analysis in a copy of the current scope without modifying the
-- original scope, but retains the errors.
enterScope :: Analysis a -> Analysis ()
enterScope a = do
    s <- get
    let (_,errs,_) = runAnalyzer a s
    tell errs

translationUnit :: TranslationUnit -> Analysis ()
translationUnit (TranslationUnit es) = mapM_ externalDeclaration es


externalDeclaration :: ExternalDeclaration -> Analysis ()
externalDeclaration e = case e of
  (Left d) -> declaration d
  (Right (FunctionDefinition t d stmt) ) -> do
    let (fn, t') = declarator (Primitive t) d
    enterScope $ do
      addName fn t'
      statement stmt

declaration :: Declaration -> Analysis ()
declaration (Declaration t is) = forM_ is (initDeclarator (Primitive t))

statement :: Stmt -> Analysis ()
statement (LabeledStmt _ stmt)      = statement stmt
statement (CompoundStmt xs)         = enterScope $ forM_ xs (either declaration statement)
statement (IfStmt e s1 Nothing)     = expression e >> statement s1
statement (IfStmt e s1 (Just s2))   = expression e >> statement s1 >> statement s2
statement (Goto _)                  = return ()
statement Continue                  = return ()
statement Break                     = return ()
statement (Return Nothing)          = return ()
statement (Return (Just e))         = void $ expression e  -- TODO: Check return type
statement (ExpressionStmt Nothing)  = return ()
statement (ExpressionStmt (Just e)) = void $ expression e


-- | Should: Matches two types and returns the more general type.
--   Currently: "Matching"  means equality
matchTypes :: CType -> CType -> Analysis CType
matchTypes t1 t2 = do
  when (t1 /= t2) $ tell [TypeMismatch t1 t2]
  return t1

matchTypes_ :: CType -> CType -> Analysis ()
matchTypes_ t1 t2 = void $ matchTypes t1 t2

expression :: Expr -> Analysis CType
expression (List es) = Tuple <$> forM es expression

expression (Ternary e1 e2 e3) = do
  t1 <- expression e1
  _ <- matchTypes t1 (Primitive Int) -- TODO: No bool type?
  t2 <- expression e2
  t3 <- expression e3
  matchTypes t2 t3


expression (BExpr bop e1 e2) = do
    t1 <- expression e1
    t2 <- expression e2
    if bop `elem` [Plus, Minus, Mult, LessThan, LAnd, LOr]
      then do
        matchTypes_ t1 (Primitive Int)
        matchTypes_ t2 (Primitive Int)
        return (Primitive Int)
      else if bop `elem` [NotEqual, EqualsEquals]
           then do
              matchTypes_ t1 t2
              return (Primitive Int) -- should be Bool
      else return (Primitive Void) -- should not happen

expression (UExpr op e) = error "unary expression not implemented"

expression (ExprIdent n) = do
  mt <- lookupType n
  case mt of
    Nothing -> do
      tell [UndeclaredName n]
      return Bottom
    (Just t) -> return t

expression (Assign e1 e2) = do
  t1 <- expression e1
  t2 <- expression e2
  matchTypes t1 t2

expression (Constant _) = return (Primitive Int)

initDeclarator :: CType -> InitDeclarator -> Analysis ()
initDeclarator t (InitializedDec d _ ) = let (n,t') = declarator t d
                                         in declare n t'


-- general pattern: analyse type and modify the Analysis state
declarator :: CType -> Declarator -> (Ident, CType)
declarator t (DeclaratorId i) = (i, t)
declarator t (IndirectDeclarator n d) = declarator (it n Pointer t) d
declarator t (FunctionDeclarator d ps) =
  let (n, t') = declarator t d
      p = map parameter ps
  in (n, Function t' p)

abstractDeclarator :: CType -> AbstractDeclarator -> CType
abstractDeclarator = undefined -- TODO: Basically as declarator but abstract

parameter :: Parameter -> CType
parameter (Parameter t d) = snd $ declarator (Primitive t) d
parameter (AbstractParameter t Nothing) = Primitive t
parameter (AbstractParameter t (Just ad)) = abstractDeclarator (Primitive t) ad


it :: Int -> (a -> a) -> a -> a
it 0 _ x = x
it n f x = f (it (n-1) f x)
