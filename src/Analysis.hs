{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE EmptyCase         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Analysis where

import           Control.Monad        (forM, when)
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.List            (intercalate)
import qualified Data.Map.Strict      as Map
import           Text.Megaparsec.Pos  (SourcePos)

import           Parser               (SynPhase)
import           Types
--------------------------------------------------------------------------------
-- Define Types for our Type-Annotated AST
--------------------------------------------------------------------------------

data SemPhase


type instance AnnTranslationUnit  SemPhase   =  Scope
type instance AnnFunctionDefinition SemPhase = (SourcePos, Scope)
type instance AnnTernary SemPhase            = (SourcePos, Scope, CType)
type instance AnnAssign SemPhase             = (SourcePos, Scope, CType)
type instance AnnArray SemPhase              = (SourcePos, Scope, CType)
type instance AnnBExpr SemPhase              = (SourcePos, Scope, CType)
type instance AnnUExpr SemPhase              = (SourcePos, Scope, CType)
type instance AnnFunc SemPhase               = (SourcePos, Scope, CType)
type instance AnnSizeOfType SemPhase         = (SourcePos, Scope, CType)
type instance AnnExprIdent SemPhase          = (SourcePos, Scope, CType)
type instance AnnConstant SemPhase           = (SourcePos, Scope, CType)
type instance AnnFieldAccess SemPhase        = (SourcePos, Scope, CType)
type instance AnnPointerAccess SemPhase      = (SourcePos, Scope, CType)
type instance AnnStringLiteral SemPhase      = (SourcePos, Scope, CType)
type instance AnnDeclaration SemPhase        = (SourcePos, Scope)
type instance AnnStructDeclaration SemPhase  = (SourcePos, Scope, CType)

type instance AnnIndirectDeclarator SemPhase = DeclaratorSemAnn
type instance AnnDeclaratorId SemPhase       = DeclaratorSemAnn
type instance AnnFunctionDeclarator SemPhase = DeclaratorSemAnn

type instance AnnParameter SemPhase = (SourcePos, Scope, CType)
type instance AnnAbstractParameter SemPhase = (SourcePos, Scope, CType)

type instance AnnCompoundStmt SemPhase       = (SourcePos, Scope)
type instance AnnIfStmt SemPhase             = (SourcePos, Scope)
type instance AnnWhileStmt SemPhase          = (SourcePos, Scope)
type instance AnnGoto SemPhase               = (SourcePos, Scope)
type instance AnnContinue SemPhase           = (SourcePos, Scope)
type instance AnnBreak SemPhase              = (SourcePos, Scope)
type instance AnnReturn SemPhase             = (SourcePos, Scope)
type instance AnnLabeledStmt SemPhase        = SourcePos

data DeclaratorSemAnn = DeclaratorSemAnn
  { _position :: SourcePos
  , _scope    :: Scope
  , _type     :: CType
  , _name     :: Ident
  }

class HasType x where
  getType :: x -> CType

class HasName x where
  getName :: x -> Ident

class HasScope x where
  getScope :: x -> Scope

-- how to get the type of an expression?
instance HasType (Expr SemPhase) where
  getType (BExpr (p,s,t) _ _ _)= t
  getType (Assign (p,s,t)  _ _)= t
  getType (List es)              = Tuple $ map getType es
  getType (Ternary _ _ _ _)      = error "ternary"
  getType (UExpr _ _ _)          = error "unary"
  getType (Func _ _ _ )          = error "func"
  getType (Constant _ _ )        = error "const"
  getType (Array _ _ _ )         = error "const"
  getType (SizeOfType _ _ )      = error "sizeoftype"
  getType (ExprIdent (p,s,t) n ) = t

instance HasType (Parameter SemPhase) where
  getType (Parameter (p,s,t) _ _ ) = t

instance HasType (Declarator SemPhase) where
  getType = _type . getDeclaratorSemAnn

instance HasName (Declarator SemPhase) where
  getName = _name . getDeclaratorSemAnn

instance HasScope (TranslationUnit SemPhase) where
  getScope (TranslationUnit a _) = a

class GetDeclaratorSemAnn x where
  getDeclaratorSemAnn :: x -> DeclaratorSemAnn

instance GetDeclaratorSemAnn ()

instance GetDeclaratorSemAnn (Declarator SemPhase) where
  getDeclaratorSemAnn (IndirectDeclarator a _ _) = a
  getDeclaratorSemAnn (DeclaratorId a _)         = a
  getDeclaratorSemAnn (FunctionDeclarator a _ _) = a



-- class HasType x where
--   getType :: x -> CType

-- instance HasType (Expr SemPhase) where -- TODO: Does that really work?
--   getType _ = error "niy"

-- class HasName x where
--   getName :: x -> Ident

-- instance GetName (Declarator SemPhase) where
--   getName (Declarator a _) = _name a

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------


-- | Any analysis that runs inside the @Analysis monad can write @SemanticErrors
-- and has a "scope" which acts as state.
type Analysis = WriterT [SemanticError] (State Scope)


-- | Adds name to the scope without checking for existing declarations.
-- This is useful shadowing declarations (e.g. function parameters or loop
-- variables). For actual declarations use @declare which will emit an error for
-- declaring a already declared variable.
addName :: Ident -> CType -> Analysis ()
addName n t = modify $ \(Scope m) -> (Scope (Map.insert n t m))

lookupType :: Ident -> Analysis (Maybe CType)
lookupType n = do
  (Scope m) <- get
  return $ Map.lookup n m

declare :: SourcePos -> Ident -> CType -> Analysis ()
declare pos n t = do
  x <- lookupType n
  case x of
    Nothing  -> addName n t
    (Just _) -> tell [AlreadyDeclaredName pos n]

-- some data types
data CType = CInt
           | CVoid
           | CChar
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
  show CInt = "Int"
  show CVoid = "Void"
  show CChar = "Char"
  show Bottom         = "⊥"
  show (Pointer t)    = "*" ++ show t
  show (Function t p) = "(" ++ pl ++ ") -> " ++ show t
    where pl          = intercalate ", " $ map show p
  show (Tuple p)      = "(" ++ pl ++ ")"
    where pl          = intercalate ", " $ map show p

emptyScope :: Scope
emptyScope = Scope Map.empty

fromType :: Type x -> CType
fromType Void = CVoid
fromType Int  = CInt
fromType Char = CChar
fromType _    = Bottom -- TODO:

data SemanticError = UndeclaredName !SourcePos !Ident
                   | AlreadyDeclaredName !SourcePos !Ident
                   | TypeMismatch {_pos :: !SourcePos, _leftType :: !CType, _rightType :: !CType }
               deriving (Show)

--------------------------------------------------------------------------------
runAnalyzer :: Analysis a -> Scope ->(a, [SemanticError], Scope)
runAnalyzer a s = let ((b,w),s') = runState (runWriterT a) s
                  in (b,w,s')

semanticAnalysis :: TranslationUnit SynPhase -> Either [SemanticError] (TranslationUnit SemPhase)
semanticAnalysis u = case runState (runWriterT (translationUnit u)) emptyScope of
        ((x,[]),_)  -> Right x
        ((_,err),_) -> Left err

-- f u = case runAnalyzer (translationUnit u) emptyScope of
--         (b,[],_) -> Right b
--         (b,err,_) -> Left err

-- | runs an analysis in a copy of the current scope without modifying the
-- original scope, but retains the errors.
enterScope :: Analysis a -> Analysis a
enterScope a = do
    s <- get
    let (a',errs,_) = runAnalyzer a s
    tell errs
    return a'

-- | Should: Matches two types and returns the more general type.
--   Currently: "Matching"  means equality
matchTypes :: SourcePos -> CType -> CType -> Analysis CType
matchTypes _ Bottom _ = return Bottom
matchTypes _ _ Bottom = return Bottom
matchTypes p t1 t2 = do
  when (t1 /= t2) $ tell [TypeMismatch p t1 t2]
  return t1

--------------------------------------------------------------------------------
-- Analyses
--------------------------------------------------------------------------------

either' f g = either (fmap Left . f) (fmap Right . g)

translationUnit :: TranslationUnit SynPhase -> Analysis (TranslationUnit SemPhase)
translationUnit (TranslationUnit _ eds) = do
  eds' <- forM eds (either' declaration functionDefinition)
  s <- get
  return $ TranslationUnit s eds'

-- with side-effect
functionDefinition :: FunctionDefinition SynPhase -> Analysis (FunctionDefinition SemPhase)
functionDefinition (FunctionDefinition pos t d stmt)  = do
    scope <- get
    d' <- declarator (fromType t) d
    tx <- typeA t
    declare pos (getName d') (getType d')
    stmt' <- enterScope $ do
      statement stmt
    return $ FunctionDefinition (pos, scope) tx d' stmt'

typeA :: Type SynPhase -> Analysis (Type SemPhase)
typeA Void = return Void
typeA Int  = return Int
typeA Char = return Char

declarator :: CType -> Declarator SynPhase -> Analysis (Declarator SemPhase)
declarator t (DeclaratorId p n) = do
  s <- get
  return (DeclaratorId (DeclaratorSemAnn p s t n) n)

declarator t (IndirectDeclarator p n d) = declarator (it n Pointer t) d

declarator t (FunctionDeclarator p d params) = do
  s <- get
  d' <- declarator t d
  params' <- mapM parameter params
  let paramsType = map getType params'
  return $ FunctionDeclarator (DeclaratorSemAnn p s (Function t paramsType) (getName d')) d' params'

parameter :: Parameter SynPhase -> Analysis (Parameter SemPhase)
parameter (Parameter p t d) = do
  s <- get
  t' <- typeA t
  d' <- declarator (fromType t) d
  return $ Parameter (p,s, getType d') t' d'

declaration :: Declaration SynPhase -> Analysis (Declaration SemPhase)
declaration (Declaration pos t is) = do
  scope <- get
  is' <- forM is $ \i -> do
     i'@(InitializedDec d _) <- initDeclarator (fromType t) i
     declare pos (getName d) (getType d)--TODO: <- this is the crucial point
     return i'
  t' <- typeA t
  return $ Declaration (pos, scope) t' is'

--declaration :: Declaration SynPhase -> Analysis (Declaration SemPhase)
--declaration (Declaration _ t is) = forM_ is $ \i -> do
--  let (n, t') = (initDeclarator (Primitive t) i)
--  declare n t'


initDeclarator :: CType -> InitDeclarator SynPhase -> Analysis (InitDeclarator SemPhase)
initDeclarator t (InitializedDec d mi) = do
  d' <- declarator t d
  mi' <- case mi of
    Nothing  -> return Nothing
    (Just i) -> Just <$> initializer i
  return $ InitializedDec d' mi'

initializer :: Initializer SynPhase -> Analysis (Initializer SemPhase)
initializer (InitializerAssignment e) = do
  e' <- expression e
  return $ InitializerAssignment e'
initializer (InitializerList es) = do
  es' <- mapM initializer es
  return $ InitializerList es'

abstractDeclarator :: CType -> AbstractDeclarator SynPhase -> CType
abstractDeclarator = undefined -- TODO: Basically as declarator but abstract

--parameter :: Parameter SynPhase -> CType
--parameter (Parameter t d)                 = snd $ declarator (fromType t) d
--parameter (AbstractParameter t Nothing)   = fromType t
--parameter (AbstractParameter t (Just ad)) = abstractDeclarator (fromType t) ad


statement :: Stmt SynPhase -> Analysis (Stmt SemPhase)
statement (LabeledStmt p lbl stmt)      = do
  stmt' <- statement stmt
  return $ LabeledStmt p lbl stmt'

statement (CompoundStmt pos xs) = do
  s <- get
  stmts' <- enterScope $ forM xs (either (fmap Left . declaration) (fmap Right . statement))
  return $ CompoundStmt (pos,s) stmts'

statement (IfStmt pos e s1 s2)     = do
  scope <- get
  e' <- expression e
  s1' <- statement s1
  case s2 of
    Nothing -> return $ IfStmt (pos, scope) e' s1' Nothing
    (Just s2') -> do
      s2'' <- statement s2'
      return $ IfStmt (pos, scope) e' s1' (Just s2'')
-- statement (WhileStmt _ e stmt)        = expression e >> statement stmt
-- statement (Goto _ _)                  = return ()
-- statement (Continue _)                 = return ()
statement (Break p)                    = do { s <- get; return $ Break (p,s)}
-- statement (Return _ Nothing)          = return ()
-- statement (Return _ (Just e))         = void $ expression e  -- TODO: Check return type
statement (ExpressionStmt Nothing)  = return $ ExpressionStmt Nothing
statement (ExpressionStmt (Just e)) = (ExpressionStmt . Just) <$> expression e


expression :: Expr SynPhase -> Analysis (Expr SemPhase)
expression (Constant p x) = do
  s <- get
  return (Constant (p, s, CInt) x)

expression (ExprIdent p n) = do
  s <- get
  mt <- lookupType n
  case mt of
    Nothing ->do
      tell [UndeclaredName p n]
      return $ ExprIdent (p,s, Bottom) n
    (Just t) -> do
      return $ ExprIdent (p,s, t) n

expression (Ternary p e1 e2 e3) = do
  s <- get
  e1' <- expression e1
  e2' <- expression e2
  e3' <- expression e3
  t <- matchTypes p (getType e1') CInt -- TODO: No bool type?
  _ <- matchTypes p (getType e2') (getType e3')
  return (Ternary (p, s, t) e1' e2' e3')

expression (List es) = do
  children <- forM es expression
  return (List children)

expression (BExpr p bop e1 e2) = do
    s <- get
    e1' <- expression e1
    e2' <- expression e2
    if bop `elem` [Plus, Minus, Mult, LessThan, LAnd, LOr]
      then do
        _ <- matchTypes p (getType e1') CInt
        _ <- matchTypes p (getType e2') CInt
        return $ BExpr (p, s, CInt) bop e1' e2'
      else if bop `elem` [NotEqual, EqualsEquals]
           then do
              _ <- matchTypes p (getType e1') (getType e2')
              return $ BExpr (p, s, CInt) bop e1' e2' -- should be Bool
      else return $ BExpr (p, s, Bottom) bop e1' e2' -- should not happen

expression (UExpr p op e) = error "unary expression not implemented"
expression (SizeOfType p t) = error "sizeof type not implemented"
expression (Func p _ _) = error "func type not implemented"
expression (PointerAccess p _ _ ) = error "pointer access not implemented"
expression (Array p _ _ ) = error "array access not implemented"
expression (FieldAccess p _ _ ) = error "field access not implemented"
expression (StringLiteral p _ ) = error "string lit"

-- expression (ExprIdent n) = do
--   mt <- lookupType n
--   case mt of
--     Nothing -> do
--       tell [UndeclaredName n]
--       return Bottom
--     (Just t) -> return t

-- expression (Assign e1 e2) = do
--   t1 <- expression e1
--   t2 <- expression e2
--   matchTypes t1 t2



it :: Int -> (a -> a) -> a -> a
it 0 = id
it n =  \f -> it (n-1) f . f
