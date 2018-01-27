{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE EmptyCase         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Analysis
 ( SemanticError(..)
 , semanticAnalysis
 )
where

import           Control.Monad        (forM, when)
import           Control.Monad.State
import           Control.Monad.Writer
import qualified Data.Map.Strict      as Map
import           Text.Megaparsec.Pos  (SourcePos)

import           Ast.SemAst
import           Ast.SynAst
import           Types

-- The functionality of the module can be summarized as converting from a
-- semantic AST to a semantic one. In particular this means that e.g.
-- 'TranslationUnit SynPhase' gets transformed into a 'TranslationUnit
-- SemPhase'.

-- | Any analysis that runs inside the @Analysis monad can write @SemanticErrors
-- and has a "scope" which acts as state.
type Analysis = WriterT [SemanticError] (State Scope)

runAnalyzer :: Analysis a -> Scope ->(a, [SemanticError], Scope)
runAnalyzer a s = let ((b,w),s') = runState (runWriterT a) s
                  in (b,w,s')

-- | Adds name to the scope without checking for existing declarations.
-- This is useful shadowing declarations (e.g. function parameters or loop
-- variables). For actual declarations use @declare which will emit an error for
-- declaring a already declared variable.
addName :: Ident -> CType -> Analysis ()
addName n t = modify $ \(Scope m) -> (Scope (Map.insert n t m))

-- | Looks up the type for a given name in the current scope
lookupType :: Ident -> Analysis (Maybe CType)
lookupType n = do
  (Scope m) <- get
  return $ Map.lookup n m

-- | adds a declaration to the current scope but writes an error in case the
-- name is already declared.

declare :: SourcePos -> Ident -> CType -> Analysis ()
declare pos n t = do
  x <- lookupType n
  case x of
    Nothing  -> addName n t
    (Just _) -> tell [AlreadyDeclaredName pos n]


data SemanticError = UndeclaredName !SourcePos !Ident
                   | AlreadyDeclaredName !SourcePos !Ident
                   | TypeMismatch {_pos :: !SourcePos, _leftType :: !CType, _rightType :: !CType }
                   | NoPointer SourcePos
               deriving (Show)


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

semanticAnalysis :: TranslationUnit SynPhase -> Either [SemanticError] (TranslationUnit SemPhase)
semanticAnalysis u = case runState (runWriterT (translationUnit u)) emptyScope of
        ((x,[]),_)  -> Right x
        ((_,err),_) -> Left err

--------------------------------------------------------------------------------
-- Analyses
--------------------------------------------------------------------------------


translationUnit :: TranslationUnit SynPhase -> Analysis (TranslationUnit SemPhase)
translationUnit (TranslationUnit _ eds) = do
  eds' <- forM eds (either' declaration functionDefinition)
  s <- get
  return $ TranslationUnit s eds'

functionDefinition :: FunctionDefinition SynPhase -> Analysis (FunctionDefinition SemPhase)
functionDefinition (FunctionDefinition pos t d stmt)  = do
    d' <- declarator (fromType t) d
    tx <- typeA t
    declare pos (getName d') (getType d')
    stmt' <- enterScope $ do
      statement stmt
    return $ FunctionDefinition pos tx d' stmt'

typeA :: Type SynPhase -> Analysis (Type SemPhase)
typeA Void = return Void
typeA Int  = return Int
typeA Char = return Char

declarator :: CType -> Declarator SynPhase -> Analysis (Declarator SemPhase)
declarator t (DeclaratorId p n) = return (DeclaratorId (DeclaratorSemAnn p t n) n)

declarator t (IndirectDeclarator p n d) = declarator (it n Pointer t) d

declarator t (FunctionDeclarator p d params) = do
  d' <- declarator t d
  params' <- mapM parameter params
  let paramsType = map getType params'
  return $ FunctionDeclarator (DeclaratorSemAnn p (Function t paramsType) (getName d')) d' params'

parameter :: Parameter SynPhase -> Analysis (Parameter SemPhase)
parameter (Parameter p t d) = do
  t' <- typeA t
  d' <- declarator (fromType t) d
  return $ Parameter (p, getType d') t' d'

declaration :: Declaration SynPhase -> Analysis (Declaration SemPhase)
declaration (Declaration pos t is) = do
  is' <- forM is $ \i -> do
     i'@(InitializedDec d _) <- initDeclarator (fromType t) i
     declare pos (getName d) (getType d)
     return i'
  t' <- typeA t
  return $ Declaration pos t' is'

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


statement :: Stmt SynPhase -> Analysis (Stmt SemPhase)
statement (LabeledStmt p lbl stmt)      = do
  stmt' <- statement stmt
  return $ LabeledStmt p lbl stmt'

statement (CompoundStmt pos xs) = do
  stmts' <- enterScope $ forM xs (either (fmap Left . declaration) (fmap Right . statement))
  return $ CompoundStmt pos stmts'

statement (Goto p l) = return (Goto p l)

statement (Continue p)  = return (Continue p)

statement (WhileStmt p c s) = do
  c' <- expression c
  s' <- statement s
  return $ WhileStmt p c' s'

statement (Return p e) = do
  e' <- maybe' expression e
  return (Return p e')

statement (IfStmt pos e s1 s2)     = do
  e' <- expression e
  s1' <- statement s1
  case s2 of
    Nothing -> return $ IfStmt pos e' s1' Nothing
    (Just s2') -> do
      s2'' <- statement s2'
      return $ IfStmt pos e' s1' (Just s2'')
-- statement (WhileStmt _ e stmt)        = expression e >> statement stmt
-- statement (Goto _ _)                  = return ()
-- statement (Continue _)                 = return ()
statement (Break p)                    = return $ Break p
-- statement (Return _ Nothing)          = return ()
-- statement (Return _ (Just e))         = void $ expression e  -- TODO: Check return type
statement (ExpressionStmt p Nothing)  = do
  return $ ExpressionStmt p Nothing

statement (ExpressionStmt p (Just e)) = do
  e' <- expression e
  return $ ExpressionStmt p (Just e')


expression :: Expr SynPhase -> Analysis (Expr SemPhase)
expression (Constant p x) = do
  return (Constant (p, CInt) x)

expression (Assign p l r) = error "assignment is not implemented yet"

expression (ExprIdent p n) = do
  mt <- lookupType n
  case mt of
    Nothing ->do
      tell [UndeclaredName p n]
      return $ ExprIdent (p, Bottom) n
    (Just t) -> do
      return $ ExprIdent (p, t) n

expression (Ternary p e1 e2 e3) = do
  e1' <- expression e1
  e2' <- expression e2
  e3' <- expression e3
  t <- matchTypes p (getType e1') CInt -- TODO: No bool type?
  _ <- matchTypes p (getType e2') (getType e3')
  return (Ternary (p, t) e1' e2' e3')

expression (List es) = do
  children <- forM es expression
  return (List children)

expression (BExpr p bop e1 e2) = do
    e1' <- expression e1
    e2' <- expression e2
    if bop `elem` [Plus, Minus, Mult, LessThan, LAnd, LOr]
      then do
        _ <- matchTypes p (getType e1') CInt
        _ <- matchTypes p (getType e2') CInt
        return $ BExpr (p, CInt) bop e1' e2'
      else if bop `elem` [NotEqual, EqualsEquals]
           then do
              _ <- matchTypes p (getType e1') (getType e2')
              return $ BExpr (p, CInt) bop e1' e2' -- should be Bool
      else return $ BExpr (p, Bottom) bop e1' e2' -- should not happen

expression (UExpr p op e) = do
  e' <- expression e
  let t' = getType e'
  t'' <- case op of
           Neg -> matchTypes p t' CInt
           Not -> matchTypes p t' CInt -- Bool
           Deref -> case t' of
             (Pointer t0) -> return t0
             _            -> do  {tell [NoPointer p]; return Bottom}
           SizeOf -> return CInt -- TODO is that correct?
           Address -> return (Pointer t')
  return $ UExpr (p, t'') op e'

expression (SizeOfType p t)       = error "sizeof type not implemented"
expression (Func p _ _)           = error "func type not implemented"
expression (PointerAccess p _ _ ) = error "pointer access not implemented"
expression (Array p _ _ )         = error "array access not implemented"
expression (FieldAccess p _ _ )   = error "field access not implemented"
expression (StringLiteral p _ )   = error "string lit"

--------------------------------------------------------------------------------
-- little helpers
--------------------------------------------------------------------------------

it :: Int -> (a -> a) -> a -> a
it 0 = id
it n =  \f -> it (n-1) f . f

maybe' :: (a -> Analysis b) -> Maybe a -> Analysis (Maybe b)
maybe' _ Nothing  = return Nothing
maybe' f (Just x) = Just <$> f x

either' :: (Functor f) => (a -> f c) -> (b -> f d) -> Either a b -> f (Either c d)
either' f g = either (fmap Left . f) (fmap Right . g)
