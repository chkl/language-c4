{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}

module Language.C4.Analysis
 ( SemanticError(..)
 , analyse
 ) where


import           Control.Monad          (forM, when)
import           Control.Monad.State
import           Control.Monad.Writer
import qualified Data.Map.Strict        as Map

import           Language.C4.Ast.SemAst
import           Language.C4.Ast.SynAst
import           Language.C4.Types

-- The functionality of the module can be summarized as converting from a
-- syntactic AST to a semantic one. In particular this means that e.g.
-- 'TranslationUnit SynPhase' gets transformed into a 'TranslationUnit
-- SemPhase'.


data AnalysisState = AnalysisState
  { scope               :: Scope
  , expectedReturnValue :: Maybe CType
  } deriving (Show)

type Analysis m = StateT AnalysisState (C4T m)

analyse :: (Monad m) => TranslationUnit SynPhase -> C4T m (TranslationUnit SemPhase)
analyse u = evalStateT (translationUnit u) (AnalysisState Map.empty Nothing)

-- | Adds name to the scope without checking for existing declarations.
-- This is useful shadowing declarations (e.g. function parameters or loop
-- variables). For actual declarations use @declare which will emit an error for
-- declaring a already declared variable.
addName :: (Monad m) => Ident -> CType -> Analysis m ()
addName n t = modify $ \s -> s {scope = Map.insert n t (scope s)}

-- | Looks up the type for a given name in the current scope
lookupType :: (Monad m) => Ident -> Analysis m (Maybe CType)
lookupType n = do
  m <- gets scope
  return $ Map.lookup n m

-- | adds a declaration to the current scope but writes an error in case the
-- name is already declared.

declare :: (Monad m) => SourcePos -> Ident -> CType -> Analysis m ()
declare pos n t = do
  x <- lookupType n
  case x of
    Nothing  -> addName n t
    (Just _) -> throwC4 $ AlreadyDeclaredName pos n


data SemanticError = UndeclaredName
                     { semanticErrorPosition :: !SourcePos
                     , identifier            :: !Ident }
                   | AlreadyDeclaredName
                     { semanticErrorPosition :: !SourcePos
                     ,  identifier           :: !Ident }
                   | TypeMismatch
                     { semanticErrorPosition :: !SourcePos
                     , leftType              :: !CType
                     , rightType             :: !CType }
                   | NoPointer
                     { semanticErrorPosition :: SourcePos
                     }
                   | UnexpectedReturn
                     { semanticErrorPosition :: !SourcePos
                     }
                   | AssignRValue
                     { semanticErrorPosition :: !SourcePos
                     , leftType              :: !CType
                     }
                   | MiscSemanticError
                     { semanticErrorPosition :: !SourcePos
                     , message               :: !String
                     }


instance C4Error SemanticError where
  getErrorPosition                            = semanticErrorPosition
  getErrorComponent (UndeclaredName _ i)      = "undeclared name " <> show i
  getErrorComponent (AlreadyDeclaredName _ i) = "name already declared " <> show i
  getErrorComponent (TypeMismatch _ l r)      = "type mismatch: expected type: " <> show l <> "; actual type: " <> show r
  getErrorComponent (NoPointer _)             = "expects a pointer"
  getErrorComponent (UnexpectedReturn _)      = "unexpected return"
  getErrorComponent (AssignRValue _ _)        = "assignment to rvalue"
  getErrorComponent (MiscSemanticError _ m)   = m

-- | runs an analysis in a copy of the current scope without modifying the
-- original scope, but retains the errors.
enterScope :: (Monad m) => Analysis m a -> Analysis m a
enterScope a = do
    s <- get
    x <- a
    put s
    return x

-- | Matches two types and returns the unified (more general) type. Currently: "Matching"  means equality
matchTypes :: (Monad m) => SourcePos -- ^ the position displayed in case of a type mismatch
  -> CType -- ^ the expected type
  -> CType -- ^ the actual type
  -> Analysis m CType -- ^ a unified type
matchTypes _ Bottom _           = return Bottom
matchTypes _ _ Bottom           = return Bottom
matchTypes p t@(Pointer _) CInt = return t -- ^ pointer arithmetic
matchTypes p CInt t@(Pointer _) = return t -- ^
matchTypes p (Tuple [t1]) t2    = matchTypes p t1 t2
matchTypes p t1 (Tuple [t2])    = matchTypes p t1 t2
matchTypes p t1 t2              = do
  when (t1 /= t2) $ throwC4 (TypeMismatch p t1 t2)
  return t1

matchTypes_ :: (Monad m) => SourcePos -> CType -> CType -> Analysis m ()
matchTypes_ p t s = void (matchTypes p t s)

--------------------------------------------------------------------------------
-- Analyses
--------------------------------------------------------------------------------


translationUnit :: (Monad m) => TranslationUnit SynPhase -> Analysis m (TranslationUnit SemPhase)
translationUnit (TranslationUnit _ eds) = do
  eds' <- forM eds (either' declaration functionDefinition)
  s <- gets scope
  return $ TranslationUnit s eds'

findParams :: Monad m =>  Declarator SemPhase -> Analysis m [Parameter SemPhase]
findParams (IndirectDeclarator _ d)  = findParams d
findParams (DeclaratorId x _ ) = throwC4 $ MiscSemanticError (_position x) "expected function declarator"
findParams (FunctionDeclarator _ _ ps) = return ps

functionDefinition :: (Monad m) => FunctionDefinition SynPhase -> Analysis m (FunctionDefinition SemPhase)
functionDefinition (FunctionDefinition pos t d stmt)  = do
    d' <- declarator (fromType t) d
    tx <- typeA t
    declare pos (getName d') (getType d')
    params <- findParams d'
    stmt' <- enterScope $ do
      expectReturnType (fromType t)
      forM_ params $ \(Parameter (p,t,n) _ _) -> declare p n t
      statement stmt
    return $ FunctionDefinition pos tx d' stmt'

expectReturnType :: Monad m => CType -> Analysis m ()
expectReturnType t = modify $ \s -> s { expectedReturnValue = Just t }

typeA :: (Monad m) => Type SynPhase -> Analysis m (Type SemPhase)
typeA Void = return Void
typeA Int  = return Int
typeA Char = return Char
typeA (StructIdentifier i) = return (StructIdentifier i)
typeA (StructInline i l) = do
  l' <- mapM structDeclaration l
  return (StructInline i l')


structDeclaration :: (Monad m) => StructDeclaration SynPhase -> Analysis m (StructDeclaration SemPhase)
structDeclaration (StructDeclaration p t l) = do
  t' <- typeA t
  l' <- mapM (declarator (fromType t')) l
  return (StructDeclaration p t' l')

declarator :: (Monad m) => CType -> Declarator SynPhase -> Analysis m (Declarator SemPhase)
declarator t (DeclaratorId p n) = return (DeclaratorId (DeclaratorSemAnn p t n) n)

declarator t (IndirectDeclarator p d) = do
  d' <- declarator (Pointer t) d
  let t' = Pointer (getType d')
  return (IndirectDeclarator (DeclaratorSemAnn p t' (getName d')) d')

declarator t (FunctionDeclarator p d params) = do
  d' <- declarator t d
  params' <- mapM parameter params
  let paramsType = map getType params'
  return $ FunctionDeclarator (DeclaratorSemAnn p (Function t paramsType) (getName d')) d' params'


parameter :: (Monad m) => Parameter SynPhase -> Analysis m (Parameter SemPhase)
parameter (Parameter p t d) = do
  t' <- typeA t
  d' <- declarator (fromType t) d
  return $ Parameter (p, getType d', getName d') t' d'
parameter (AbstractParameter p t d) = do
  t' <- typeA t
  d' <- maybe' (abstractDeclarator (fromType t)) d
  let typ = case d' of
              Nothing    -> fromType t
              (Just d'') -> getType d''
  return $ AbstractParameter (p, typ) t' d'

declaration :: (Monad m) => Declaration SynPhase -> Analysis m (Declaration SemPhase)
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


initDeclarator :: (Monad m) => CType -> InitDeclarator SynPhase -> Analysis m (InitDeclarator SemPhase)
initDeclarator t (InitializedDec d mi) = do
  d' <- declarator t d
  mi' <- case mi of
    Nothing  -> return Nothing
    (Just i) -> Just <$> initializer i
  return $ InitializedDec d' mi'

initializer :: (Monad m) => Initializer SynPhase -> Analysis m (Initializer SemPhase)
initializer (InitializerAssignment e) = do
  e' <- expression e
  return $ InitializerAssignment e'
initializer (InitializerList es) = do
  es' <- mapM initializer es
  return $ InitializerList es'

abstractDeclarator :: CType -> AbstractDeclarator SynPhase -> Analysis m (AbstractDeclarator SemPhase)
abstractDeclarator = undefined -- TODO: Basically as declarator but abstract


statement :: (Monad m) => Stmt SynPhase -> Analysis m (Stmt SemPhase)
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
  let t' = maybe CVoid getType e' -- an empty return statement defaults to return 0
  gets expectedReturnValue >>= \case
    Nothing -> throwC4 $ UnexpectedReturn p
    Just t -> matchTypes_ p t t'
  return (Return p e')

statement (IfStmt pos e s1 s2)     = do
  e' <- expression e
  s1' <- statement s1
  case s2 of
    Nothing -> return $ IfStmt pos e' s1' Nothing
    (Just s2') -> do
      s2'' <- statement s2'
      return $ IfStmt pos e' s1' (Just s2'')
statement (Break p)                    = return $ Break p

statement (ExpressionStmt p e) = do
  e' <- expression e
  return $ ExpressionStmt p e'


expression :: (Monad m) => Expr SynPhase -> Analysis m (Expr SemPhase)
expression (Constant p x) = do
  return (Constant (p, CInt, RValue) x)

expression (Assign p l r) = do
  l' <- expression l
  r' <- expression r
  t <- matchTypes p (getType l') (getType r')
  case (getLValuedness l') of
    LValue -> return (Assign (p, t, RValue) l' r')
    RValue -> throwC4 $ AssignRValue p (getType l')

expression (ExprIdent p n) = do
  mt <- lookupType n
  case mt of
    Nothing  -> throwC4 $ UndeclaredName p n
    (Just t) -> return $ ExprIdent (p, t, LValue) n --TODO: check LValue

expression (Ternary p e1 e2 e3) = do
  e1' <- expression e1
  e2' <- expression e2
  e3' <- expression e3
  t <- matchTypes p (getType e1') CInt -- TODO: No bool type?
  _ <- matchTypes p (getType e2') (getType e3')
  return (Ternary (p, t, RValue) e1' e2' e3')

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
        return $ BExpr (p, CInt, RValue) bop e1' e2'
      else if bop `elem` [NotEqual, EqualsEquals]
           then do
              _ <- matchTypes p (getType e1') (getType e2')
              return $ BExpr (p, CInt, RValue) bop e1' e2' -- should be Bool
      else return $ BExpr (p, Bottom, RValue) bop e1' e2' -- should not happen

expression (UExpr p op e) = do
  e' <- expression e
  let t' = getType e'
  t'' <- case op of
           Neg -> matchTypes p t' CInt
           Not -> matchTypes p t' CInt -- Bool
           Deref -> case t' of
             (Pointer t0) -> return t0
             _            -> throwC4 $ NoPointer p
           SizeOf -> return CInt -- TODO is that correct?
           Address -> return $ Pointer t'
  return $ UExpr (p, t'', RValue) op e'

expression (SizeOfType p t)       = do
  t' <- typeA t
  return (SizeOfType (p, fromType t, RValue) t')

expression (Func p l r)           = do
  l' <- expression l
  r' <- expression r
  case getType l' of
    Function t tp -> do
      _ <- matchTypes p (Tuple tp) (getType r')
      return $ Func (p, t, RValue) l' r'
    _ -> do
      throwC4 $ TypeMismatch p (Function Bottom [getType r']) (getType l')

expression (PointerAccess p l r ) = do
  l' <- expression l
  r' <- expression r
  return $ PointerAccess (p, Bottom, (getLValuedness l')) l' r'

expression (ArrayAccess p l r)         = do
  l' <- expression l
  r' <- expression r
  case getType l' of
    Pointer t -> do
      matchTypes_ p CInt (getType r')
      return (ArrayAccess (p, t, (getLValuedness l')) l' r')
    _ -> do
      throwC4 $ TypeMismatch p (Pointer Bottom) (getType l')

expression (FieldAccess p l r)   = do
  l' <- expression l
  r' <- expression r
  return (FieldAccess (p, Bottom, LValue) l' r') -- TODO: type check field access

expression (StringLiteral p b)   = return $ StringLiteral (p, Pointer CChar, RValue) b

--------------------------------------------------------------------------------
-- little helpers
--------------------------------------------------------------------------------

maybe' :: (Monad m) => (a -> Analysis m b) -> Maybe a -> Analysis m (Maybe b)
maybe' _ Nothing  = return Nothing
maybe' f (Just x) = Just <$> f x

either' :: (Functor f) => (a -> f c) -> (b -> f d) -> Either a b -> f (Either c d)
either' f g = either (fmap Left . f) (fmap Right . g)
