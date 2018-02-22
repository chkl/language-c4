{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE MultiWayIf       #-}
{-# LANGUAGE TypeFamilies     #-}

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
  { scope               :: Scope -- ^ declared and defined variables and functions
  , labels              :: Map.Map Ident SourcePos -- ^ defined labels
  , usedLabels          :: Map.Map Ident SourcePos -- ^ used labels, think of this as a queue of labels to be checked
  , structures          :: Map.Map Ident (Map.Map Ident CType)
  , expectedReturnValue :: Maybe CType
  , inLoop              :: Bool
  , inFunDef            :: Bool
  } deriving (Show)

emptyAnalysisState :: AnalysisState
emptyAnalysisState = AnalysisState Map.empty Map.empty Map.empty Map.empty Nothing False False

type Analysis m = StateT AnalysisState (C4T m)

analyse :: (Monad m) => TranslationUnit SynPhase -> C4T m (TranslationUnit SemPhase)
analyse u = evalStateT (translationUnit u) emptyAnalysisState

-- | Adds name to the scope without checking for existing declarations.
-- This is useful shadowing declarations (e.g. function parameters or loop
-- variables). For actual declarations use @declare which will emit an error for
-- declaring a already declared variable.
addName :: (Monad m) => Ident -> CType -> DStatus -> Analysis m ()
addName n t d = modify $ \s -> s {scope = Map.insert n (t,d) (scope s)}

-- | Looks up the type for a given name in the current scope
--lookupType :: (Monad m) => Ident -> Analysis m (Maybe CType)
--lookupType n = fst <$> lookupName n

lookupName :: (Monad m) => Ident -> Analysis m (Maybe (CType, DStatus))
lookupName n = do
  m <- gets scope
  return $ Map.lookup n m


lookupType :: (Monad m) => Ident -> Analysis m (Maybe CType)
lookupType n = (fmap .fmap) fst  (lookupName n)

-- | Adds a declaration to the current scope but writes an error in case the
-- name is already declared with a different type. Note that it is legal to
-- declare a name multiple times as long as they have the same declared type.
declare :: (Monad m) => SourcePos -> Ident -> CType -> Analysis m ()
declare pos n t = do
  ifd <- gets inFunDef
  lookupName n >>= \case
    Nothing  -> addName n t Declared
    Just (_, Shadowable) -> addName n t Declared
    Just (_, Defined)    -> throwC4 $ RedeclaredSymbol pos n
    Just (t',Declared) | ifd       -> throwC4 $ RedeclaredSymbol pos n
                       | t == t'   -> return ()
                       | otherwise -> throwC4 $ RedeclaredSymbolDifferentType pos n t t'

-- | 'define' is similar to 'declare'. The difference is that a name can only be defined once. This function also checks that if the name has been declared, that the types correspond.
define :: (Monad m) => SourcePos -> Ident -> CType -> Analysis m ()
define pos n t = do
  lookupName n >>= \case
    Nothing -> addName n t Defined
    (Just (_, Defined)) -> throwC4 $ AlreadyDefinedName pos n
    (Just (t', Declared)) -> do
      matchTypes_ pos t t'
      addName n t' Defined
    (Just (t', Shadowable)) -> addName n t' Defined

defineLabel :: Monad m => Ident -> SourcePos -> Analysis m ()
defineLabel n p = do
  hasLabel n >>= \case
    Nothing -> modify $ \s -> s { labels = Map.insert n p (labels s) }
    Just p' -> throwC4 $ DuplicateLabel p n p'

hasLabel :: Monad m => Ident -> Analysis m (Maybe SourcePos)
hasLabel n = Map.lookup n <$> gets labels

data SemanticError = UndeclaredName
                     { semanticErrorPosition :: !SourcePos
                     , identifier            :: !Ident }
                   | AlreadyDefinedName
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
                   | BreakOutsideLoop
                     { semanticErrorPosition :: !SourcePos }
                   | ContinueOutsideLoop
                     { semanticErrorPosition :: !SourcePos }
                   | UndefinedLabel
                     { semanticErrorPosition :: !SourcePos
                     , label                 :: Ident
                     }
                   | DuplicateLabel
                     { semanticErrorPosition :: !SourcePos
                     , label                 :: !Ident
                     , prevPosition          :: !SourcePos
                     }
                   | RedeclaredSymbol
                     { semanticErrorPosition :: !SourcePos
                     , ident                 :: !Ident
                     }
                   | DuplicateStruct
                   { semanticErrorPosition :: !SourcePos
                   , ident                 :: !Ident
                   }
                   | DuplicateField
                   { semanticErrorPosition :: !SourcePos
                   , ident                 :: !Ident
                   }
                   | UndefinedStruct
                   { semanticErrorPosition :: !SourcePos
                   , ident                 :: !Ident
                   }
                   | UndefinedField
                   { semanticErrorPosition :: !SourcePos
                   , ident                 :: !Ident
                   }
                   | AnonymousStructsDontMatch
                   { semanticErrorPosition :: !SourcePos
                   }
                   | RedeclaredSymbolDifferentType
                   { semanticErrorPosition :: !SourcePos
                   , ident                 :: !Ident
                   , typeError             :: !CType
                   , typePrevious          :: !CType }


instance C4Error SemanticError where
  getErrorPosition                           = semanticErrorPosition
  getErrorComponent (UndeclaredName _ i)     = "undeclared name " <> show i
  getErrorComponent (AlreadyDefinedName _ i) = "name already defined " <> show i
  getErrorComponent (TypeMismatch _ l r)     = "type mismatch: expected type: " <> show l <> "; actual type: " <> show r
  getErrorComponent (NoPointer _)            = "expects a pointer"
  getErrorComponent (UnexpectedReturn _)     = "unexpected return"
  getErrorComponent (AssignRValue _ _)       = "assignment to rvalue"
  getErrorComponent (BreakOutsideLoop _)     = "break can only be used inside a loop"
  getErrorComponent (ContinueOutsideLoop _)  = "continue can only be used inside a loop"
  getErrorComponent (MiscSemanticError _ m)  = m
  getErrorComponent (UndefinedLabel _ l)     = "undefined label " <> show l
  getErrorComponent (DuplicateLabel _ l p')  = "duplicate label " <> show l <> ", previously defined at " <> show (prettyPrintPos p')
  getErrorComponent (DuplicateStruct _ n)    = "duplicate struct " <> show n
  getErrorComponent (RedeclaredSymbol _ n)   = "re-declared symbol " <> show n
  getErrorComponent (DuplicateField _ n)     = "duplicate field " <> show n
  getErrorComponent (UndefinedStruct _ n)    = "undefined struct " <> show n
  getErrorComponent (UndefinedField _ n)     = "undefined field " <> show n
  getErrorComponent (AnonymousStructsDontMatch _)     = "anonymous structs cannot be matched"
  getErrorComponent (RedeclaredSymbolDifferentType _ n t t') = "redeclared symbol " <> show n <> " with type " <> show t <> ", previous type " <> show t'

-- | runs an analysis in a copy of the current scope without modifying the
-- original scope, but retains the errors and defined and used labels.
enterScope :: (Monad m) => Analysis m a -> Analysis m a
enterScope a = do
    oldState <- get
    put $ oldState { scope = Map.fromList $ map (\(n,(t,_)) -> (n,(t, Shadowable))) (Map.toList $ scope oldState)}
    x <- a
    modify $ \s -> s { scope               = scope oldState
                     , inLoop              = inLoop oldState
                     , expectedReturnValue = expectedReturnValue oldState
                     , inFunDef            = inFunDef oldState
                     }
    return x

-- | Matches two types and returns the unified (more general) type. Currently: "Matching"  means equality
matchTypes :: (Monad m) => SourcePos -- ^ the position displayed in case of a type mismatch
  -> CType -- ^ the expected type
  -> CType -- ^ the actual type
  -> Analysis m CType -- ^ a unified type
matchTypes _ Bottom _           = return Bottom
matchTypes _ _ Bottom           = return Bottom
matchTypes p (Pointer a) (Pointer b) = matchTypes p a b
matchTypes p t@(Pointer _) CInt = return t -- pointer arithmetic
matchTypes p CInt t@(Pointer _) = return t -- 
matchTypes p (Tuple [t1]) t2    = matchTypes p t1 t2
matchTypes p t1 (Tuple [t2])    = matchTypes p t1 t2
matchTypes p AnonymousStruct AnonymousStruct = throwC4 $ AnonymousStructsDontMatch p
matchTypes p t1 t2              = do
  when (t1 /= t2) $ throwC4 (TypeMismatch p t1 t2)
  return t1

matchTypes_ :: (Monad m) => SourcePos -> CType -> CType -> Analysis m ()
matchTypes_ p t s = void (matchTypes p t s)

--------------------------------------------------------------------------------
-- Analyses
--------------------------------------------------------------------------------

-- analyses a translation unit. Also ensures that all used labels are defined
translationUnit :: (Monad m) => TranslationUnit SynPhase -> Analysis m (TranslationUnit SemPhase)
translationUnit (TranslationUnit _ eds) = do
  eds' <- forM eds (either' declaration functionDefinition)
  s <- gets scope
  return $ TranslationUnit s eds'

checkForUndefinedLabels :: Monad m => Analysis m ()
checkForUndefinedLabels = do
  uLbls <- gets usedLabels
  forM_ (Map.toList uLbls) $ \(u,p) -> do
    hasLabel u >>= \case
      Just _ -> return ()
      Nothing -> throwC4 $ UndefinedLabel p u


functionDefinition :: (Monad m) => FunctionDefinition SynPhase -> Analysis m (FunctionDefinition SemPhase)
functionDefinition (FunctionDefinition pos t d (CompoundStmt p stmts))  = do
    t' <- typeA t
    d' <- declarator (getType t') d
    define pos (getName d') (getType d')
    params <- findParams d'
    stmt' <- enterScope $ do
      modify $ \s -> s { inFunDef = True}
      expectReturnType (getType t')
      forM_ params ( \case
        (Parameter (pp, pt, n) _ _) -> define pp n pt
        (AbstractParameter _ _ _ ) -> return () -- we don't have to declare abstract params
        )
      stmts' <- forM stmts $ either (fmap Left . declaration) (fmap Right . statement)
      return $ CompoundStmt p stmts'

    checkForUndefinedLabels -- check labels under 'enterScope'
    modify $ \s -> s { usedLabels = Map.empty, labels = Map.empty }
    return $ FunctionDefinition pos t' d' stmt'

functionDefinition (FunctionDefinition pos t d _)  = throwC4 $ MiscSemanticError pos "a function definition needs a compound statement"

expectReturnType :: Monad m => CType -> Analysis m ()
expectReturnType t = modify $ \s -> s { expectedReturnValue = Just t }

setInLoop :: Monad m => Analysis m ()
setInLoop = modify $ \s -> s { inLoop = True}



structDeclaration :: (Monad m) => StructDeclaration SynPhase -> Analysis m (StructDeclaration SemPhase)
structDeclaration (StructDeclaration p t l) = do
  t' <- typeA t
  l' <- mapM (declarator (getType t')) l
  return (StructDeclaration p t' l')

declarator :: (Monad m) => CType -> Declarator SynPhase -> Analysis m (Declarator SemPhase)
declarator t (DeclaratorId p n) = return (DeclaratorId (DeclaratorSemAnn p t n) n)

declarator t (IndirectDeclarator p d) = do
  d' <- declarator (Pointer t) d
  return (IndirectDeclarator (DeclaratorSemAnn p (getType d') (getName d')) d')

declarator t (FunctionDeclarator p d params) = do
  d' <- declarator t d
  params' <- mapM parameter params
  let paramsType = map getType params'
  return $ FunctionDeclarator (DeclaratorSemAnn p (Function t paramsType) (getName d')) d' params'

declarator t (ArrayDeclarator p d e) = do
  d' <- declarator t d
  e' <- expression e
  return $ ArrayDeclarator (DeclaratorSemAnn p (Array (getType d')) (getName d')) d' e'

parameter :: (Monad m) => Parameter SynPhase -> Analysis m (Parameter SemPhase)
parameter (Parameter p t d) = do
  t' <- typeA t
  d' <- declarator (getType t') d
  return $ Parameter (p, getType d', getName d') t' d'

parameter (AbstractParameter p t d) = do
  t' <- typeA t
  d' <- maybe' (abstractDeclarator (getType t')) d
  let typ = case d' of
           Nothing    -> getType t'
           (Just d'') -> getType d''
  return $ AbstractParameter (p, typ) t' d'

declaration :: (Monad m) => Declaration SynPhase -> Analysis m (Declaration SemPhase)
declaration (Declaration pos t is) = do
  t' <- typeA t
  is' <- forM is $ \i -> do
     i'@(InitializedDec d _) <- initDeclarator (getType t') i
     declare pos (getName d) (getType d)
     return i'
  return $ Declaration pos t' is'


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

abstractDeclarator :: Monad m => CType -> AbstractDeclarator SynPhase -> Analysis m (AbstractDeclarator SemPhase)
abstractDeclarator t (AbstractTerminal p) = return (AbstractTerminal (p,t))

abstractDeclarator t (IndirectAbstractDeclarator p d) = do
  d' <- abstractDeclarator (Pointer t) d
  return (IndirectAbstractDeclarator (p, getType d')  d')

abstractDeclarator t (AbstractFunctionDeclarator p d params) = do
  d' <- abstractDeclarator t d
  params' <- mapM parameter params
  let t' = Tuple $ map getType params'
  return $ AbstractFunctionDeclarator (p, t') d' params'

statement :: (Monad m) => Stmt SynPhase -> Analysis m (Stmt SemPhase)
statement (LabeledStmt p lbl stmt)      = do
  defineLabel lbl p
  stmt' <- statement stmt
  return $ LabeledStmt p lbl stmt'

statement (CompoundStmt pos xs) = do
  stmts' <- enterScope $ forM xs (either (fmap Left . declaration) (fmap Right . statement))
  return $ CompoundStmt pos stmts'

statement (Goto p lbl) = do
  modify $ \s -> s { usedLabels = Map.insert lbl p (usedLabels s)}
  return $ Goto p lbl


statement (WhileStmt p c s) = do
  c' <- expression c
  s' <- enterScope $ do
    setInLoop
    statement s
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
statement (Break p)                    = do
  x <- gets inLoop
  unless x $ throwC4 $ BreakOutsideLoop p
  return $ Break p

statement (Continue p)  = do
  x <- gets inLoop
  unless x $ throwC4 $ ContinueOutsideLoop p
  return (Continue p)

statement (ExpressionStmt p e) = do
  e' <- expression e
  return $ ExpressionStmt p e'


expression :: (Monad m) => Expr SynPhase -> Analysis m (Expr SemPhase)
expression (IntConstant p x) = do
  return (IntConstant (p, CInt, RValue) x)

expression (CharConstant p x) = do
  return (CharConstant (p, CInt, RValue) x)

expression (Assign p l r) = do
  l' <- expression l
  r' <- expression r
  t <- matchTypes p (getType l') (getType r')
  case getLValuedness l' of
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
    if
      | bop `elem` [Plus, Minus, Mult, LessThan, LAnd, LOr]  -> do
        _ <- matchTypes p (getType e1') CInt
        _ <- matchTypes p (getType e2') CInt
        return $ BExpr (p, CInt, RValue) bop e1' e2'

      | bop `elem` [NotEqual, EqualsEquals] -> do
              _ <- matchTypes p (getType e1') (getType e2')
              return $ BExpr (p, CInt, RValue) bop e1' e2' -- should be Bool
      | bop == AssignOp   -> do
              t <- matchTypes p (getType e1') (getType e2')
              case getLValuedness e1' of
                LValue -> return (BExpr (p, t, RValue) AssignOp e1' e2')
                RValue -> throwC4 $ AssignRValue p (getType e1')

      | otherwise -> return $ BExpr (p, Bottom, RValue) bop e1' e2' -- should not happen

expression (UExpr p op e) = do
  e' <- expression e
  let t' = getType e'
  if
    | op `elem` [Neg, Not] -> do
        t <- matchTypes p t' CInt
        return $ UExpr (p, t, RValue) op e'

    | op == Deref -> do
        t <- case t' of
               (Pointer t0) -> return t0
               _            -> throwC4 $ NoPointer p
        return $ UExpr (p, t, LValue) op e' -- TODO: Deref always results in an lvalue?

    | op == SizeOf -> return $ UExpr (p, CInt, RValue) op e' -- TODO is that correct?

    | op == Address -> return $ UExpr (p, Pointer t', RValue) op e'

expression (SizeOfType p t)       = do
  t' <- typeA t
  return $ SizeOfType (p, getType t', RValue) t'

expression (Func p l r)           = do
  l' <- expression l
  r' <- expression r
  case getType l' of
    Function t tp -> do
      _ <- matchTypes p (Tuple tp) (getType r')
      return $ Func (p, t, RValue) l' r'
    _ -> do
      throwC4 $ TypeMismatch p (Function Bottom [getType r']) (getType l')

expression (PointerAccess p l f ) = do
  l' <- expression l
  ty <- case getType l' of
          (Pointer (Struct s)) -> do
            Map.lookup s <$> gets structures >>= \case
              Nothing -> throwC4 $ UndefinedStruct p s
              (Just s') -> case Map.lookup f s' of
                                Nothing  -> throwC4 $ UndefinedField p f
                                (Just t) -> return t
          (Pointer AnonymousStruct) -> return Bottom -- TODO
          _ -> throwC4 $ TypeMismatch p (getType l') (Pointer Bottom)
  return $ PointerAccess (p, ty , getLValuedness l') l' f

expression (ArrayAccess p l r)         = do
  l' <- expression l
  r' <- expression r
  case getType l' of
    Pointer t -> do
      matchTypes_ p CInt (getType r')
      return (ArrayAccess (p, t, getLValuedness l') l' r')
    Array t -> do
      matchTypes_ p CInt (getType r')
      return (ArrayAccess (p, t, getLValuedness l') l' r')
    _ -> do
      throwC4 $ TypeMismatch p (Pointer Bottom) (getType l')

expression (FieldAccess p l f)   = do
  l' <- expression l
  ty <- case getType l' of
          (Struct s) -> do
            strcts <- gets structures
            case Map.lookup s strcts of
                Nothing -> throwC4 $ UndefinedStruct p s
                (Just s') -> case Map.lookup f s' of
                                  Nothing  -> throwC4 $ UndefinedField p f
                                  (Just t) -> return t
          _ -> throwC4 $ TypeMismatch p (getType l') AnonymousStruct
  return (FieldAccess (p, ty, LValue) l' f) -- TODO: type check field access

expression (StringLiteral p b)   = return $ StringLiteral (p, Pointer CChar, RValue) b


typeA :: (Monad m) => Type SynPhase -> Analysis m (Type SemPhase)
typeA (Void p) = return $ Void (p, CVoid)
typeA (Int  p) = return $ Int (p, CInt)
typeA (Char p) = return $ Char (p, CChar)

-- | Apparently struct do not need to be defined in order to be used in declarations
typeA (StructIdentifier p i) = return $ StructIdentifier (p, Struct i) i

typeA (StructInline p mi l) = do
  l' <- mapM structDeclaration l
  case mi of
    Nothing -> return $ StructInline (p, AnonymousStruct) mi l'
    Just i -> do
      fields <- execStateT (
        forM_ l' $ \(StructDeclaration  p' _ ds) ->
              forM_ ds $ \d -> do
                  let n = getName d
                  Map.lookup n <$> get >>= \case
                    Just _ -> throwC4 $ DuplicateField p' n
                    Nothing -> modify $ Map.insert n (getType d)
        ) Map.empty
      Map.lookup i <$> gets structures >>= \case
        Nothing -> do
          modify $ \s -> s {structures = Map.insert i fields (structures s) }
          return $ StructInline (p, Struct i) mi l'
        Just _ -> throwC4 $ DuplicateStruct p i


--------------------------------------------------------------------------------
-- little helpers
--------------------------------------------------------------------------------


maybe' :: (Monad m) => (a -> Analysis m b) -> Maybe a -> Analysis m (Maybe b)
maybe' _ Nothing  = return Nothing
maybe' f (Just x) = Just <$> f x

either' :: (Functor f) => (a -> f c) -> (b -> f d) -> Either a b -> f (Either c d)
either' f g = either (fmap Left . f) (fmap Right . g)
