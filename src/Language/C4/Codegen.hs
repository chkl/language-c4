{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PartialTypeSignatures      #-}
{-# LANGUAGE RecursiveDo                #-}
{-# LANGUAGE TypeFamilies               #-}


module Language.C4.Codegen
  ( compile
  , ppllvm
  ) where

import           Control.Monad.Error.Class
import           Control.Monad.State
import qualified Data.Map                   as Map
import           Data.Word                  (Word32)
import           Debug.Trace

import           LLVM.AST                   hiding (alignment, function)
import           LLVM.AST.AddrSpace
import           LLVM.AST.Global
import           LLVM.AST.Constant
import           LLVM.AST.IntegerPredicate  as Pred
import           LLVM.AST.Type
import           LLVM.IRBuilder.Constant
import           LLVM.IRBuilder.Instruction
import           LLVM.IRBuilder.Module
import           LLVM.IRBuilder.Monad
import           LLVM.Pretty                (ppllvm)
import           Prelude                    hiding (EQ, and, or)

import           Language.C4.Ast.SemAst     as SemAst
import           Language.C4.Types

-- | We use the builder monads provided by llvm-hs in combination to our C4 Monad so we can still throw errors and have access to a future log / configuration.

data CGState = CGState
  { _labels :: Map.Map Ident Name
  , _scope  ::  Map.Map Ident Operand
  }


emptyCGState :: CGState
emptyCGState = CGState Map.empty Map.empty

newtype CGModule a = CGModule { unCGModule :: ModuleBuilderT (StateT CGState C4) a }
  deriving (Functor, Applicative, Monad, MonadFix, MonadError (SourcePos, String), MonadModuleBuilder, MonadState CGState)


runCGModule :: CGModule a -> C4 Module
runCGModule m = fst <$> runStateT (buildModuleT "blah" $ unCGModule m) emptyCGState


addToScope :: (MonadState CGState m) => Ident -> Operand -> m ()
addToScope i n = trace ("adding " ++ show i ++ " to scope") $ modify $ \s -> s {_scope = Map.insert i n (_scope s)}

lookupOperand :: Ident -> CGBlock (Maybe Operand)
lookupOperand i = Map.lookup i <$> gets _scope

type CGBlock  = IRBuilderT CGModule

data CodegenError = FeatureNotImplemented { codegenErrorPosition :: ! SourcePos
                                          , featureName          :: String }
                  | UnknownLabel { codegenErrorPosition :: !SourcePos
                                 , labelName            :: String }

instance C4Error CodegenError where
  getErrorPosition = codegenErrorPosition
  getErrorComponent (FeatureNotImplemented _ fn ) = "feature not implemented: " ++ fn
  getErrorComponent (UnknownLabel _ l) = "label unknown : " ++ l ++ " (this should have been detected during semantic analysis (TODO!))"

compile :: TranslationUnit SemPhase -> C4 LLVM.AST.Module
compile (TranslationUnit _ es) = runCGModule $
  mapM_ (either externalDeclaration functionDefinition) es



externalDeclaration :: Declaration SemPhase -> CGModule ()
externalDeclaration (Declaration p t ids) = forM_ ids $ \(InitializedDec d mi)  -> do
  case getType d of
    (SemAst.Function b as) -> do
      let name = Name $ getName d
      let typedParams = map toLLVMType  as
          returnType = toLLVMType  b
      x <- extern name typedParams returnType
      addToScope (getName d) x
      return ()
    t -> do
      let nm   = Name $ getName d
          glbl = globalVariableDefaults { name = nm, LLVM.AST.Global.type' = toLLVMType t}
      emitDefn (GlobalDefinition glbl)




functionDefinition :: FunctionDefinition SemPhase -> CGModule ()
functionDefinition (FunctionDefinition _ _ d stmt) = Control.Monad.State.void $ mdo
  params <- findParams d
  let paramList = map (\p -> (toLLVMType $ getType p, ParameterName (getName p))) params
      funName                      = Name (getName d)
      (SemAst.Function returnTy _) = getType d

  fn <- function funName paramList (toLLVMType returnTy) $ \paramsLLVM -> mdo
    _ <- block `named` "entry" -- Do not remove this
    forM_ (zip params paramsLLVM)  $ \(param, operand) -> mdo
        ll <- alloca (toLLVMType (getType param)) Nothing (getAlignment param) 
        store ll (getAlignment param) operand
        addToScope (getName param) ll
    statement stmt
    addDefaultReturn returnTy
  return fn

addDefaultReturn CVoid = retVoid
addDefaultReturn CInt = do
  zero <- int32 0
  modifyBlock $ \b -> case partialBlockTerm b of
    Just _ -> b
    Nothing -> b { partialBlockTerm = Just (Do (Ret {returnOperand = Just zero, metadata' = []})) }



-- addDefaultReturnInt = modifyBlock $ \b -> trace (show (partialBlockTerm b)) b


-- | Only for "internal" declarations
declaration :: Declaration SemPhase -> CGBlock ()
declaration (Declaration _ _ ids ) = forM_ ids $ \(InitializedDec d mi) -> do
  let t   = getType d
      n   = getName d
      t'  = toLLVMType t
      alg = getAlignment d
  llvmName <- alloca t' Nothing alg
  case mi of
    Just (InitializerAssignment e) -> do
      e' <- expression R e
      store llvmName alg e'
    Nothing -> return ()
  addToScope n llvmName
  return ()


-- declaration (Declaration _ Int [InitializedDec (DeclaratorId _ n) Nothing]) = do
--   llvmName <- alloca i32 Nothing 4
--   addToScope n llvmName
--   return ()

statement :: Stmt SemPhase -> CGBlock ()
statement (Return _ Nothing) = retVoid
statement (Return _ (Just e)) = do
  e' <- expression R e
  ret e'
statement (CompoundStmt _ stmtsOrDecls) = mapM_ f stmtsOrDecls
  where f = either declaration statement

statement (ExpressionStmt _ e) = Control.Monad.State.void $ expression R e

statement (IfStmt _ c s1 ms2) = mdo
    c' <- expression R c
    zero <- int32 0
    c'' <- icmp NE c' zero
    condBr c'' thB elB
    thB <- block `named` "then"
    statement s1
    br fi
    elB <- block `named` "else"
    case ms2 of
      Nothing -> return ()
      Just s2 -> do
        statement s2
        br fi
    fi <- block `named` "fi"
    return ()

statement (WhileStmt _ c s) = mdo
  br hd
  hd <- block `named` "head"
  e' <- expression R c
  zero <- int32 0
  e'' <- icmp NE e' zero
  condBr e'' bd tl
  bd <- block `named` "body"
  statement s
  br hd
  tl <- block `named` "end"
  return ()

statement (LabeledStmt _ l s) = mdo
  b <- block `named` l
  addLabel l b
  statement s

statement (Goto p l) = mdo
  n <- lookupLabel p l
  br n

addLabel :: Ident -> Name -> CGBlock ()
addLabel l b = do
  s <- get
  let s' = s {_labels = Map.insert l b (_labels s)} -- TODO: Use lenses?
  put s'


lookupLabel :: SourcePos -> Ident -> CGBlock Name
lookupLabel p l = do
  st <- get
  case Map.lookup l (_labels st) of
    Nothing -> throwC4 $ UnknownLabel p (show l)
    Just n  -> return n



data LR = L | R

expression :: LR -> Expr SemPhase -> CGBlock Operand

expression lr e@(ExprIdent _ i) = do
  lookupOperand i >>= \case
    Just n -> case lr of
                  L -> return n
                  R -> load n (getAlignment e)
    Nothing -> return (ConstantOperand (GlobalReference (toLLVMType (getType e)) (Name i)))



expression lr (BExpr _ AssignOp e1 e2) = do
    let a = getAlignment e1
    e1' <- expression L e1
    e2' <- expression R e2
    store e1' a e2'
    case lr of
      L -> return e1'
      R -> return e2'

expression R (BExpr _ op e1 e2) = do
  l <- expression R e1
  r <- expression R e2
  case op of
    Plus         -> add l r
    Minus        -> sub l r
    Mult         -> mul l r
    LessThan     -> icmp Pred.SLT l r
    EqualsEquals -> icmp Pred.EQ l r
    NotEqual     -> icmp Pred.NE l r
    LAnd         -> and l r
    LOr          -> or l r

expression _ (List l) = error "cannot handle lists of expressions"
expression _ (Ternary _ i t e)   = do undefined
expression _ (Assign _ l r)      = error "this kind of assign expression should not appear in the AST anymore anyway"
expression _ (SizeOfType _ t)    = do undefined
expression _ (ArrayAccess _ a i) = do undefined


expression _ (UExpr _ Address e) = expression L e

expression _ (UExpr _ Deref e) = do
  let (Pointer t)  = getType e
  e' <- expression R e
  load e' (alignmentOfType t)

expression R (UExpr _ op e) = do
  e' <- expression R e
  case op of
    SizeOf -> int32 4
    Address -> undefined
    Neg -> do
      x <- int32 0
      sub x e'
    Not -> undefined

expression _ (Func _ f (List es )) = do
  f' <- expression L f
  p' <- mapM (expression R) es
  let p'' = zip p' (repeat [])
  call f' p''

-- hackish
expression lr (Func a f e) = expression  lr (Func  a f (List [e]))

expression _ (FieldAccess _ f i)   = do undefined
expression _ (PointerAccess _ p i) = do undefined
expression _ (StringLiteral _ s)   = do undefined
expression _ (CharConstant _ c)    = int32 42  -- TODO
expression _ (IntConstant _ i)     = int32 i  -- TODO


alignmentOfType CInt        = 4
alignmentOfType (Pointer _) = 8

getAlignment :: HasType t => t -> Word32
getAlignment = alignmentOfType . getType
  --------------------------------------------------------------------------------
--  some constants we might need


toLLVMType :: CType -> LLVM.AST.Type
toLLVMType CVoid                    = LLVM.AST.Type.void
toLLVMType CInt                     = i32
toLLVMType (Pointer t)              = PointerType (toLLVMType t) (AddrSpace 0)
toLLVMType (SemAst.Function r ptys) = FunctionType (toLLVMType r) (map toLLVMType ptys) False 
