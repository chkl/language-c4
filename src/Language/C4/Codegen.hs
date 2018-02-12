{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
import           LLVM.AST                   hiding (function)
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

newtype CGState = CGState { _labels :: Map.Map Ident Name}


emptyCGState :: CGState
emptyCGState = CGState Map.empty

newtype CGModule a = CGModule { unCGModule :: ModuleBuilderT (StateT CGState C4) a }
  deriving (Functor, Applicative, Monad, MonadFix, MonadError (SourcePos, String), MonadModuleBuilder, MonadState CGState)


runCGModule :: CGModule a -> C4 Module
runCGModule m = fst <$> runStateT (buildModuleT "blah" $ unCGModule m) emptyCGState

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
externalDeclaration (Declaration p t ids) = throwC4 (FeatureNotImplemented p "declarations")

functionDefinition :: FunctionDefinition SemPhase -> CGModule ()
functionDefinition (FunctionDefinition p t d stmt) =
  let funName = Name (getName d)
      (SemAst.Function b a)= getType d
      paramTypes = [(i32, "a"), (i32, "b")]
  in do
      _ <- function funName paramTypes (toLLVMType b) $ \params -> statement stmt
      return ()

declaration :: Declaration SemPhase -> CGBlock ()
declaration = undefined

statement :: Stmt SemPhase -> CGBlock ()
statement (Return _ Nothing) = retVoid
statement (Return _ (Just e)) = do
  e' <- expression e
  ret e'
statement (CompoundStmt _ stmtsOrDecls) = mapM_ f stmtsOrDecls
  where f = either declaration statement

statement (ExpressionStmt _ Nothing) = return ()
statement (ExpressionStmt _ (Just e)) = do
  _ <- expression e
  return ()

statement (IfStmt _ c s1 ms2) = mdo
    c' <- expression c
    condBr c' thB elB
    thB <- block
    statement s1
    elB <- block
    maybe (return ()) statement ms2

statement (WhileStmt _ c s) = mdo
  hd <- block `named` "head"
  e' <- expression c
  condBr e' bd tl
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


expression :: Expr SemPhase -> CGBlock Operand
--expression (ExprIdent _ i) = Name i
expression (BExpr _ op e1 e2) = do
  l <- expression e1
  r <- expression e2
  case op of
    Plus         -> add l r
    Minus        -> sub l r
    Mult         -> mul l r
    LessThan     -> icmp Pred.SLT l r
    EqualsEquals -> icmp Pred.EQ l r
    NotEqual     -> icmp Pred.NE l r
    LAnd         -> and l r
    LOr          -> or l r
    AssignOp     -> undefined
expression (List l) = do undefined
expression (Ternary _ i t e) = do undefined
expression (Assign _ l r) = do undefined
expression (SizeOfType _ t) = do undefined
expression (ArrayAccess _ a i) = do undefined
expression (UExpr _ op e) = do
  e' <- expression e
  case op of
    SizeOf -> int32 4
    Address -> undefined
    Deref -> undefined
    Neg -> do
      x <- int32 0
      sub x e'
    Not -> undefined
expression (Func _ f p) = do undefined
expression (ExprIdent _ i) = do undefined
expression (FieldAccess _ f i) = do undefined
expression (PointerAccess _ p i) = do undefined
expression (StringLiteral _ s) = do undefined
expression (CharConstant _ c) = int32 42  -- TODO
expression (IntConstant _ i) = int32 i  -- TODO


--------------------------------------------------------------------------------
--  some constants we might need
toLLVMType (CInt) = i32
