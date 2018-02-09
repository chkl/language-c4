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
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import qualified Data.Map                   as Map


import           LLVM.AST                   hiding (function)
import           LLVM.AST.Type
import           LLVM.IRBuilder.Constant
import           LLVM.IRBuilder.Instruction
import           LLVM.IRBuilder.Module
import           LLVM.IRBuilder.Monad
import           LLVM.Pretty                (ppllvm)

import           Language.C4.Ast.SemAst     as SemAst
import           Language.C4.Types

-- | We use the builder monads provided by llvm-hs in combination to our C4 Monad so we can still throw errors and have access to a future log / configuration.

data CGState = CGState { _labels :: Map.Map Ident BasicBlock }


emptyCGState :: CGState
emptyCGState = CGState Map.empty

newtype CGModule a = CGModule { unCGModule :: ModuleBuilderT (StateT CGState C4) a }
  deriving (Functor, Applicative, Monad, MonadFix, MonadError (SourcePos, String), MonadModuleBuilder)


runCGModule :: CGModule a -> C4 Module
runCGModule m = fst <$> runStateT (buildModuleT "blah" $ unCGModule m) emptyCGState

type CGBlock  = IRBuilderT CGModule

data CodegenError = FeatureNotImplemented { codegenErrorPosition :: ! SourcePos, featureName :: String }

instance C4Error CodegenError where
  getErrorPosition = codegenErrorPosition
  getErrorComponent err = "feature not implemented: " ++ featureName err

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


expression :: Expr SemPhase -> _
--expression (ExprIdent _ i) = Name i
expression (BExpr _ Plus e1 e2) = do
  l <- expression e1
  r <- expression e2
  add l r
expression (Constant _ b) = int32 42  -- TODO


--------------------------------------------------------------------------------
--  some constants we might need
toLLVMType (CInt) = i32
