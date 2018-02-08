{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE TypeFamilies          #-}


module Language.C4.Codegen
  ( compile
  , ppllvm
  ) where

-- import qualified LLVM.AST.Attribute         as A
-- import qualified LLVM.AST.CallingConvention as CC
-- import qualified LLVM.AST.Linkage           as L
import           Prelude                    hiding (and, or, EQ)
import           Data.ByteString
import           Data.Text.Lazy.IO          as T
import           Language.C4.Ast.SemAst     as SemAst
import           LLVM.AST                   hiding (function)
import           LLVM.AST.Type
import           LLVM.AST.IntegerPredicate  as Pred
import           LLVM.IRBuilder.Constant
import           LLVM.IRBuilder.Instruction
import           LLVM.IRBuilder.Module
import           LLVM.IRBuilder.Monad
import           LLVM.Pretty                (ppllvm)

import           Language.C4.Ast.SemAst
import           Language.C4.Types

-- | We use the builder monads provided by llvm-hs in combination to our C4 Monad so we can still throw errors and have access to a future log / configuration.
type Codegen a = ModuleBuilderT C4 a

data CodegenError = FeatureNotImplemented { codegenErrorPosition :: ! SourcePos, featureName :: String }

instance C4Error CodegenError where
  getErrorPosition = codegenErrorPosition
  getErrorComponent err = "feature not implemented: " ++ featureName err

compile :: TranslationUnit SemPhase -> C4 LLVM.AST.Module
compile (TranslationUnit _ es) = buildModuleT "THE_MODULE" $
  mapM_ (either declaration functionDefinition) es



--declaration :: Declaration SemPhase -> Codegen Operand
declaration (Declaration p t ids) = throwC4 (FeatureNotImplemented p "declarations")

functionDefinition :: FunctionDefinition SemPhase -> Codegen Operand
functionDefinition (FunctionDefinition p t d stmt) =
  let funName = Name (getName d)
      (SemAst.Function b a)= getType d
      paramTypes = [(i32, "a"), (i32, "b")]
  in
    function funName paramTypes (toLLVMType b) $ \params -> do
--      entry <- block `named` "entry"
      statement stmt

statement :: Stmt SemPhase -> _
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
expression (BExpr _ op e1 e2) = do
  l <- expression e1
  r <- expression e2
  case op of
    Plus -> add l r
    Minus -> sub l r
    Mult -> mul l r
    LessThan -> icmp Pred.SLT l r
    EqualsEquals -> icmp Pred.EQ l r
    NotEqual -> icmp Pred.NE l r
    LAnd -> and l r 
    LOr -> or l r
expression (List _) = do undefined
expression (Ternary _ _ _ _) = do undefined
expression (Assign _ _ _) = do undefined
expression (SizeOfType _ _) = do undefined
expression (ArrayAccess _ _ _) = do undefined
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
expression (Func _ _ _) = do undefined
expression (ExprIdent _ _) = do undefined
expression (FieldAccess _ _ _) = do undefined
expression (PointerAccess _ _ _) = do undefined
expression (StringLiteral _ _) = do undefined

    
  
expression (Constant _ b) = int32 42  -- TODO


translateUnit :: TranslationUnit SemPhase -> ModuleBuilder ()
translateUnit = undefined

--------------------------------------------------------------------------------
--  some constants we might need
toLLVMType (CInt) = i32
