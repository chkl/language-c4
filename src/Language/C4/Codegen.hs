{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecursiveDo           #-}

module Language.C4.Codegen where

-- import qualified LLVM.AST.Attribute         as A
-- import qualified LLVM.AST.CallingConvention as CC
-- import qualified LLVM.AST.Constant          as C
-- import qualified LLVM.AST.Linkage           as L
import           Data.Text.Lazy.IO          as T
import           LLVM.AST                   hiding (function)
import           LLVM.AST.Type
import           LLVM.IRBuilder.Instruction
import           LLVM.IRBuilder.Module
import           LLVM.IRBuilder.Monad
import           LLVM.Pretty                (ppllvm)

import           Language.C4
import           Language.C4.Ast.SemAst

-- | We use the builder monads provided by llvm-hs in combination to our C4 Monad so we can still throw errors and have access to a future log / configuration.
type Codegen a = ModuleBuilderT C4 a

data CodegenError = FeatureNotImplemented { codegenErrorPosition :: ! SourcePos, featureName :: String }

instance C4Error CodegenError where
  getErrorPosition = codegenErrorPosition
  getErrorComponent err = "feature not implemented: " ++ featureName err

compile :: TranslationUnit SemPhase -> C4 LLVM.AST.Module
compile (TranslationUnit _ es) = buildModuleT "THE_MODULE" $
  mapM_ (either declaration functionDefinition) es



declaration :: Declaration SemPhase -> Codegen Operand
declaration (Declaration p t ids) = throwC4 (FeatureNotImplemented p "declarations")

functionDefinition :: FunctionDefinition SemPhase -> Codegen Operand
functionDefinition _ = function "add" [(i32, "a"), (i32, "b")] i32 $ \[a,b] -> mdo
  entry <- block `named` "entry"; do
    c <- add a b
    ret c

translateUnit :: TranslationUnit SemPhase -> ModuleBuilder ()
translateUnit = undefined

debug :: Module -> IO ()
debug = T.putStrLn . ppllvm
--------------------------------------------------------------------------------
--  some constants we might need
