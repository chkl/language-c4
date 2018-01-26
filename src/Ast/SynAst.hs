{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}

module Ast.SynAst
  ( module Ast.SynAst
  , module Ast -- ^ re-export the main AST module
  ) where

import           Ast
import           Text.Megaparsec.Pos (SourcePos)

data SynPhase

type instance AnnTranslationUnit SynPhase    =  ()
type instance AnnFunctionDefinition SynPhase =  SourcePos
type instance AnnTernary SynPhase            =  SourcePos
type instance AnnAssign SynPhase             =  SourcePos
type instance AnnArray SynPhase              =  SourcePos
type instance AnnBExpr SynPhase              =  SourcePos
type instance AnnUExpr SynPhase              =  SourcePos
type instance AnnFunc SynPhase               =  SourcePos
type instance AnnSizeOfType SynPhase         =  SourcePos
type instance AnnExprIdent SynPhase          =  SourcePos
type instance AnnConstant SynPhase           =  SourcePos
type instance AnnFieldAccess SynPhase        =  SourcePos
type instance AnnPointerAccess SynPhase      =  SourcePos
type instance AnnStringLiteral SynPhase      =  SourcePos
type instance AnnDeclaration SynPhase        =  SourcePos
type instance AnnIndirectDeclarator SynPhase =  SourcePos
type instance AnnStructDeclaration SynPhase  =  SourcePos
type instance AnnDeclaratorId SynPhase       =  SourcePos
type instance AnnFunctionDeclarator SynPhase =  SourcePos
type instance AnnCompoundStmt SynPhase       =  SourcePos
type instance AnnIfStmt SynPhase             =  SourcePos
type instance AnnWhileStmt SynPhase          =  SourcePos
type instance AnnGoto SynPhase               =  SourcePos
type instance AnnContinue SynPhase           =  SourcePos
type instance AnnBreak SynPhase              =  SourcePos
type instance AnnReturn SynPhase             =  SourcePos
type instance AnnLabeledStmt SynPhase        =  SourcePos
type instance AnnParameter SynPhase = SourcePos
type instance AnnAbstractParameter SynPhase = SourcePos


-- we would like to have a class "HasSourcePos" for each annotated element of
-- the AST, e.g. Stmt SynPhase, or Expr SynPhase.

class HasSourcePos x where
  getSourcePos :: x -> SourcePos

instance HasSourcePos SourcePos where
  getSourcePos = id

instance HasSourcePos (Stmt SynPhase) where
  getSourcePos (LabeledStmt p _ _ ) = p
  getSourcePos _                    = error "niy"

instance HasSourcePos (Expr SynPhase) where
  getSourcePos = error "niy"
