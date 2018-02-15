{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}

module Language.C4.Ast.SynAst
  ( module Language.C4.Ast.SynAst
  , module Language.C4.Ast -- re-export the main AST module
  ) where

import           Text.Megaparsec.Pos (SourcePos)

import           Language.C4.Ast

data SynPhase

type instance AnnTranslationUnit SynPhase    =  ()
type instance AnnFunctionDefinition SynPhase =  SourcePos
type instance AnnTernary SynPhase            =  SourcePos
type instance AnnAssign SynPhase             =  SourcePos
type instance AnnArrayAccess SynPhase        =  SourcePos
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
type instance AnnExpressionStmt SynPhase     =  SourcePos
type instance AnnGoto SynPhase               =  SourcePos
type instance AnnContinue SynPhase           =  SourcePos
type instance AnnBreak SynPhase              =  SourcePos
type instance AnnReturn SynPhase             =  SourcePos
type instance AnnLabeledStmt SynPhase        =  SourcePos
type instance AnnParameter SynPhase          = SourcePos
type instance AnnAbstractParameter SynPhase  = SourcePos
type instance AnnAbstractDeclarator SynPhase = SourcePos

deriving instance Show (Expr SynPhase)
deriving instance Show (Initializer SynPhase)
deriving instance Show (InitDeclarator SynPhase)
deriving instance Show (Declaration SynPhase)
deriving instance Show (Stmt SynPhase)
deriving instance Show (AbstractDeclarator SynPhase)
deriving instance Show (Parameter SynPhase)
deriving instance Show (Declarator SynPhase)
deriving instance Show (StructDeclaration SynPhase)
deriving instance Show (Type SynPhase)
deriving instance Show (FunctionDefinition SynPhase)
deriving instance Show (TranslationUnit SynPhase)

deriving instance Eq SynPhase
deriving instance Eq (Expr SynPhase)
deriving instance Eq (Initializer SynPhase)
deriving instance Eq (InitDeclarator SynPhase)
deriving instance Eq (Declaration SynPhase)
deriving instance Eq (Stmt SynPhase)
deriving instance Eq (AbstractDeclarator SynPhase)
deriving instance Eq (Parameter SynPhase)
deriving instance Eq (Declarator SynPhase)
deriving instance Eq (StructDeclaration SynPhase)
deriving instance Eq (Type SynPhase)
deriving instance Eq (FunctionDefinition SynPhase)
deriving instance Eq (TranslationUnit SynPhase)

-- we would like to have a class "HasSourcePos" for each annotated element of
-- the AST, e.g. Stmt SynPhase, or Expr SynPhase. I could not come up with a way
-- to get those automatically. Hence this tedious boilerplate.

class HasSourcePos x where
 sourcePos :: x -> SourcePos

instance HasSourcePos (FunctionDefinition SynPhase) where
  sourcePos (FunctionDefinition p _ _ _) = p

instance HasSourcePos (Declaration SynPhase) where
  sourcePos (Declaration p _ _) = p

instance HasSourcePos (Declarator SynPhase) where
  sourcePos (IndirectDeclarator p _ )   = p
  sourcePos (DeclaratorId p _)          = p
  sourcePos (FunctionDeclarator p _ _ ) = p

instance HasSourcePos (Stmt SynPhase) where
 sourcePos (LabeledStmt p _ _ ) = p
 sourcePos (CompoundStmt p _ )  = p
 sourcePos (IfStmt p _ _ _ )    = p
 sourcePos (Goto p _ )          = p
 sourcePos (ExpressionStmt p _) = p
 sourcePos (WhileStmt p _ _ )   = p
 sourcePos (Continue p)         = p
 sourcePos (Break p)            = p
 sourcePos (Return p _ )        = p


instance HasSourcePos (Expr SynPhase) where
  sourcePos (List es)             = sourcePos (head es)
  sourcePos (Ternary p _ _ _)     = p
  sourcePos (Assign p  _ _)       = p
  sourcePos (BExpr p _ _ _)       = p
  sourcePos (UExpr p _ _)         = p
  sourcePos (SizeOfType p _)      = p
  sourcePos (ArrayAccess p _ _)   = p
  sourcePos (Func p _ _)          = p
  sourcePos (ExprIdent p _)       = p
  sourcePos (Constant p _)        = p
  sourcePos (FieldAccess p _ _)   = p
  sourcePos (PointerAccess p _ _) = p
  sourcePos (StringLiteral p _)   = p

