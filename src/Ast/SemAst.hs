{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}

module Ast.SemAst
  ( module Ast.SemAst
  , module Ast
  ) where

import           Ast
import qualified Data.Map.Strict as Map
import           Types
import           Data.List            (intercalate)

--------------------------------------------------------------------------------
-- Define Types for our Type-Annotated AST
--------------------------------------------------------------------------------

data SemPhase

type instance AnnTranslationUnit  SemPhase   =  Scope
type instance AnnFunctionDefinition SemPhase = (SourcePos, Scope)
type instance AnnTernary SemPhase            = (SourcePos, Scope, CType)
type instance AnnAssign SemPhase             = (SourcePos, Scope, CType)
type instance AnnArray SemPhase              = (SourcePos, Scope, CType)
type instance AnnBExpr SemPhase              = (SourcePos, Scope, CType)
type instance AnnUExpr SemPhase              = (SourcePos, Scope, CType)
type instance AnnFunc SemPhase               = (SourcePos, Scope, CType)
type instance AnnSizeOfType SemPhase         = (SourcePos, Scope, CType)
type instance AnnExprIdent SemPhase          = (SourcePos, Scope, CType)
type instance AnnConstant SemPhase           = (SourcePos, Scope, CType)
type instance AnnFieldAccess SemPhase        = (SourcePos, Scope, CType)
type instance AnnPointerAccess SemPhase      = (SourcePos, Scope, CType)
type instance AnnStringLiteral SemPhase      = (SourcePos, Scope, CType)
type instance AnnDeclaration SemPhase        = (SourcePos, Scope)
type instance AnnStructDeclaration SemPhase  = (SourcePos, Scope, CType)

type instance AnnIndirectDeclarator SemPhase = DeclaratorSemAnn
type instance AnnDeclaratorId SemPhase       = DeclaratorSemAnn
type instance AnnFunctionDeclarator SemPhase = DeclaratorSemAnn

type instance AnnParameter SemPhase = (SourcePos, Scope, CType)
type instance AnnAbstractParameter SemPhase = (SourcePos, Scope, CType)

type instance AnnCompoundStmt SemPhase       = (SourcePos, Scope)
type instance AnnIfStmt SemPhase             = (SourcePos, Scope)
type instance AnnWhileStmt SemPhase          = (SourcePos, Scope)
type instance AnnGoto SemPhase               = (SourcePos, Scope)
type instance AnnContinue SemPhase           = (SourcePos, Scope)
type instance AnnBreak SemPhase              = (SourcePos, Scope)
type instance AnnReturn SemPhase             = (SourcePos, Scope)
type instance AnnLabeledStmt SemPhase        = SourcePos

data DeclaratorSemAnn = DeclaratorSemAnn
  { _position :: SourcePos
  , _scope    :: Scope
  , _type     :: CType
  , _name     :: Ident
  }

class HasType x where
  getType :: x -> CType

class HasName x where
  getName :: x -> Ident

class HasScope x where
  getScope :: x -> Scope

-- how to get the type of an expression?
instance HasType (Expr SemPhase) where
  getType (BExpr (p,s,t) _ _ _)= t
  getType (Assign (p,s,t)  _ _)= t
  getType (List es)              = Tuple $ map getType es
  getType (Ternary _ _ _ _)      = error "ternary"
  getType (UExpr _ _ _)          = error "unary"
  getType (Func _ _ _ )          = error "func"
  getType (Constant _ _ )        = error "const"
  getType (Array _ _ _ )         = error "const"
  getType (SizeOfType _ _ )      = error "sizeoftype"
  getType (ExprIdent (p,s,t) n ) = t

instance HasType (Parameter SemPhase) where
  getType (Parameter (p,s,t) _ _ ) = t

instance HasType (Declarator SemPhase) where
  getType = _type . getDeclaratorSemAnn

instance HasName (Declarator SemPhase) where
  getName = _name . getDeclaratorSemAnn

instance HasScope (TranslationUnit SemPhase) where
  getScope (TranslationUnit a _) = a

class GetDeclaratorSemAnn x where
  getDeclaratorSemAnn :: x -> DeclaratorSemAnn





instance GetDeclaratorSemAnn (Declarator SemPhase) where
  getDeclaratorSemAnn (IndirectDeclarator a _ _) = a
  getDeclaratorSemAnn (DeclaratorId a _)         = a
  getDeclaratorSemAnn (FunctionDeclarator a _ _) = a



--------------------------------------------------------------------------------
-- Testing Lenses
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
--  some data types needed for semantic ASTs
--------------------------------------------------------------------------------
-- some data types
data CType = CInt
           | CVoid
           | CChar
           | Pointer CType
           | Tuple [CType]
           | Function CType [CType]
           | Bottom -- c.f. undefined in Haskell
           deriving (Eq)


newtype Scope = Scope (Map.Map Ident CType)

instance Show Scope where
  show (Scope m) = unlines $ map f (Map.toList m)
    where f (k,v) = show k ++ " :: " ++ show v

instance Show CType where
  show CInt = "Int"
  show CVoid = "Void"
  show CChar = "Char"
  show Bottom         = "⊥"
  show (Pointer t)    = "*" ++ show t
  show (Function t p) = "(" ++ pl ++ ") -> " ++ show t
    where pl          = intercalate ", " $ map show p
  show (Tuple p)      = "(" ++ pl ++ ")"
    where pl          = intercalate ", " $ map show p

emptyScope :: Scope
emptyScope = Scope Map.empty

fromType :: Type x -> CType
fromType Void = CVoid
fromType Int  = CInt
fromType Char = CChar
fromType _    = Bottom -- TODO:


