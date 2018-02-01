{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}

module Language.C4.Ast.SemAst
  ( module Language.C4.Ast.SemAst
  , module Language.C4.Ast
  ) where

import           Data.List       (intercalate)
import qualified Data.Map.Strict as Map

import           Language.C4.Ast
import           Language.C4.Types

--------------------------------------------------------------------------------
-- Define Types for our Type-Annotated AST
--------------------------------------------------------------------------------

data SemPhase

type instance AnnTranslationUnit  SemPhase   =  Scope
type instance AnnFunctionDefinition SemPhase =  SourcePos

-- expressions
type instance AnnTernary SemPhase            = (SourcePos, CType)
type instance AnnAssign SemPhase             = (SourcePos, CType)
type instance AnnArrayAccess SemPhase        = (SourcePos, CType)
type instance AnnBExpr SemPhase              = (SourcePos, CType)
type instance AnnUExpr SemPhase              = (SourcePos, CType)
type instance AnnFunc SemPhase               = (SourcePos, CType)
type instance AnnSizeOfType SemPhase         = (SourcePos, CType)
type instance AnnExprIdent SemPhase          = (SourcePos, CType)
type instance AnnConstant SemPhase           = (SourcePos, CType)
type instance AnnFieldAccess SemPhase        = (SourcePos, CType)
type instance AnnPointerAccess SemPhase      = (SourcePos, CType)
type instance AnnStringLiteral SemPhase      = (SourcePos, CType)

-- declarations
type instance AnnDeclaration SemPhase        = SourcePos
type instance AnnStructDeclaration SemPhase  = SourcePos

-- declarators
type instance AnnIndirectDeclarator SemPhase = DeclaratorSemAnn
type instance AnnDeclaratorId SemPhase       = DeclaratorSemAnn
type instance AnnFunctionDeclarator SemPhase = DeclaratorSemAnn

-- parameters
type instance AnnParameter SemPhase         = (SourcePos, CType)
type instance AnnAbstractParameter SemPhase = (SourcePos, CType)

-- statements
type instance AnnCompoundStmt SemPhase       = SourcePos
type instance AnnIfStmt SemPhase             = SourcePos
type instance AnnWhileStmt SemPhase          = SourcePos
type instance AnnGoto SemPhase               = SourcePos
type instance AnnContinue SemPhase           = SourcePos
type instance AnnBreak SemPhase              = SourcePos
type instance AnnReturn SemPhase             = SourcePos
type instance AnnLabeledStmt SemPhase        = SourcePos
type instance AnnExpressionStmt SemPhase     = SourcePos

data DeclaratorSemAnn = DeclaratorSemAnn
  { _position :: SourcePos
  , _type     :: CType
  , _name     :: Ident
  }

-- this is getting insanely tedious!
class HasType x where
  getType :: x -> CType

class HasName x where
  getName :: x -> Ident


-- how to get the type of an expression?
instance HasType (Expr SemPhase) where
  getType (BExpr (_,t) _ _ _)       = t
  getType (Assign (_,t)  _ _)       = t
  getType (List es)                 = Tuple $ map getType es
  getType (Ternary (_,t) _ _ _)     = t
  getType (UExpr (_,t) _ _)         = t
  getType (Func (_,t) _ _ )         = t
  getType (Constant (_,t) _ )       = t
  getType (ArrayAccess (_,t) _ _ )  = t
  getType (SizeOfType (_,t) _ )     = t
  getType (ExprIdent (_,t) _ )      = t
  getType (PointerAccess (_,t) _ _) = t
  getType (FieldAccess (_,t) _ _)   = t
  getType (StringLiteral (_,t) _)   = t


instance HasType (Parameter SemPhase) where
  getType (Parameter (_,t) _ _ )         = t
  getType (AbstractParameter (_,t) _ _ ) = t

instance HasType (Declarator SemPhase) where
  getType = _type . getDeclaratorSemAnn

instance HasType (AbstractDeclarator SemPhase) where -- TODO


instance HasName (Declarator SemPhase) where
  getName = _name . getDeclaratorSemAnn


class GetDeclaratorSemAnn x where
  getDeclaratorSemAnn :: x -> DeclaratorSemAnn





instance GetDeclaratorSemAnn (Declarator SemPhase) where
  getDeclaratorSemAnn (IndirectDeclarator a  _)  = a
  getDeclaratorSemAnn (DeclaratorId a _)         = a
  getDeclaratorSemAnn (FunctionDeclarator a _ _) = a


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

