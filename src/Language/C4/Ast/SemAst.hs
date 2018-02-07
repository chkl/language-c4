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
type Scope = Map.Map Ident CType

data SemPhase

type instance AnnTranslationUnit  SemPhase   =  Scope
type instance AnnFunctionDefinition SemPhase =  SourcePos

-- expressions
type instance AnnTernary SemPhase            = (SourcePos, CType, LValuedness)
type instance AnnAssign SemPhase             = (SourcePos, CType, LValuedness)
type instance AnnArrayAccess SemPhase        = (SourcePos, CType, LValuedness)
type instance AnnBExpr SemPhase              = (SourcePos, CType, LValuedness)
type instance AnnUExpr SemPhase              = (SourcePos, CType, LValuedness)
type instance AnnFunc SemPhase               = (SourcePos, CType, LValuedness)
type instance AnnSizeOfType SemPhase         = (SourcePos, CType, LValuedness)
type instance AnnExprIdent SemPhase          = (SourcePos, CType, LValuedness)
type instance AnnConstant SemPhase           = (SourcePos, CType, LValuedness)
type instance AnnFieldAccess SemPhase        = (SourcePos, CType, LValuedness)
type instance AnnPointerAccess SemPhase      = (SourcePos, CType, LValuedness)
type instance AnnStringLiteral SemPhase      = (SourcePos, CType, LValuedness)

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

data LValuedness = LValue | RValue

-- this is getting insanely tedious!
class HasType x where
  getType :: x -> CType

class HasName x where
  getName :: x -> Ident

class HasLValuedness x where
  getLValuedness :: x -> LValuedness


-- how to get the type of an expression?
instance HasType (Expr SemPhase) where
  getType (BExpr (_,t,_) _ _ _)       = t
  getType (Assign (_,t,_)  _ _)       = t
  getType (List es)                 = Tuple $ map getType es
  getType (Ternary (_,t,_) _ _ _)     = t
  getType (UExpr (_,t,_) _ _)         = t
  getType (Func (_,t,_) _ _ )         = t
  getType (Constant (_,t,_) _ )       = t
  getType (ArrayAccess (_,t,_) _ _ )  = t
  getType (SizeOfType (_,t,_) _ )     = t
  getType (ExprIdent (_,t,_) _ )      = t
  getType (PointerAccess (_,t,_) _ _) = t
  getType (FieldAccess (_,t,_) _ _)   = t
  getType (StringLiteral (_,t,_) _)   = t


instance HasType (Parameter SemPhase) where
  getType (Parameter (_,t) _ _ )         = t
  getType (AbstractParameter (_,t) _ _ ) = t

instance HasType (Declarator SemPhase) where
  getType = _type . getDeclaratorSemAnn

instance HasType (AbstractDeclarator SemPhase) where -- TODO


instance HasName (Declarator SemPhase) where
  getName = _name . getDeclaratorSemAnn

instance HasLValuedness (Expr SemPhase) where -- TODO
  getLValuedness (BExpr (_,_,l) _ _ _)       = l
  getLValuedness (Assign (_,_,l)  _ _)       = l
  getLValuedness (List es)                 = undefined
  getLValuedness (Ternary (_,_,l) _ _ _)     = l
  getLValuedness (UExpr (_,_,l) _ _)         = l
  getLValuedness (Func (_,_,l) _ _ )         = l
  getLValuedness (Constant (_,_,l) _ )       = l
  getLValuedness (ArrayAccess (_,_,l) _ _ )  = l
  getLValuedness (SizeOfType (_,_,l) _ )     = l
  getLValuedness (ExprIdent (_,_,l) _ )      = l
  getLValuedness (PointerAccess (_,_,l) _ _) = l
  getLValuedness (FieldAccess (_,_,l) _ _)   = l
  getLValuedness (StringLiteral (_,_,l) _)   = l
  
class GetDeclaratorSemAnn x where
  getDeclaratorSemAnn :: x -> DeclaratorSemAnn





instance GetDeclaratorSemAnn (Declarator SemPhase) where
  getDeclaratorSemAnn (IndirectDeclarator a  _)  = a
  getDeclaratorSemAnn (DeclaratorId a _)         = a
  getDeclaratorSemAnn (FunctionDeclarator a _ _) = a


-------------------------------------------------------------------------------
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

fromType :: Type x -> CType
fromType Void = CVoid
fromType Int  = CInt
fromType Char = CChar
fromType _    = Bottom -- TODO:

