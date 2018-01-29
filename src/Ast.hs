{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE TypeFamilies          #-}

module Ast where

import           Data.ByteString.Lazy (ByteString)
import           Types

--------------------------------------------------------------------------------
type family AnnTranslationUnit x
type family AnnFunctionDefinition x
type family AnnTernary x
type family AnnAssign x
type family AnnArray x
type family AnnBExpr x
type family AnnUExpr x
type family AnnFunc x
type family AnnSizeOfType x
type family AnnExprIdent x
type family AnnConstant x
type family AnnFieldAccess x
type family AnnPointerAccess x
type family AnnStringLiteral x
type family AnnDeclaration x
type family AnnIndirectDeclarator x
type family AnnStructDeclaration x
type family AnnDeclaratorId x
type family AnnFunctionDeclarator x
type family AnnCompoundStmt x
type family AnnIfStmt x
type family AnnWhileStmt x
type family AnnExpressionStmt x
type family AnnGoto x
type family AnnContinue x
type family AnnBreak x
type family AnnReturn x
type family AnnLabeledStmt x
type family AnnParameter x
type family AnnAbstractParameter x

--------------------------------------------------------------------------------
-- Root / Translation Units
--------------------------------------------------------------------------------


data TranslationUnit x = TranslationUnit
  { _ann                  :: AnnTranslationUnit x
  , _externalDeclarations :: [ExternalDeclaration x]
  }

type ExternalDeclaration x = Either (Declaration x) (FunctionDefinition x)

data FunctionDefinition x = FunctionDefinition (AnnFunctionDefinition x) (Type x) (Declarator x) (Stmt x)

--------------------------------------------------------------------------------
-- Declarations
--------------------------------------------------------------------------------
type Pointers = Int

data Declaration x = Declaration (AnnDeclaration x) (Type x) [InitDeclarator x]

data Type x = Void
          | Char
          | Int
          | StructIdentifier Ident
          | StructInline (Maybe Ident) [StructDeclaration x]

data StructDeclaration x = StructDeclaration (AnnStructDeclaration x) (Type x) [Declarator x ]


-- | first parameter is the number of stars
data Declarator x = IndirectDeclarator (AnnIndirectDeclarator x) Pointers (Declarator x)
                  | DeclaratorId (AnnDeclaratorId x) Ident
                  | FunctionDeclarator (AnnFunctionDeclarator x) (Declarator x) [Parameter x]

-- TODO: Find a way to unify declarator and abstract declarator
data AbstractDeclarator x = IndirectAbstractDeclarator Pointers (AbstractDeclarator x)
                        | AbstractFunctionDeclarator (AbstractDeclarator x) [Parameter x]
                        | ArrayStar (AbstractDeclarator x)

-- | In contrast to the spec this takes only one initializer
data InitDeclarator x = InitializedDec (Declarator x) (Maybe (Initializer x))


data Initializer x = InitializerAssignment (Expr x) -- assignment expression
                 | InitializerList [Initializer x]

data Parameter x =  Parameter (AnnParameter x) (Type x) (Declarator x)
               |  AbstractParameter (AnnAbstractParameter x) (Type x) (Maybe (AbstractDeclarator x))
--------------------------------------------------------------------------------
-- Statements
--------------------------------------------------------------------------------

data Stmt x = LabeledStmt (AnnLabeledStmt x) Ident (Stmt x)
          | CompoundStmt (AnnCompoundStmt x) [Either (Declaration x) (Stmt x)]
          | ExpressionStmt (AnnExpressionStmt x) (Maybe (Expr x))
          | IfStmt (AnnIfStmt x) (Expr x) (Stmt x) (Maybe (Stmt x))
          | WhileStmt (AnnWhileStmt x) (Expr x) (Stmt x)
          | Goto (AnnGoto x) Ident
          | Continue (AnnContinue x)
          | Break (AnnBreak x)
          | Return (AnnReturn x) (Maybe (Expr x))

--------------------------------------------------------------------------------
-- Expressions
--------------------------------------------------------------------------------

data BOp = Mult | Plus | Minus | LessThan | EqualsEquals
         | NotEqual | LAnd | LOr | AssignOp
         deriving (Show, Eq)

data UOp = SizeOf | Address | Deref | Neg | Not
         deriving (Show, Eq)



data Expr x = List [Expr x] -- ^ should never be empty
          | Ternary(AnnTernary x) (Expr x) (Expr x) (Expr x)
          | Assign (AnnAssign x) (Expr x) (Expr x)
          | BExpr (AnnBExpr x) BOp (Expr x) (Expr x)
          | UExpr (AnnUExpr x) UOp (Expr x)
          | SizeOfType (AnnSizeOfType x) (Type x)
          | Array (AnnArray x) (Expr x) (Expr x)
          | Func (AnnFunc x) (Expr x) (Expr x)
          | ExprIdent (AnnExprIdent x) ByteString
          | Constant (AnnConstant x) ByteString
          | FieldAccess (AnnFieldAccess x) (Expr x) (Expr x)
          | PointerAccess (AnnPointerAccess x) (Expr x) (Expr x)
          | StringLiteral (AnnStringLiteral x) ByteString

--------------------------------------------------------------------------------
--  Undecorated Trees
--------------------------------------------------------------------------------
data UD

type instance AnnTranslationUnit UD    = ()
type instance AnnFunctionDefinition UD = ()
type instance AnnTernary UD            = ()
type instance AnnAssign UD             = ()
type instance AnnArray UD              = ()
type instance AnnBExpr UD              = ()
type instance AnnUExpr UD              = ()
type instance AnnFunc UD               = ()
type instance AnnSizeOfType UD         = ()
type instance AnnExprIdent UD          = ()
type instance AnnConstant UD           = ()
type instance AnnFieldAccess UD        = ()
type instance AnnPointerAccess UD      = ()
type instance AnnStringLiteral UD      = ()
type instance AnnDeclaration UD        = ()
type instance AnnIndirectDeclarator UD = ()
type instance AnnStructDeclaration UD  = ()
type instance AnnDeclaratorId UD       = ()
type instance AnnFunctionDeclarator UD = ()
type instance AnnCompoundStmt UD       = ()
type instance AnnIfStmt UD             = ()
type instance AnnWhileStmt UD          = ()
type instance AnnExpressionStmt UD     = ()
type instance AnnGoto UD               = ()
type instance AnnContinue UD           = ()
type instance AnnBreak UD              = ()
type instance AnnReturn UD             = ()
type instance AnnLabeledStmt UD        = ()
type instance AnnParameter UD          = ()
type instance AnnAbstractParameter UD  = ()

-- -- some pattern synonyms
-- pattern LabeledStmtUD :: Ident -> Stmt UD -> Stmt UD
-- pattern LabeledStmtUD l s <- LabeledStmt _ l s
--   where LabeledStmtUD l s = LabeledStmt () l s

-- pattern CompoundStmtUD :: [Either (Declaration UD) (Stmt UD)]-> Stmt UD
-- pattern CompoundStmtUD ss <- CompoundStmt _ ss
--   where CompoundStmtUD ss = CompoundStmt () ss


class Undecorate a where
  undecorate :: a x -> a UD

instance Undecorate TranslationUnit where
  undecorate (TranslationUnit _ s) = TranslationUnit () (map f s)
    where f = either (Left . undecorate) (Right . undecorate)

instance Undecorate FunctionDefinition where
  undecorate (FunctionDefinition _ t d s) = FunctionDefinition () (undecorate t) (undecorate d) (undecorate s)


instance Undecorate Declaration where
  undecorate (Declaration _ t xs) = Declaration () (undecorate t) (map undecorate xs)

instance Undecorate InitDeclarator where
  undecorate (InitializedDec d mi ) = InitializedDec (undecorate d) (fmap undecorate mi)



instance Undecorate Stmt where
  undecorate (LabeledStmt _ l s)   = LabeledStmt () l (undecorate s)
  undecorate (CompoundStmt _ es)   = CompoundStmt () (map f es)
    where f                        = either (Left . undecorate) (Right . undecorate)
  undecorate (ExpressionStmt _ me) = ExpressionStmt () (fmap undecorate me)
  undecorate (IfStmt _ e s ms)     = IfStmt () (undecorate e) (undecorate s) (fmap undecorate ms)
  undecorate (WhileStmt _ e s)     = WhileStmt () (undecorate e) (undecorate s)
  undecorate (Goto _ l)            = Goto () l
  undecorate (Break _)             = Break ()
  undecorate (Continue _)          = Continue ()
  undecorate (Return _ me)         = Return () (fmap undecorate me)

instance Undecorate Expr where
  undecorate (List es)               = List (map undecorate es)
  undecorate (Ternary _ e1 e2 e3)    = Ternary () (undecorate e1) (undecorate e2) (undecorate e3)
  undecorate (Assign _ e1 e2)        = Assign () (undecorate e1) (undecorate e2)
  undecorate (BExpr _ op e1 e2)      = BExpr () op (undecorate e1) (undecorate e2)
  undecorate (UExpr _ op e)          = UExpr () op (undecorate e)
  undecorate (SizeOfType _ t)        = SizeOfType () (undecorate t)
  undecorate (Array _ e1 e2)         = Array () (undecorate e1)  (undecorate e2)
  undecorate (Func _ e1 e2)          = Func () (undecorate e1) (undecorate e2)
  undecorate (ExprIdent _ b)         = ExprIdent () b
  undecorate (Constant _ b)          = Constant  () b
  undecorate (FieldAccess _ e1 e2)   = FieldAccess () (undecorate e1) (undecorate e2)
  undecorate (PointerAccess _ e1 e2) = PointerAccess () (undecorate e1) (undecorate e2)
  undecorate (StringLiteral _ b )    = StringLiteral () b

instance Undecorate Declarator where
  undecorate (IndirectDeclarator _ n d) = IndirectDeclarator () n (undecorate d)
  undecorate (DeclaratorId _ i)         = DeclaratorId () i
  undecorate (FunctionDeclarator _ d p) = FunctionDeclarator () (undecorate d) (map undecorate p)

instance Undecorate Initializer where
  undecorate (InitializerAssignment e) = InitializerAssignment (undecorate e)
  undecorate (InitializerList l)       = InitializerList (map undecorate l)

instance Undecorate Parameter where
  undecorate (Parameter _ t d)          = Parameter () (undecorate t) (undecorate d)
  undecorate (AbstractParameter _ t d) = AbstractParameter () (undecorate t) (fmap undecorate d)
 
instance Undecorate AbstractDeclarator where
  undecorate (IndirectAbstractDeclarator n d) = IndirectAbstractDeclarator n (undecorate d)
  undecorate (AbstractFunctionDeclarator d p) = AbstractFunctionDeclarator (undecorate d) (map undecorate p)
  undecorate (ArrayStar d)                    = ArrayStar (undecorate d)
  
instance Undecorate Type where
  undecorate Void = Void
  undecorate Char = Char
  undecorate Int  = Int
  undecorate (StructIdentifier b) = StructIdentifier b
  undecorate (StructInline b d)   = StructInline b (map undecorate d)

instance Undecorate StructDeclaration where
  undecorate (StructDeclaration _ t d) = StructDeclaration () (undecorate t) (map undecorate d)

