{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module PrettyPrinter where

import           Prelude                      hiding (concatMap, print)

import           Control.Monad.State

import qualified Data.ByteString.Lazy         as LB
import           Data.ByteString.Lazy.Builder
import qualified Data.ByteString.Lazy.Char8   as C8
import           Data.Monoid                  ((<>))
import           Data.String
import           System.IO                    (Handle)
import           Text.Megaparsec.Error        hiding (ParseError)

import           Types

type Level = Int


class PrettyPrint a where
  prettyPrint :: a -> Printer ()

  toPrettyString :: a -> LB.ByteString
  toPrettyString x = toLazyByteString (prettyBuilder x)

  hPutPrettyPrint :: a -> Handle -> IO ()
  hPutPrettyPrint a h = hPutBuilder h (prettyBuilder a)

  prettyBuilder   :: a -> Builder
  prettyBuilder x = builder $ snd $ runPrinter (prettyPrint x) defaultPrinterEnv

  {-# MINIMAL (prettyPrint) #-}

defaultPrinterEnv :: PrinterEnv

defaultPrinterEnv = PrinterEnv 0 True mempty


data PrinterEnv = PrinterEnv { level     :: Int
                             , startLine :: Bool
                             , builder   :: Builder
                             }

newtype Printer a = Printer { runPrinter :: PrinterEnv -> (a, PrinterEnv)
                            }


instance MonadState PrinterEnv Printer where
  get = Printer $ \env -> (env, env)
  put env = Printer $ const ((), env)

instance Functor Printer where
  fmap f p = Printer $ \env -> let (a,env') = runPrinter p env
                               in (f a, env')

instance Applicative Printer where
  pure x = Printer $ \env -> (x, env)
  pf <*> pa = Printer $ \env -> let (f, env') = runPrinter pf env
                                    (a, env'') = runPrinter pa env'
                                in (f a, env'')

instance Monad Printer where
  ma >>= f = Printer $ \env -> let (a, env') = runPrinter ma env
                               in runPrinter (f a) env'
instance (Monoid a) => Monoid (Printer a) where
  mempty = pure mempty
  mappend a b = do
    x <- a
    y <- b
    return (x <> y)

instance IsString (Printer ()) where
  fromString s = print (C8.pack s)

nest :: Int -> Printer a -> Printer a
nest n p = Printer $ \env ->
  let (x, env') = runPrinter p (env{level = level env + n})
  in (x, env')

ident :: Printer a -> Printer a
ident = nest 1

newline :: Printer ()
newline = do
  s <- get
  put s { builder = builder s <> "\n"
        , startLine = True}

-- TODO: rewrite this
print :: LB.ByteString -> Printer ()
print bs = state f
  where f env = ((), env')
          where
            env' = env { builder = builder', startLine = False}
            builder' = builder env <> ind <> lazyByteString bs
            ind = if startLine env
                  then tabs (level env)
                  else mempty

tabs :: Int -> Builder
tabs n = mconcat $ replicate n "\t"

printLn :: LB.ByteString -> Printer ()
printLn s = print s >> newline

----------------------------------------------------------------------------------
---- Some combinators
----------------------------------------------------------------------------------

-- | K&R style braces
braces :: Printer () -> Printer ()
braces p = do
  printLn "{"
  newline
  ident p
  print "}"

parens :: Printer () -> Printer ()
parens p = print "(" <> p <> print ")"

spaces :: Printer () -> Printer ()
spaces p = print " " <> p <> print " "

----------------------------------------------------------------------------------
-- Instance definitions
--------------------------------------------------------------------------------
instance PrettyPrint TranslationUnit where
  prettyPrint (TranslationUnit units) =
    forM_ units $ \u -> do
      prettyPrint u
      newline
instance PrettyPrint ExternalDeclaration where
  prettyPrint (ExtDeclarationFunction funDec ) = prettyPrint funDec
  prettyPrint (ExtDeclarationDeclaration dec ) = prettyPrint dec

instance PrettyPrint FunctionDefinition where
  prettyPrint (FunctionDefinition t dec stmt) = printLn "/* function definition */"

instance PrettyPrint Stmt where
  prettyPrint (CompoundStmt stmts)      = braces $ mapM_ prettyPrint stmts
  prettyPrint (ExpressionStmt Nothing)  = print ";"
  prettyPrint (ExpressionStmt (Just e)) = prettyPrint e
  prettyPrint (IfStmt e stmt Nothing)   = print "/* TODO if */"
  prettyPrint Break                     = print "break;"
  prettyPrint Continue                  = print "continue;"
  prettyPrint _                         = print "/* TODO: stmt */"

instance (PrettyPrint a, PrettyPrint b) => PrettyPrint (Either a b) where
  prettyPrint (Left a)  = prettyPrint a
  prettyPrint (Right a) = prettyPrint a

instance PrettyPrint Declaration where
  prettyPrint _ = print "/* TODO: declaration */"


instance PrettyPrint Expr where
  prettyPrint (BExpr op e1 e2) = parens $ mconcat [prettyPrint e1, prettyPrint op, prettyPrint e2]
  prettyPrint (UExpr op e)     = prettyPrint op <> parens (prettyPrint e)
  prettyPrint (ExprIdent i)    = print i
  prettyPrint (Ternary e1 e2 e3) = parens $ prettyPrint e1 <> " ? " <> prettyPrint e2 <> " : " <> prettyPrint e3
  prettyPrint _                = print "/* TODO: expression */"

instance PrettyPrint BOp where
  prettyPrint Mult         = spaces "*"
  prettyPrint Minus        = spaces "-"
  prettyPrint Plus         = spaces "+"
  prettyPrint LessThan     = spaces "<="
  prettyPrint EqualsEquals = spaces "=="
  prettyPrint LOr          = spaces "||"
  prettyPrint LAnd         = spaces "&&"
  prettyPrint NotEqual     = spaces "!="
  prettyPrint AssignOp     = spaces "="

instance PrettyPrint UOp where
  prettyPrint Neg     = "-"
  prettyPrint Deref   = "*"
  prettyPrint Address = "&"
  prettyPrint SizeOf  = "sizeof"
  prettyPrint Not     = "!"

--------------------------------------------------------------------------------
-- This has nothing to do with the rest of PrettyPrinter
-- TODO: find a better place
--------------------------------------------------------------------------------
myParseErrorPretty :: ParseError -> String
myParseErrorPretty e = sourcePosStackPretty (errorPos e) <>
                       ": error: " <>
                       parseErrorTextPretty e
