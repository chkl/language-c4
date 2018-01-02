{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module PrettyPrinter where

import           Prelude                      hiding (concatMap, print, unlines)

import           Control.Monad.State

import qualified Data.ByteString.Lazy         as LB
import           Data.ByteString.Lazy.Builder
import qualified Data.ByteString.Lazy.Char8   as C8
import           Data.Monoid                  ((<>))
import           Data.String                  hiding (unlines)
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
  in (x, env' {level = level env})

noNest :: Printer a -> Printer a
noNest p = Printer $ \env ->
  let (x, env') = runPrinter p (env{level = 0})
  in (x, env' {level = level env})

indent :: Printer a -> Printer a
indent = nest 1


newline :: Printer ()
newline = do
  s <- get
  put s { builder = builder s <> "\n"
        , startLine = True}


intercalate :: Printer () -> [Printer ()] -> Printer ()
intercalate _ []     = return ()
intercalate _ [p]    = p
intercalate s (p:ps) = p >> mapM_ (\x -> s >> x) ps

commaSep :: [Printer ()] -> Printer ()
commaSep = intercalate $ print ", "

unlines :: [Printer ()] -> Printer ()
unlines = intercalate newline


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

prettyPrintLn :: PrettyPrint p => p -> Printer ()
prettyPrintLn p = prettyPrint p >> newline

-- | K&R style braces
braces :: Printer () -> Printer ()
braces p = bracesNN p >> newline

bracesNN :: Printer () -> Printer ()
bracesNN p =  do
  printLn "{"
  indent p
  print "}"

parens :: Printer () -> Printer ()
parens p = print "(" <> p <> print ")"

brackets :: Printer () -> Printer ()
brackets p = print "[" <> p <> print "]"

space, semicolon, eos, period :: Printer ()
space = print " "
semicolon = print ";"
eos = semicolon >> newline -- end of statement
period = print "."

spaces :: Printer () -> Printer ()
spaces p = space <> p <> space

----------------------------------------------------------------------------------
-- Instance definitions
--------------------------------------------------------------------------------
instance PrettyPrint TranslationUnit where
  prettyPrint (TranslationUnit units) = mapM_ prettyPrintLn units

instance PrettyPrint ExternalDeclaration where
  prettyPrint (ExtDeclarationFunction funDec ) = prettyPrint funDec
  prettyPrint (ExtDeclarationDeclaration dec ) = prettyPrint dec

instance PrettyPrint FunctionDefinition where
  prettyPrint (FunctionDefinition t dec stmt) = do
    prettyPrint t
    space
    prettyPrint dec
    newline
    prettyPrint stmt

instance PrettyPrint Type where
  prettyPrint Int = "int"
  prettyPrint Char = "char"
  prettyPrint Void = "void"
  prettyPrint (StructIdentifier i)  = "struct " <> print i
  prettyPrint (StructInline mi decls)  = do
    print "struct"
    maybe "" (\x -> space >> print x) mi
    newline
    bracesNN $ unlines $ map prettyPrint decls

instance PrettyPrint Declarator where
  prettyPrint (Declarator 0 dir) = prettyPrint dir
  prettyPrint (Declarator n dir) = parens $ replicateM_ n (print "*") >> prettyPrint dir

instance PrettyPrint DirectDeclarator where
  prettyPrint (DirectDeclaratorId i) =  print i
  prettyPrint (DirectDeclaratorParens d) =  prettyPrint d
  prettyPrint (DirectDeclaratorParams d ps) =  parens $ prettyPrint d >> parens (commaSep $ map prettyPrint ps)

instance PrettyPrint StructDeclaration where
  prettyPrint (StructDeclaration t decls) = do
    prettyPrint t
    print " "
    commaSep $ map prettyPrint decls
    eos

instance PrettyPrint Parameter where
  prettyPrint (Parameter t dec) = prettyPrint t >> space >> prettyPrint dec
  prettyPrint (AbstractParameter t md) = prettyPrint t >> whenM md prettyPrint

instance PrettyPrint AbstractDec where
  prettyPrint x = print "/* TODO: abstract dec */"

whenM :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenM Nothing _  = return ()
whenM (Just x) f = f x

-- implements this rule I don't completely understand yet... TODO: Investigate
-- omits trailing new line to allow for a potential "else"
smartBraces :: Stmt -> Printer()
smartBraces stmt =
  case stmt of
    (Return _)           -> noBraces
    (LabeledStmt _ _ )   -> noBraces
    (Goto _ )            -> noBraces
    (CompoundStmt stmts) -> space >> bracesNN (unlines $ map prettyPrint stmts)
    _                    -> space >> prettyPrint stmt
    where noBraces = newline >> indent (prettyPrint stmt)

instance PrettyPrint Stmt where
  prettyPrint (LabeledStmt lbl stmt)    = noNest (print lbl >> print ":") >> newline >>  prettyPrint stmt
  prettyPrint (CompoundStmt stmts)      = braces $ mapM_ prettyPrint stmts
  prettyPrint (ExpressionStmt Nothing)  = eos
  prettyPrint (ExpressionStmt (Just e)) = prettyPrint e >> eos
  prettyPrint (IfStmt e stmt Nothing)   = do
    print "if"
    space
    parens $ prettyPrint e
    newline
    smartBraces stmt
    newline

  prettyPrint (IfStmt e stmt1 (Just stmt2))   = do
    print "if" >> space
    parens $ prettyPrint e
    smartBraces stmt1
    space >> print "else"
    smartBraces stmt2
    newline

  prettyPrint (WhileStmt e stmt) = do
    print "while "
    parens $ prettyPrint e
    smartBraces stmt
    newline

  prettyPrint (Goto i) = print "goto " <> print i <> ";"
  prettyPrint (Return me) = do
    print "return"
    whenM me $ \e -> do
      print " "
      prettyPrint e
    eos
  prettyPrint Break                     = print "break" >> eos
  prettyPrint Continue                  = print "continue" >> eos

instance (PrettyPrint a, PrettyPrint b) => PrettyPrint (Either a b) where
  prettyPrint (Left a)  = prettyPrint a
  prettyPrint (Right a) = prettyPrint a

instance PrettyPrint Declaration where
  prettyPrint (Declaration t initDecls) = do
    prettyPrint t
    space
    commaSep $ map prettyPrint initDecls
    eos

instance PrettyPrint InitDeclarator where
  prettyPrint (InitializedDec d Nothing) = prettyPrint d
  prettyPrint (InitializedDec d (Just i))= do
    prettyPrint d
    prettyPrint i

instance PrettyPrint Initializer where
  prettyPrint (InitializerAssignment e) = print " = " >> prettyPrint e
  prettyPrint _ = print "/* TODO: Not implemented yet: InitializerList */"

instance PrettyPrint Expr where
  prettyPrint (BExpr op e1 e2)    = parens $ mconcat [prettyPrint e1, prettyPrint op, prettyPrint e2]
  prettyPrint (UExpr op e)        = parens $ prettyPrint op >> prettyPrint e
  prettyPrint (ExprIdent i)       = print i
  prettyPrint (Ternary e1 e2 e3)  = parens $ prettyPrint e1 <> " ? " <> prettyPrint e2 <> " : " <> prettyPrint e3
  prettyPrint (SizeOfType t)      = parens $ print "sizeof" >> space >> parens (prettyPrint t)
  prettyPrint (Assign l r)        = prettyPrint l >> print " = " >> prettyPrint r
  prettyPrint (Func f a)          = prettyPrint f >> parens (prettyPrint a)
  prettyPrint (Constant c)        = print c
  prettyPrint (Array a b )        = prettyPrint a >> brackets (prettyPrint b)
  prettyPrint (FieldAccess a b)   = parens $ prettyPrint a >> period >> prettyPrint b
  prettyPrint (PointerAccess a b) = parens $ prettyPrint a >> print "->" >> prettyPrint b
  prettyPrint (StringLiteral s)   = print "\"" >> print s >> print "\""
  prettyPrint (List es)           = commaSep $ map prettyPrint es



instance PrettyPrint BOp where
  prettyPrint Mult         = spaces "*"
  prettyPrint Minus        = spaces "-"
  prettyPrint Plus         = spaces "+"
  prettyPrint LessThan     = spaces "<"
  prettyPrint EqualsEquals = spaces "=="
  prettyPrint LOr          = spaces "||"
  prettyPrint LAnd         = spaces "&&"
  prettyPrint NotEqual     = spaces "!="
  prettyPrint AssignOp     = spaces "="

instance PrettyPrint UOp where
  prettyPrint Neg     = "-"
  prettyPrint Deref   = "*"
  prettyPrint Address = "&"
  prettyPrint SizeOf  = "sizeof "
  prettyPrint Not     = "!"

--------------------------------------------------------------------------------
-- This has nothing to do with the rest of PrettyPrinter
-- TODO: find a better place
--------------------------------------------------------------------------------
myParseErrorPretty :: ParseError -> String
myParseErrorPretty e = sourcePosStackPretty (errorPos e) <>
                       ": error: " <>
                       parseErrorTextPretty e
