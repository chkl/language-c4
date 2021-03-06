{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Language.C4.PrettyPrinter where

import           Prelude                 hiding (print, unlines)

import           Control.Monad.State

import qualified Data.ByteString         as BS
import           Data.ByteString.Builder
import qualified Data.ByteString.Char8   as C8
import           Data.ByteString.Lazy    (toStrict)
import           Data.ByteString.Short   (fromShort)
import           Data.Monoid             ((<>))
import           Data.String             hiding (unlines)
import           System.IO               (Handle, stdout)
import           Text.Megaparsec.Error   hiding (ParseError)

import           Language.C4.Ast
import           Language.C4.Types

type Level = Int


class PrettyPrint a where
  prettyPrint :: a -> Printer ()


  toPrettyString :: a -> BS.ByteString
  toPrettyString x = toStrict $ toLazyByteString (prettyBuilder x)

  hPutPrettyPrint :: a -> Handle -> IO ()
  hPutPrettyPrint a h = hPutBuilder h (prettyBuilder a)

  putPrettyPrint :: a ->  IO ()
  putPrettyPrint a = hPutPrettyPrint a stdout

  prettyBuilder   :: a -> Builder
  prettyBuilder x = builder $ snd $ runPrinter (prettyPrint x) defaultPrinterEnv

  {-# MINIMAL (prettyPrint) #-}

defaultPrinterEnv :: PrinterEnv

defaultPrinterEnv = PrinterEnv 0 True mempty


-- | describes the current environment with a level of intendation, whether it is the start of a line
--   and a Builder.
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

-- | indentates a Printer by n tabulators, TODO
nest :: Int -> Printer a -> Printer a
nest n p = Printer $ \env ->
  let (x, env') = runPrinter p (env{level = level env + n})
  in (x, env' {level = level env})

-- | TODO
noNest :: Printer a -> Printer a
noNest p = Printer $ \env ->
  let (x, env') = runPrinter p (env{level = 0})
  in (x, env' {level = level env})

-- | puts indentation of one level.
indent :: Printer a -> Printer a
indent = nest 1


-- | "smart" newline: only inserts a new line if the cursor is not the first position in a line.
newline :: Printer ()
newline = do
  b <- gets startLine
  unless b $ do
    modify $ \s -> s { builder = builder s <> "\n" , startLine = True }

-- | "smart" empty line: inserts two new line iff the cursor is not at the beginning in a line.
emptyLine :: Printer ()
emptyLine = do
  at0 <- gets startLine
  if at0
    then modify $ \s -> s {builder = builder s <> "\n", startLine = True}
    else modify $ \s -> s {builder = builder s <> "\n\n", startLine = True}

-- | intercalate p lp takes intersperses p in between elements of lp.
intercalate :: Printer () -> [Printer ()] -> Printer ()
intercalate _ []     = return ()
intercalate _ [p]    = p
intercalate s (p:ps) = p >> mapM_ (\x -> s >> x) ps

-- | puts commas in between elements of the given list.
commaSep :: [Printer ()] -> Printer ()
commaSep = intercalate $ print ", "

-- | puts new lines in between elements of the given list.
unlines :: [Printer ()] -> Printer ()
unlines = intercalate newline


-- TODO: rewrite this
print :: BS.ByteString -> Printer ()
print = print' . byteString

-- | TODO
print' :: Builder -> Printer ()
print' bs = state f
  where f env = ((), env')
          where
            env' = env { builder = builder', startLine = False}
            builder' = builder env <> ind <> bs
            ind = if startLine env
                  then tabs (level env)
                  else mempty

-- | tabs n prints a number of n tabulators.
tabs :: Int -> Builder
tabs n = mconcat $ replicate n "\t"

printLn :: BS.ByteString -> Printer ()
printLn s = print s >> newline

----------------------------------------------------------------------------------
---- Some combinators
----------------------------------------------------------------------------------

-- | prettyPrintLn p invokes prettyPrint p and adds a new line.
prettyPrintLn :: PrettyPrint p => p -> Printer ()
prettyPrintLn p = prettyPrint p >> newline

-- | prints K&R style braces.
braces :: Printer () -> Printer ()
braces p = bracesNN p >> newline

-- | prints braces without new line at the end.
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

instance PrettyPrint (TranslationUnit x) where
  prettyPrint (TranslationUnit _ units) = intercalate "\n" (map prettyPrint units)

instance PrettyPrint (FunctionDefinition x) where
  prettyPrint (FunctionDefinition _ t dec stmt) = do
    prettyPrint t
    space
    prettyPrint dec
    newline
    prettyPrint stmt

instance PrettyPrint (Type x) where
  prettyPrint (Int _) = "int"
  prettyPrint (Char _) = "char"
  prettyPrint (Void _) = "void"
  prettyPrint (StructIdentifier _ i)  = "struct " <> print (fromShort i)
  prettyPrint (StructInline _ mi decls)  = do
    print "struct"
    maybe "" (\x -> space >> print x) (fromShort <$> mi)
    newline
    bracesNN $ unlines $ map prettyPrint decls

instance PrettyPrint (Declarator x) where
  prettyPrint (IndirectDeclarator _ dir) = parens $ print "*" >> prettyPrint dir
  prettyPrint (DeclaratorId _ i) =  print (fromShort i)
  prettyPrint (FunctionDeclarator _ d ps) =  parens $ prettyPrint d >> parens (commaSep $ map prettyPrint ps)
  prettyPrint (ArrayDeclarator _ d e) = parens $ prettyPrint d >> brackets (prettyPrint e)


instance PrettyPrint (StructDeclaration x) where
  prettyPrint (StructDeclaration _ t decls) = do
    prettyPrint t
    print " "
    commaSep $ map prettyPrint decls
    eos

instance PrettyPrint (Parameter x) where
  prettyPrint (Parameter _ t dec) = prettyPrint t >> space >> prettyPrint dec
  prettyPrint (AbstractParameter _ t md) = prettyPrint t >> whenM md prettyPrint

instance PrettyPrint (AbstractDeclarator x) where
  prettyPrint (IndirectAbstractDeclarator _ dir) = parens $ print "*" >> prettyPrint dir
  prettyPrint (AbstractTerminal _) = return () -- don't do anything
  prettyPrint _ = print "/* TODO: complicated Abstract declarators */"


-- | TODO
whenM :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenM Nothing _  = return ()
whenM (Just x) f = f x

-- TODO: Investigate
-- | prints braces with regard to the printed element before, e.g. if(-).
smartBraces :: Stmt x -> Printer()
smartBraces stmt =
  case stmt of
    (CompoundStmt _ stmts) -> space >> bracesNN (unlines $ map prettyPrint stmts)
    (Return _ _)           -> noBraces
    (LabeledStmt _ _ _ )   -> noBraces
    (Continue _ )          -> noBraces
    (Break _ )             -> noBraces
    (Goto _ _ )            -> noBraces
    (IfStmt _ _ _ _ )      -> withBraces
    (WhileStmt _ _ _ )     -> withBraces
    (ExpressionStmt _ _)   -> withBraces
  where noBraces = newline >> indent (prettyPrint stmt)
        withBraces = space >> prettyPrint stmt

instance PrettyPrint (Stmt x) where
  prettyPrint (LabeledStmt _ lbl stmt)    = noNest (print (fromShort lbl) >> print ":") >> newline >>  prettyPrint stmt
  prettyPrint (CompoundStmt _ stmts)      = braces $ mapM_ prettyPrint stmts
  prettyPrint (ExpressionStmt _ e) = prettyPrint e >> eos
  prettyPrint (IfStmt _ e stmt Nothing)   = do
    print "if"
    space
    parens $ prettyPrint e
    newline
    smartBraces stmt
    newline

  prettyPrint (IfStmt _ e stmt1 (Just stmt2))   = do
    print "if" >> space
    parens $ prettyPrint e
    smartBraces stmt1
    at0 <- gets startLine
    if at0
      then print "else"
      else print " else"
    smartBraces stmt2
    newline

  prettyPrint (WhileStmt _ e stmt) = do
    print "while "
    parens $ prettyPrint e
    smartBraces stmt
    newline

  prettyPrint (Goto _ i) = print "goto " >> print (fromShort i) >> eos
  prettyPrint (Return _ me) = do
    print "return"
    whenM me $ \e -> do
      print " "
      prettyPrint e
    eos
  prettyPrint (Break _)  = print "break" >> eos
  prettyPrint (Continue _) = print "continue" >> eos

instance (PrettyPrint a, PrettyPrint b) => PrettyPrint (Either a b) where
  prettyPrint (Left a)  = prettyPrint a
  prettyPrint (Right a) = prettyPrint a

instance PrettyPrint (Declaration x) where
  prettyPrint (Declaration _ t initDecls) = do
    prettyPrint t
    space
    commaSep $ map prettyPrint initDecls
    eos

instance PrettyPrint (InitDeclarator x) where
  prettyPrint (InitializedDec d Nothing) = prettyPrint d
  prettyPrint (InitializedDec d (Just i))= do
    prettyPrint d
    prettyPrint i

instance PrettyPrint (Initializer x) where
  prettyPrint (InitializerAssignment e) = print " = " >> prettyPrint e
--   prettyPrint _ = print "/* TODO: Not implemented yet: InitializerList */"


instance PrettyPrint (Expr x) where
  prettyPrint (BExpr _ op e1 e2)    = parens $ mconcat [prettyPrint e1, prettyPrint op, prettyPrint e2]
  prettyPrint (UExpr _ op e)        = parens $ prettyPrint op >> prettyPrint e
  prettyPrint (ExprIdent _ i)       = print (fromShort i)
  prettyPrint (Ternary _ e1 e2 e3)  = parens $ prettyPrint e1 <> " ? " <> prettyPrint e2 <> " : " <> prettyPrint e3
  prettyPrint (SizeOfType _ t)      = parens $ print "sizeof" >> parens (prettyPrint t)
  prettyPrint (Assign _ l r)        = prettyPrint l >> print " = " >> prettyPrint r
  prettyPrint (Func _ f a)          = parens $ prettyPrint f >> parens (prettyPrint a)
  prettyPrint (IntConstant _ c)     = print' (integerDec c)
  prettyPrint (CharConstant _ c)    = print "'" >> print c >> print "'"
  prettyPrint (ArrayAccess _ a b )  = prettyPrint a >> brackets (prettyPrint b)
  prettyPrint (FieldAccess _ a b)   = parens $ prettyPrint a >> period >> print (fromShort b)
  prettyPrint (PointerAccess _ a b) = parens $ prettyPrint a >> print "->" >> print (fromShort b)
  prettyPrint (StringLiteral _ s)   = print "\"" >> print s >> print "\""
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

-- | prints usual error messages.
myParseErrorPretty :: ParseError -> String
myParseErrorPretty e = sourcePosStackPretty (errorPos e) <>
                       ": error: " <>
                       parseErrorTextPretty e
