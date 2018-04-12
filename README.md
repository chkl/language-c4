# A compiler in Haskell for (a subset of) C 
*Because writing a compiler in C is just boring* 

(TODO: Turn this into a nice blog post)

## Parsing (the nice part)

Let's talk about the good parts first. The parser is fairly straight-forward,
using megaparsec, working with the C specification documents is bothersome
though, so please don't expect that it works on more complicated files. Also,
there's no preprocessor.

## Semantic Analysis (the cool part)

For the type checking part we really wanted to be able to annotate our AST at
specific places with type annotations. We had no idea how to achieve that in a
'nice' way, but `#haskell` to the rescue! Someone pointed us to the "Trees That
Grow" paper, and it described exactly the problem we had encountered too.

So, a `e :: Expr SynPhase` (an expression after parsing) contains a `SourcePos`
whereas a `e :: Expr SemPhase`, contains a `Type`. Nice!

## Code Generation (the ugly part)
We used `llvm-hs-pure` to generate some LLVM code. The whole Codegen module is
more of a last-minute result and utterly incorrect.
