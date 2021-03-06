-- |
-- Module      :  Language.C4
-- Copyright   :  © 2017–2018 Robert Schenck, Felix Stutz, Christian Klinger
-- Stability   :  experimental
--
-- TODO: Add a short summary of the project here
--
-- This modules re-exports the high-level functions

module Language.C4
  ( analyse
  , tokenize
  , parse
  , module Language.C4.PrettyPrinter
  , module Language.C4.Types
  , module Language.C4.Codegen
  , TranslationUnit
  , SemPhase
  , SynPhase
  ) where

import           Language.C4.Analysis      (analyse)
import           Language.C4.Ast
import           Language.C4.Ast.SemAst    (SemPhase)
import           Language.C4.Ast.SynAst    (SynPhase)
import           Language.C4.Codegen
import           Language.C4.Lexer         (tokenize)
import           Language.C4.Parser        (parse)
import           Language.C4.PrettyPrinter
import           Language.C4.Types
