{-# LANGUAGE TemplateHaskell #-}

module Spec.Analysis (testSemanticSamples) where

import           Control.Monad   (forM_)
import           Data.ByteString (ByteString)
import           Data.FileEmbed
import           Data.List       (isSuffixOf)
import           Data.Monoid     ((<>))
import           Test.Hspec

import           Language.C4

testSemanticSamples :: SpecWith ()
testSemanticSamples = do
  let files = $(embedDir "assets/test/analysis/") :: [(FilePath, ByteString)]
      posFiles = filter (\(fn,_) -> ".positive.c" `isSuffixOf` fn) files
      negFiles = filter (\(fn,_) -> ".negative.c" `isSuffixOf` fn) files
  describe "testing the semantic analysis with positive samples" $
    forM_ posFiles $ \(fnA, a) -> do
      it ("parses " <> fnA <> " and should accept it after of semantic analysis") $ do
        case runC4 (parse fnA a) of
          Left err  -> expectationFailure ("parser should have accepted file " <> fnA <> ", instead: " <> show err)
          Right ast -> case runC4 (analyse ast) of
                         Left semErr  ->  expectationFailure ("semantic analysis should have accepted file " <> fnA <> ", instead: " <> show semErr)
                         Right _ -> return ()
  describe "testing the semantic analysis with negative samples" $
    forM_ negFiles $ \(fnA, a) -> do
      it ("parses " <> fnA <> " and should reject it because of semantic analysis") $ do
        case runC4 (parse fnA a) of
          Left err  -> expectationFailure ("parser should have accepted file " <> fnA <> ", instead: " <> show err)
          Right ast -> case runC4 (analyse ast) of
                         Left _ ->  return ()
                         Right _ -> expectationFailure ("semantic analysis should have rejected file " <> fnA)

