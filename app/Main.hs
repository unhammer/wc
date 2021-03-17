{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}
module Main where

import           Data.Foldable
import qualified Data.HashMap.Strict as M
import qualified Data.HashTable      as H
import           System.Environment
import           System.Exit
import           System.IO
import           Text.Printf
import           Types

import           FileSplit
import           FileSplitUTF
import           HandleSplitUTF
import           InlinedBSFold
import           InlinedMonoidBSFold
import           Lazy
import           LazyUTFAgnostic
import           MonoidBSFold
import           Parallel
import           Simple
import           SimpleBSFold
import           SimpleFold
import           Streaming
import           StreamingUTF
import           Strict
import           Stupid

printResult (name, Counts{charCount, wordCount, lineCount}) = printf "%d %d %d %s\n" lineCount (getFlux wordCount) charCount name

main :: IO ()
main = do
    results <- getArgs >>= \case
        ("handle-utf": filenames) -> handleSplitUTF filenames
        ("lazy": filenames) -> lazyBytestream filenames
        ("lazy-utf": filenames) -> lazyUTF8 filenames
        ("simple-bs-fold": filenames) -> simpleBSFold filenames
        ("monoid-bs-fold": filenames) -> monoidBSFold filenames
        ("inlined-monoid-bs-fold": filenames) -> inlinedMonoidBSFold filenames
        ("inlined-bs-fold": filenames) -> inlinedBSFold filenames
        ("stupid": filenames) -> stupid filenames
        ("simple": filenames) -> simple filenames
        ("simple-fold": filenames) -> simpleFold filenames
        ("strict": filenames) -> strictBytestream filenames
        ("parallel": filenames) -> parallelBytestream filenames
        ("streaming": filenames) -> streamingBytestream filenames
        ("streaming-utf": filenames) -> streamingUTF filenames
        ("split": filenames) -> filesplit filenames
        ("split-utf": filenames) -> filesplitUTF filenames
        ("hm": filename:_) -> do
          s <- hm filename
          mapM_ print (M.toList s)
          return []
        ("ht": filename : _) -> do
          s <- ht filename
          kv <- H.readAssocsIO s
          mapM_ print kv
          return []
        _ -> hPutStrLn stderr "usage: <simple|lazy> [files...]" >> exitFailure
    traverse_ printResult results
