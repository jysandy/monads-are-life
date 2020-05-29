{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import           MonadTest
import           Test.Tasty
import           Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup " Tests" [monadTests]
