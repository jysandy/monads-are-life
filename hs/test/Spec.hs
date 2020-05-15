{-# LANGUAGE OverloadedStrings #-}
module Main
  ( main
  )
where

import           Test.Tasty
import           Test.Tasty.HUnit
import           MonadTest

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup " Tests" [monadTests]
