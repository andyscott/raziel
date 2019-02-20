{-# LANGUAGE OverloadedStrings #-}

module Bazel.LabelNodeSpec (main) where

import Data.Text
import Test.Tasty
import Test.Tasty.HUnit

import Bazel.LabelNode

main :: IO ()
main = defaultMain unitTests

unitTests = testGroup "parseLabelNode" [yays, nays]
  where
    yays = testGroup "valid"
      [ yay "//foo/bar" $ LabelNode Nothing (Just "foo/bar") Nothing
      , yay "//foo/bar:baz" $ LabelNode Nothing (Just "foo/bar") (Just "baz")
      , yay "@zip//foo/bar" $ LabelNode (Just "zip") (Just "foo/bar") Nothing
      , yay "@zip//foo/bar:zap" $ LabelNode (Just "zip") (Just "foo/bar") (Just "zap")
      , yay "@shellcheck//:bin/shellcheck" $ LabelNode (Just "shellcheck") Nothing (Just "bin/shellcheck")
      , yay "@io_company_x//src/scala/com/zedd:util" $ LabelNode (Just "io_company_x") (Just "src/scala/com/zedd") (Just "util")
      ]

    nays = testGroup "invalid"
      [ nay "//foo:bar:baz"
      , nay "@hello/world"
      ]

    check :: Text -> Maybe LabelNode -> TestTree
    check raw expected = testCase ("parseLabelNode " ++ (show raw)) $ parseLabelNode raw @?= expected

    yay raw = check raw . Just
    nay raw = check raw Nothing
