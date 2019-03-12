{-# LANGUAGE OverloadedStrings #-}

module Bazel.LabelSpec (main) where

import Data.Text
import Test.Tasty
import Test.Tasty.HUnit

import Bazel.Label

main :: IO ()
main = defaultMain unitTests

unitTests :: TestTree
unitTests = testGroup "Label" [showTests, parseLabelTests]

showTests :: TestTree
showTests = testGroup "show"
  [ check (Label  Nothing       Nothing          Nothing)     "???"
  , check (Label  Nothing      (Just "foo/bar")  Nothing)     "//foo/bar"
  , check (Label  Nothing       Nothing         (Just "baz")) ":baz"
  , check (Label  Nothing      (Just "foo/bar") (Just "baz")) "//foo/bar:baz"
  , check (Label (Just "zedd")  Nothing          Nothing)     "@zedd"
  , check (Label (Just "zedd") (Just "foo/bar")  Nothing)     "@zedd//foo/bar"
  , check (Label (Just "zedd")  Nothing         (Just "baz")) "@zedd//:baz"
  , check (Label (Just "zedd") (Just "foo/bar") (Just "baz")) "@zedd//foo/bar:baz"
  ]
  where
    check :: Label -> String -> TestTree
    check label expected = testCase ("show " ++ expected) $ (show label) @?= expected

parseLabelTests :: TestTree
parseLabelTests = testGroup "parseLabel" [yays, nays]
  where
    yays = testGroup "valid"
      [ yay "//foo/bar" $ Label Nothing (Just "foo/bar") Nothing
      , yay "//foo/bar:baz" $ Label Nothing (Just "foo/bar") (Just "baz")
      , yay "@zip//foo/bar" $ Label (Just "zip") (Just "foo/bar") Nothing
      , yay "@zip//foo/bar:zap" $ Label (Just "zip") (Just "foo/bar") (Just "zap")
      , yay "@shellcheck//:bin/shellcheck" $ Label (Just "shellcheck") Nothing (Just "bin/shellcheck")
      , yay "@io_company_x//src/scala/com/zedd:util" $ Label (Just "io_company_x") (Just "src/scala/com/zedd") (Just "util")
      ]

    nays = testGroup "invalid"
      [ nay "//foo:bar:baz"
      , nay "@hello/world"
      ]

    check :: Text -> Maybe Label -> TestTree
    check raw expected = testCase ("parseLabel " ++ (show raw)) $ parseLabel raw @?= expected

    yay raw = check raw . Just
    nay raw = check raw Nothing
