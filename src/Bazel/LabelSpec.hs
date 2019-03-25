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
  [ check (label  Nothing      (Just "foo/bar")  Nothing)     "//foo/bar"
  , check (label  Nothing       Nothing         (Just "baz")) ":baz"
  , check (label  Nothing      (Just "foo/bar") (Just "baz")) "//foo/bar:baz"
  , check (label (Just "zedd")  Nothing          Nothing)     "@zedd"
  , check (label (Just "zedd") (Just "foo/bar")  Nothing)     "@zedd//foo/bar"
  , check (label (Just "zedd")  Nothing         (Just "baz")) "@zedd//:baz"
  , check (label (Just "zedd") (Just "foo/bar") (Just "baz")) "@zedd//foo/bar:baz"
  ]
  where
    check :: Label -> String -> TestTree
    check l expected = testCase ("show " ++ expected) $ (show l) @?= expected

parseLabelTests :: TestTree
parseLabelTests = testGroup "parseLabel" [yays, nays]
  where
    yays = testGroup "valid"
      [ yay "//foo/bar" $ label Nothing (Just "foo/bar") Nothing
      , yay "//foo/bar:baz" $ label Nothing (Just "foo/bar") (Just "baz")
      , yay "@zip//foo/bar" $ label (Just "zip") (Just "foo/bar") Nothing
      , yay "@zip//foo/bar:zap" $ label (Just "zip") (Just "foo/bar") (Just "zap")
      , yay "@shellcheck//:bin/shellcheck" $ label (Just "shellcheck") Nothing (Just "bin/shellcheck")
      , yay "@io_company_x//src/scala/com/zedd:util" $ label (Just "io_company_x") (Just "src/scala/com/zedd") (Just "util")
      ]

    nays = testGroup "invalid"
      [ nay "//foo:bar:baz"
      , nay "@hello/world"
      ]

    check :: Text -> Maybe Label -> TestTree
    check raw expected
      = testCase ("parseLabel " ++ (show raw))
      $ (either (const Nothing) Just . parseLabel) raw @?= expected

    yay raw = check raw . Just
    nay raw = check raw Nothing

label :: Maybe Text -> Maybe Text -> Maybe Text -> Label
label r p n = case maybeLabel r p n of
  Just l  -> l
  Nothing -> undefined
