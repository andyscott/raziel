{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Bazel.Query.XMLSpec (main) where

import Data.Text.Lazy
import Data.Maybe
import Test.Tasty
import Test.Tasty.HUnit

import Text.RawString.QQ

import Bazel.Query.XML

main :: IO ()
main = defaultMain unitTests

checkRuleNode :: Text -> Maybe RuleNode -> TestTree
checkRuleNode raw expected = testCase ("parseRuleNodeText " ++ (show expected)) $
  case parseRuleNodeText raw of
    Left e -> assertBool (show e) (isNothing expected)
    Right v -> case expected of
      Just expected' -> v @?= expected'
      Nothing -> assertFailure $ "unexpected result " ++ (show v)

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ yay [r|
          <rule class="haskell_import" location="/foo/BUILD:39:5" name="//foo:bar">
          </rule>
          |]
      $ RuleNode "//foo:bar" "haskell_import" "/foo/BUILD:39:5" [] [] Nothing

  , yay [r|
          <rule class="shellcheck_test" location="/raziel/tools/BUILD:3:1" name="//tools:bazel@shellcheck">
            <string name="name" value="bazel@shellcheck"/>
            <list name="srcs">
              <label value="//tools:bazel"/>
            </list>
            <rule-input name="//tools:bazel"/>
            <rule-input name="@bazel_tools//tools/test:runtime"/>
            <rule-input name="@bazel_tools//tools/test:test_setup"/>
            <rule-input name="@bazel_tools//tools/test:test_wrapper"/>
            <rule-output name="//tools:bazel@shellcheck-bin"/>
          </rule>
          |]
      $ RuleNode
        "//tools:bazel@shellcheck"
        "shellcheck_test"
        "/raziel/tools/BUILD:3:1"
        [ "//tools:bazel"
        , "@bazel_tools//tools/test:runtime"
        , "@bazel_tools//tools/test:test_setup"
        , "@bazel_tools//tools/test:test_wrapper"
        ]
        [ "//tools:bazel@shellcheck-bin"
        ]
        Nothing

  , yay [r|
          <rule class="haskell_test" location="/raziel/src/Bazel/Query/BUILD:21:1" name="//src/Bazel/Query:OutputSpec">
            <string name="generator_name" value="OutputSpec"/>
            <string name="generator_function" value="haskell_test"/>
            <string name="generator_location" value="src/Bazel/Query/BUILD:21"/>
          </rule>
        |]
      $ RuleNode
        "//src/Bazel/Query:OutputSpec"
        "haskell_test"
        "/raziel/src/Bazel/Query/BUILD:21:1"
        []
        []
        (Just $ GeneratorNode "OutputSpec" "haskell_test" "src/Bazel/Query/BUILD:21")
  ]
  where
    yay raw = checkRuleNode raw . Just
