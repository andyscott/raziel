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

module Bazel.Query.XML
  ( Result (..)
  , RuleNode (..)
  , parseRuleNodeDocument
  , parseRuleNodeText
  )
  where

import           Control.Exception
import           Control.Monad
import           Control.Monad.Catch
import           Data.Maybe
import           Data.Foldable
import           Data.Typeable                (Typeable)
import           Data.Text hiding (foldr)
import qualified Data.Text.Lazy               as TL
import qualified Data.Map.Strict              as Map
import           Data.Map.Strict              (Map)
import           Text.XML

data Result
  = Result [RuleNode]
  deriving (Eq, Show)

data RuleNode
  = RuleNode
  { ruleNodeName :: Text
  , ruleNodeClass :: Text
  , ruleNodeLocation :: Text
  , ruleInputs :: [Text]
  , ruleOutputs :: [Text]
  }
  deriving (Eq, Show)

data ResultException
  = UnexpectedElement Element
  | MissingAttribute Text
  deriving (Show, Typeable)

instance Exception ResultException

parseRuleNodeText :: TL.Text -> Either SomeException RuleNode
parseRuleNodeText = parseText def >=> parseRuleNodeDocument

parseRuleNodeDocument :: Document -> Either SomeException RuleNode
parseRuleNodeDocument = (ensureIsRuleNodeElement >=> parseRuleNodeElement) . documentRoot
  where
    ensureIsRuleNodeElement e = case (unpack $ nameLocalName $ elementName e) of
      "rule" -> Right e
      _      -> throwM $ UnexpectedElement e

lookupAttr :: Name -> Map Name Text -> Either SomeException Text
lookupAttr name attrs =
  fromMaybe (throwM $ MissingAttribute (nameLocalName name)) (Right <$> Map.lookup name attrs)

parseRuleNodeElement :: Element -> Either SomeException RuleNode
parseRuleNodeElement e =
  RuleNode
  <$> (lookupAttr "name" attrs)
  <*> (lookupAttr "class" attrs)
  <*> (lookupAttr "location" attrs)
  <*> ((\ ~(i, _) -> i) <$> parsedNodes)
  <*> ((\ ~(_, o) -> o) <$> parsedNodes)
  where
    attrs = elementAttributes e
    parsedNodes = parseNodesM $ elementNodes e

    parseNodesM :: [Node] -> Either SomeException ([Text], [Text])
    parseNodesM =
      foldrM (\n ~(i, o) ->
               case n of
                 NodeElement e' -> case (unpack $ nameLocalName $ elementName e') of
                   "rule-input" -> (\i' -> (i' : i, o)) <$> (lookupAttr "name" $ elementAttributes e')
                   "rule-output" -> (\o' -> (i, o' : o)) <$> (lookupAttr "name" $ elementAttributes e')
                   _ -> pure (i, o)
                 _ -> pure (i, o)
            ) mempty
