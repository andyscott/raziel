{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE DeriveGeneric      #-}
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
  , GeneratorNode (..)
  )
  where

import           Control.Exception
import           Control.Monad
import           Control.Monad.Catch
import           GHC.Generics
import           Data.Monoid.Generic
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
  , ruleGenerator :: Maybe GeneratorNode
  }
  deriving (Eq, Show)

data GeneratorNode
  = GeneratorNode
  { generatorName :: Text
  , generatorFunction :: Text
  , generatorLocation :: Text
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


data RuleAttributeState
  = RuleAttributeState
  { _ruleInputs :: [Text]
  , _ruleOutputs :: [Text]
  , _generatorName :: Maybe Text
  , _generatorFunction :: Maybe Text
  , _generatorLocation :: Maybe Text
  }
  deriving (Generic, Eq)
  deriving Semigroup via GenericSemigroup RuleAttributeState
  deriving Monoid    via GenericMonoid RuleAttributeState

parseRuleNodeElement :: Element -> Either SomeException RuleNode
parseRuleNodeElement e =
  RuleNode
  <$> lookupAttr "name" attrs
  <*> lookupAttr "class" attrs
  <*> lookupAttr "location" attrs
  <*> fmap _ruleInputs innerData
  <*> fmap _ruleOutputs innerData
  <*> fmap (\z -> GeneratorNode
        <$> (_generatorName z)
        <*> (_generatorFunction z)
        <*> (_generatorLocation z)) innerData
  where
    attrs = elementAttributes e
    innerData = parseChildren $ elementNodes e

    parseChildren :: [Node] -> Either SomeException RuleAttributeState
    parseChildren =
      foldrM (\n s ->
               case n of
                 NodeElement e' -> case (unpack $ nameLocalName $ elementName e') of
                   "rule-input" ->
                     (\v -> s { _ruleInputs = v : _ruleInputs s }) <$>
                     (lookupAttr "name" $ elementAttributes e')
                   "rule-output" ->
                     (\v -> s { _ruleOutputs = v : _ruleOutputs s }) <$>
                     (lookupAttr "name" $ elementAttributes e')
                   "string" ->
                     let attrs' = elementAttributes e'
                     in case lookupAttr "name" attrs' of
                       Right "generator_name" ->
                         (\v -> s { _generatorName = Just v }) <$>
                         (lookupAttr "value" attrs')
                       Right "generator_function" ->
                         (\v -> s { _generatorFunction = Just v }) <$>
                         (lookupAttr "value" attrs')
                       Right "generator_location" ->
                         (\v -> s { _generatorLocation = Just v }) <$>
                         (lookupAttr "value" attrs')
                       _ -> pure s

                   _ -> pure s
                 _ -> pure s
            ) mempty
