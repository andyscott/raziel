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

module Bazel.Query.Output
  ( Output (..)
  , RuleNode (..)
  , parseRuleNodeDocument
  , parseRuleNodeText
  )
  where

import           Control.Exception
import           Control.Applicative
import           Control.Monad
import           Control.Monad.Catch
import           Data.Maybe
import           Data.Typeable                (Typeable)
import           Data.Text
import qualified Data.Text.Lazy               as TL
import qualified Data.Map.Strict              as Map
import           Data.Map.Strict              (Map)
import           Text.XML

data Output
  = Output [RuleNode]
  deriving (Eq, Show)

data RuleNode
  = RuleNode
  { ruleNodeName :: Text
  , ruleNodeClass :: Text
  , ruleNodeLocation :: Text
  }
  deriving (Eq, Show)

data OutputException
  = UnexpectedElement Element
  | MissingAttribute Text
  deriving (Show, Typeable)

instance Exception OutputException

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
  where attrs = elementAttributes e
