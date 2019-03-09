{-# LANGUAGE OverloadedStrings #-}

module Bazel.LabelNode
  ( LabelNode (..)
  , parseLabelNode
  )
  where

import Prelude hiding (concat, takeWhile)

import           Control.Applicative
import           Data.Attoparsec.Text
import           Data.Text (Text, concat, pack)

data LabelNode
  = LabelNode
  { repositoryName :: Maybe Text
  , packageName :: Maybe Text
  , labelName :: Maybe Text
  }
  deriving (Eq, Show)

parseLabelNode :: Text -> Maybe LabelNode
parseLabelNode = either (const Nothing) Just . parseOnly p
  where
    p :: Parser LabelNode
    p = LabelNode <$> repo <*> package <*> name

    -- see:
    -- https://github.com/bazelbuild/bazel/blob/0.23.1/
    --   src/main/java/com/google/devtools/build/lib/cmdline/LabelNodeValidator.java

    repo :: Parser (Maybe Text)
    repo =
      (Nothing <$ string "//") <|>
      (Just . pack <$> (char '@' >> manyTill anyChar (string "//")))

    package :: Parser (Maybe Text)
    package =
      (fmap Just $ takeWhile1 $ inClass "0-9a-zA-Z !\"#$%&'()*+,-./;<=>?@[]^_`{|}~") <|>
      (pure Nothing)

    name :: Parser (Maybe Text)
    name =
      (Nothing <$ endOfInput) <|>
      (Just . concat <$> (char ':' >> manyTill name0 endOfInput))

    name0 :: Parser Text
    name0 = takeWhile1 $ notInClass ":"
