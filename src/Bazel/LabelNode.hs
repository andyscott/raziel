{-# LANGUAGE OverloadedStrings #-}

module Bazel.LabelNode
  ( LabelNode (..)
  , parseLabelNode
  )
  where

import Prelude hiding (concat)

import           Control.Applicative
import           Data.Ix (inRange)
import           Data.Text (Text, concat, pack)
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char

data LabelNode
  = LabelNode
  { repositoryName :: Maybe Text
  , packageName :: Maybe Text
  , labelName :: Maybe Text
  }
  deriving (Eq, Show)


type Parser = Parsec Void Text

parseLabelNode :: Text -> Maybe LabelNode
parseLabelNode = either (const Nothing) Just . runParser p "string"
  where
    p :: Parser LabelNode
    p = LabelNode <$> repo <*> package <*> name

    -- see:
    -- https://github.com/bazelbuild/bazel/blob/0.23.1/
    --   src/main/java/com/google/devtools/build/lib/cmdline/LabelValidator.java

    repo :: Parser (Maybe Text)
    repo =
      (Nothing <$ string "//") <|>
      (Just . pack <$> (char '@' >> manyTill anySingle (string "//")))

    package :: Parser (Maybe Text)
    package =
      (fmap Just $ takeWhile1P Nothing packageChar) <|>
      (pure Nothing)

    name :: Parser (Maybe Text)
    name =
      (Nothing <$ eof) <|>
      (Just . concat <$> (char ':' >> manyTill name0 eof))

    name0 :: Parser Text
    name0 = takeWhile1P Nothing (/= ':')


-- bazel says: "0-9a-zA-Z !\"#$%&'()*+,-./;<=>?@[]^_`{|}~"
packageChar :: Char -> Bool
packageChar c = case c of
  '!'  -> True
  '"'  -> True
  '#'  -> True
  '$'  -> True
  '%'  -> True
  '&'  -> True
  '\'' -> True
  '('  -> True
  ')'  -> True
  '*'  -> True
  '+'  -> True
  ','  -> True
  '-'  -> True
  '.'  -> True
  '/'  -> True
  ';'  -> True
  '<'  -> True
  '='  -> True
  '>'  -> True
  '?'  -> True
  '@'  -> True
  '['  -> True
  ']'  -> True
  '^'  -> True
  '_'  -> True
  '`'  -> True
  '{'  -> True
  '|'  -> True
  '}'  -> True
  '~'  -> True
  _ | inRange ('A', 'Z') c -> True
  _ | inRange ('a', 'z') c -> True
  _ | inRange ('0', '9') c -> True
  _                        -> False
