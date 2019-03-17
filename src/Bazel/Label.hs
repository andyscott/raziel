{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveLift #-}

module Bazel.Label
  ( Label (..)
  , parseLabel
  , parseLabelQ
  , qlabel
  )
  where

import Prelude hiding (concat)

import           Control.Applicative
import           Data.Ix (inRange)
import           Data.Maybe
import           Data.Text (Text, concat, pack, unpack, strip)
import           Data.Void
import           Data.Hashable
import           Text.Megaparsec hiding (Label)
import           Text.Megaparsec.Char
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import           Language.Haskell.TH.Quote


data Label
  = Label
  { labelRepositoryName :: Maybe Text
  , labelPackageName :: Maybe Text
  , labelName :: Maybe Text
  }
  deriving (Lift, Eq, Ord)

instance Hashable Label where
  hashWithSalt s (Label a b c) =
    s `hashWithSalt`
    a `hashWithSalt`
    b `hashWithSalt` c

instance Show Label where
  show (Label Nothing Nothing Nothing) = "???"
  show (Label (Just r) Nothing Nothing) = unpack $ "@" <> r
  show (Label Nothing Nothing (Just n)) = unpack $ ":" <> n
  show (Label r p n) = unpack $
    (maybe "" (\r' -> "@" <> r') r) <>
    "//" <> (fromMaybe "" p) <> (maybe "" (\n' -> ":" <> n') n)

type Parser = Parsec Void Text

parseLabelQ :: String -> Q Exp
parseLabelQ t =
  maybe (fail $ "unable to parse label " ++ t) lift
  . parseLabel . strip . pack $ t

qlabel :: QuasiQuoter
qlabel
  = QuasiQuoter
  { quoteExp  = parseLabelQ
  , quotePat  = notHandled "patterns"
  , quoteType = notHandled "types"
  , quoteDec  = notHandled "declarations"
  }
  where notHandled things = error $
          things ++ " are not handled by the label quasiquoter."

parseLabel :: Text -> Maybe Label
parseLabel = either (const Nothing) Just . runParser p "string"
  where
    p :: Parser Label
    p = Label <$> repo <*> package <*> name

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


-- orphan needed for lifting Labels
instance Lift Text where
  lift t = [| pack t' |] where
    t' = unpack t
