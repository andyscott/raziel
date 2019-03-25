{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveLift #-}

module Bazel.Label
  ( Label
  , parseLabel
  , maybeLabel
  , parseLabelQ
  , qlabel
  )
  where

import Prelude hiding (concat)

import           Control.Applicative hiding (many)
import           Control.Exception
import           Control.Monad.Catch
import           Data.Bifunctor
import           Data.Hashable
import           Data.Ix (inRange)
import           Data.Maybe
import           Data.Text (Text, concat, pack, unpack, strip)
import           Data.Typeable
import           Data.Void
import           Text.Megaparsec hiding (Label)
import           Text.Megaparsec.Char
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import           Language.Haskell.TH.Quote

newtype Label = Label RawLabel
  deriving (Lift, Eq, Ord)

instance Show Label where
  show l @ (Label rl) = case rl of
    RawLabelRPN r p n -> unpack $ "@" <> r <> "//" <> p <> ":" <> n
    RawLabelR   r     -> unpack $ "@" <> r
    RawLabelP     p   -> unpack $             "//" <> p
    RawLabelN       n -> unpack $                          ":" <> n
    RawLabelRP  r p   -> unpack $ "@" <> r <> "//" <> p
    RawLabelRN  r   n -> unpack $ "@" <> r <> "//" <>      ":" <> n
    RawLabelPN    p n -> unpack $             "//" <> p <> ":" <> n

data LabelException
  = UnableToParseLabel (ParseErrorBundle Text Void)
  | EmptyLabelException Text
  deriving (Show, Typeable)

instance Exception LabelException

labelRepositoryName :: Label -> Maybe Text
labelRepositoryName (Label rl) = rawLabelRepositoryName rl

labelPacakgeName :: Label -> Maybe Text
labelPacakgeName (Label rl) = rawLabelPackageName rl

labelName :: Label -> Text
labelName (Label rl) = case rl of
  RawLabelRPN _ _ n -> n
  RawLabelR   r     -> r
  RawLabelP     p   -> lastPackageComponent p
  RawLabelN       n -> n
  RawLabelRP  _ p   -> lastPackageComponent p
  RawLabelRN  _   n -> n
  RawLabelPN    _ n -> n

reifyName :: Label -> Label
reifyName l @ (Label rl) = case rl of
  RawLabelRPN _ _ n -> l
  RawLabelR   r     -> Label $ RawLabelRN r $ r
  RawLabelP     p   -> Label $ RawLabelPN p $ lastPackageComponent p
  RawLabelN       n -> l
  RawLabelRP  r p   -> Label $ RawLabelRPN r p $ lastPackageComponent p
  RawLabelRN  _   n -> l
  RawLabelPN    _ n -> l

data RawLabel
  = RawLabelRPN Text Text Text
  | RawLabelR   Text
  | RawLabelP        Text
  | RawLabelN             Text
  | RawLabelRP  Text Text
  | RawLabelRN  Text      Text
  | RawLabelPN       Text Text
  deriving (Lift, Eq, Ord)

rawLabelRepositoryName :: RawLabel -> Maybe Text
rawLabelRepositoryName rl = case rl of
  RawLabelRPN r _ _ -> Just r
  RawLabelR   r     -> Just r
  RawLabelP     _   -> Nothing
  RawLabelN       _ -> Nothing
  RawLabelRP  r _   -> Just r
  RawLabelRN  r _   -> Just r
  RawLabelPN    _ _ -> Nothing

rawLabelPackageName :: RawLabel -> Maybe Text
rawLabelPackageName rl = case rl of
  RawLabelRPN _ p _ -> Just p
  RawLabelR   _     -> Nothing
  RawLabelP     p   -> Just p
  RawLabelN       _ -> Nothing
  RawLabelRP  _ p   -> Just p
  RawLabelRN  _   _ -> Nothing
  RawLabelPN    p _ -> Just p

rawLabelName :: RawLabel -> Maybe Text
rawLabelName rl = case rl of
  RawLabelRPN _ _ n -> Just n
  RawLabelR   _     -> Nothing
  RawLabelP     _   -> Nothing
  RawLabelN       n -> Just n
  RawLabelRP  _ _   -> Nothing
  RawLabelRN  _   n -> Just n
  RawLabelPN    _ n -> Just n


packageComponents :: Text -> [Text]
packageComponents t = case runParser p "string" t of
  Left e   -> [t]
  Right cs -> cs
  where
    p :: Parser [Text]
    p =
      (:)
      <$> component
      <*> many (char '/' >> component)
    component :: Parser Text
    component = takeWhile1P Nothing (/= '/')

lastPackageComponent :: Text -> Text
lastPackageComponent = last . packageComponents

type Parser = Parsec Void Text

maybeLabel :: Maybe Text -> Maybe Text -> Maybe Text -> Maybe Label
maybeLabel r p n = Label <$> maybeRawLabel r p n

maybeRawLabel :: Maybe Text -> Maybe Text -> Maybe Text -> Maybe RawLabel
maybeRawLabel (Just r) (Just p) (Just n) = Just $ RawLabelRPN r p n
maybeRawLabel (Just r)  Nothing  Nothing = Just $ RawLabelR   r
maybeRawLabel  Nothing (Just p)  Nothing = Just $ RawLabelP     p
maybeRawLabel  Nothing  Nothing (Just n) = Just $ RawLabelN       n
maybeRawLabel (Just r) (Just p)  Nothing = Just $ RawLabelRP  r p
maybeRawLabel (Just r)  Nothing (Just n) = Just $ RawLabelRN  r   n
maybeRawLabel  Nothing (Just p) (Just n) = Just $ RawLabelPN    p n
maybeRawLabel  Nothing  Nothing  Nothing = Nothing

parseLabel :: Text -> Either SomeException Label
parseLabel l = Label <$> parseRawLabel l

parseLabelQ :: String -> Q Exp
parseLabelQ t =
  either (\e -> fail $ "unable to parse label " ++ t ++ " due to " ++ (show e)) lift
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

parseRawLabel :: Text -> Either SomeException RawLabel
parseRawLabel l = case runParser p "string" l of
  Left e          -> throwM $ UnableToParseLabel e
  Right (Just v)  -> Right v
  Right (Nothing) -> throwM $ EmptyLabelException l
  where
    p :: Parser (Maybe RawLabel)
    p = maybeRawLabel <$> repo <*> package <*> name

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
