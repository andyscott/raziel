{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes     #-}

module Bazel.LabelGraph
  ( LabelGraph (..)
  , toLabelGraph
  , rdeps
  )
  where

import           Bazel.Label
import           Bazel.Query.XML
import           Control.Exception
import           Control.Monad.Catch
import           Control.Monad.Except
import           Data.Foldable
import qualified Data.Graph.Inductive.Graph     as G
import           Data.Graph.Inductive.Graph     hiding (empty)
import           Data.Graph.Inductive.NodeMap
import           Data.Graph.Inductive.PatriciaTree
import           Data.Graph.Inductive.Query.DFS
import           Data.Typeable
import           Data.Text                      hiding (empty, filter, map)


import Control.Monad.State.Lazy

data Rel
  = Input
  | Output
  deriving (Eq, Show)

data LabelGraph
  = LabelGraph
  { gr :: Gr Label Rel
  , nm :: NodeMap Label
  }
  deriving (Show)

empty :: LabelGraph
empty = LabelGraph G.empty new

data LabelGraphException
  = UnableToParseLabel Text
  | FailedToCreateEdge Label Rel Label
  deriving (Show, Typeable)

instance Exception LabelGraphException

toLabelGraph :: QueryNode -> Either SomeException LabelGraph
toLabelGraph qn =
  snd <$> runStateT (traverse_ frn $ ruleNodes qn) empty
  where
    parseLabel' :: Text -> Either SomeException Label
    parseLabel' t = maybe (throwM $ UnableToParseLabel t) Right $ parseLabel t

    addLabel :: Label -> StateT LabelGraph (Either SomeException) ()
    addLabel l = modify f
      where
        f g =
          if gelem (fst n) (gr g) then g
          else g{ gr = insNodes [n] (gr g), nm = nm' }
          where
            (n, nm') = mkNode (nm g) l

    addEdge :: Label -> Rel -> Label -> StateT LabelGraph (Either SomeException) ()
    addEdge a r b =
      modify (\lg -> lg{ gr = insMapEdge (nm lg) (a, b, r) (gr lg) })

    frn :: RuleNode -> StateT LabelGraph (Either SomeException) ()
    frn rn = do
      label    <- lift $ parseLabel'          $ ruleNodeName rn
      inputs   <- lift $ traverse parseLabel' $ ruleInputs   rn
      outputs  <- lift $ traverse parseLabel' $ ruleOutputs  rn
      addLabel label
      traverse_ addLabel inputs
      traverse_ addLabel outputs
      traverse_ (\l -> addEdge label Input  l    ) inputs
      traverse_ (\l -> addEdge l     Output label) outputs

rdeps :: Label -> LabelGraph -> [Label]
rdeps l lg = do
  xdfsWith next' label' [start] (gr lg)
  where
    start = fst . mkNode_ (nm lg) $ l
    next' :: Context a Rel -> [Node]
    next' (_, _, _, adjOut)
      = res
      where
        res = map snd . filter ((==Input) . fst) $ adjOut
    label' :: Context a b -> a
    label' (_, _, label, _) = label
