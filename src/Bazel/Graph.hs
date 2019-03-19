{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes     #-}

module Bazel.Graph
  ( main
  )
  where

import           Bazel.Label
import           Bazel.Query.XML
import           Control.Exception
import           Control.Monad.Except
import qualified Data.Graph.Inductive.Graph     as G
import           Data.Graph.Inductive.Graph     hiding (empty)
import           Data.Graph.Inductive.NodeMap
import           Data.Graph.Inductive.PatriciaTree
import           Data.Graph.Inductive.Query.DFS
import           Data.Maybe
import qualified Data.Text.Lazy                 as TL

data Rel
  = Input
  | Output
  deriving (Eq, Show)

data LabelGraph
  = LabelGraph
  { gr :: Gr Label Rel
  , nm :: NodeMap Label
  }

empty :: LabelGraph
empty = LabelGraph G.empty new

addLabel :: Label -> LabelGraph -> LabelGraph
addLabel a g =
  g{ gr = insNodes [n] (gr g), nm = nm' }
  where
    (n, nm') = mkNode (nm g) a

addEdge :: Label -> Rel -> Label -> LabelGraph -> LabelGraph
addEdge a r b g = id
  fromMaybe g $
  (\e -> g{ gr = insEdges [e] (gr g) }) <$> mkEdge (nm g) (a, b, r)

toLabelGraph' :: QueryNode -> Maybe LabelGraph
toLabelGraph' q =
  ($ empty) <$> flg
  where
    flg :: Maybe (LabelGraph -> LabelGraph)
    flg = foldl (.) id <$> traverse frn (ruleNodes q)

    frn :: RuleNode -> Maybe (LabelGraph -> LabelGraph)
    frn rn = do
      label   <- parseLabel          $ ruleNodeName rn
      inputs  <- traverse parseLabel $ ruleInputs   rn
      outputs <- traverse parseLabel $ ruleOutputs  rn
      pure
        $ foldl (.) id (fmap (\l -> addEdge label Input l . addLabel l) inputs)
        . foldl (.) id (fmap (\l -> addEdge l Output label . addLabel l) outputs)
        . addLabel label


toLabelGraph :: QueryNode -> Either SomeException LabelGraph
toLabelGraph = (maybe (Left undefined) Right) . toLabelGraph'

work :: LabelGraph -> IO ()
work lg = do
  putStrLn $ show res
  return ()
  where
    start = fst . mkNode_ (nm lg) $ [qlabel| //tasks:ci |]
    next' :: Context a Rel -> [Node]
    next' (_, _, _, adjOut) =
      map snd . filter ((==Input) . fst) $ adjOut
    label' :: Context a b -> a
    label' (_, _, label, _) = label
    res = xdfsWith next' label' [start] (gr lg)

main :: IO ()
main = do
    e <- runExceptT $ do
      input <- lift       $ getContents
      qn    <- liftEither $ parseQueryNodeText . TL.pack $ input
      lg    <- liftEither $ toLabelGraph qn
      lift $ work lg
      return ()
    case e of
        Left  _   -> putStrLn $ "Error!"
        Right _   -> pure ()
