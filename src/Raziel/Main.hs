{-# LANGUAGE OverloadedStrings #-}

module Raziel.Main
  ( main
  )
  where

import           Bazel.Label
import           Bazel.LabelGraph
import           Bazel.Workspace
import           Bazel.Query.XML
import           Control.Exception
import           Control.Monad.Except
import           Data.Bifunctor
import           Data.Semigroup ((<>))
import           Data.Text
import qualified Data.Text.Lazy as TL
import           Data.These
import           Options.Applicative
import           System.Directory (getCurrentDirectory)
import           System.IO
import           System.Process hiding (cwd)
import           System.FilePath (makeRelative, takeFileName, takeDirectory)

data Command
  = Lint
  | Move Label Label (Maybe Text)
  | Stats
  | FileToLabel String
  | LabelToFile Label

commandMain :: Command -> ExceptT SomeException IO ()

main :: IO ()
main = do
  e <- runExceptT . commandMain =<< execParser opts
  case e of
    Left  _   -> putStrLn $ "Error!"
    Right _   -> pure ()
  where
    opts :: ParserInfo Command
    opts = info (commandOpts <**> helper)
      (  fullDesc
      <> progDesc "raz"
      <> header "A linter/tool for Bazel"
      )

    commandOpts :: Parser Command
    commandOpts
      = subparser
      ( commandGroup "big commands:"
        <> command "lint"   ( info lintOpts
                            ( progDesc "lint a build" ))
        <> command "move"   ( info moveOpts
                            ( progDesc "refactor/move labels" ))
        <> command "stats"  ( info statsOpts
                            ( progDesc "calculate stats from the build event stream" ))
      )
      <|> subparser
      ( commandGroup "quick conversions:"
        <> command "l2f"    ( info l2fOpts
                            ( progDesc "convert a label to a file" ))
        <> command "f2l"    ( info f2lOpts
                            ( progDesc "convert a file to a label" ))
      )

    lintOpts :: Parser Command
    lintOpts = pure $ Lint

    moveOpts :: Parser Command
    moveOpts = Move
      <$> argument lbl ( metavar "<label-from>" )
      <*> argument lbl ( metavar "<label-to>" )
      <*> optional ( strOption
                     $  long "query-file"
                     <> short 'f'
                     <> metavar "<query-file>"
                     <> help "input query" )

    statsOpts :: Parser Command
    statsOpts = pure $ Stats

    l2fOpts :: Parser Command
    l2fOpts = LabelToFile
      <$> argument lbl ( metavar "<label>" )

    f2lOpts :: Parser Command
    f2lOpts = FileToLabel
      <$> argument str ( metavar "<file>" )

    lbl :: ReadM Label
    lbl = eitherReader $ first show . parseLabel . pack

bazelQuery :: Workspace -> String -> IO String
bazelQuery ws q = do
  (_, Just hout, _, _) <- createProcess
    (proc (bazel ws) ["query", q, "--output=xml" ]){ std_out = CreatePipe }
  hGetContents hout

commandMain (Move a b _) = do
  qs <- liftIO $ do
    cwd <- getCurrentDirectory
    ws  <- resolveWorkspace cwd
    bazelQuery ws "//..."
  q  <- liftEither $ parseQueryNodeText . TL.pack $ qs
  lg <- liftEither $ toLabelGraph q
  liftIO $ putStrLn $ "move " ++ (show a) ++ " to " ++ (show b)
  liftIO $ putStrLn $ "q: " ++ (show $ rdeps a lg)
  return ()

commandMain (FileToLabel file) = do
  ws <- liftIO $ resolveWorkspace file
  let rel = makeRelative (root ws) file
  liftIO . putStrLn . prettyString . newRelativeLabel
    $ These (pack . takeDirectory $ rel) (pack . takeFileName $ rel)

commandMain _ = do
  liftIO $ putStrLn "This command/arg-combo hasn't been implemented yet"
