{-# LANGUAGE OverloadedStrings #-}

module Raziel.Main
  ( main
  )
  where

import Options.Applicative
import Data.Semigroup ((<>))
import Data.Text
import Bazel.Label

data Command
  = Lint
  | Move Label Label

commandMain :: Command -> IO ()

main :: IO ()
main = commandMain =<< execParser opts
  where
    opts :: ParserInfo Command
    opts = info (commandOpts <**> helper)
      (  fullDesc
      <> progDesc "raz"
      <> header "A linter/tool for Bazel"
      )

    commandOpts :: Parser Command
    commandOpts = hsubparser
      (  command "lint" (info lintOpts ( progDesc "lint a build" ))
      <> command "move" (info moveOpts ( progDesc "refactor/move labels" ))
      <> commandGroup "WIP commands:"
      )

    lintOpts :: Parser Command
    lintOpts = pure $ Lint

    moveOpts :: Parser Command
    moveOpts = Move
      <$> argument lbl (metavar "<label-from>")
      <*> argument lbl (metavar "<label-to>")

    lbl :: ReadM Label
    lbl = maybeReader $ parseLabel . pack

commandMain (Move a b) =
  putStrLn $ "move " ++ (show a) ++ " to " ++ (show b)

commandMain _ = putStrLn "This command/arg-combo hasn't been implemented yet"
