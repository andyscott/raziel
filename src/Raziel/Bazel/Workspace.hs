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

module Raziel.Bazel.Workspace
  ( WorkspacePath
  , loadWorkspacePath
  , findWorkspacePath
  , BazelPath
  , resolveBazelPath
  , Workspace(..)
  , resolveWorkspace
  ) where

import           Control.Applicative
import qualified Control.Monad.Catch          as E
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Maybe
import           Data.List.Split
import           Data.Foldable
import           Data.Maybe
import           Data.Typeable
import           System.Directory
import qualified System.Environment           as Env
import           System.FilePath

data Workspace
  = Workspace
    { root :: WorkspacePath
    , bazel :: BazelPath
    }
    deriving (Show)


resolveWorkspace :: MonadIO io => FilePath -> io Workspace
resolveWorkspace fp = do
  root' <- findWorkspacePath fp
  bazel' <- resolveBazelPath root'
  return Workspace
    { root = root'
    , bazel = bazel'
    }


type WorkspacePath = String


newtype WorkspaceNotFoundException = WorkspaceNotFoundException { workspaceNotFoundMessage :: String }
    deriving (Show, Typeable)

instance E.Exception WorkspaceNotFoundException


findWorkspacePath :: MonadIO io => FilePath -> io WorkspacePath
findWorkspacePath path = liftIO $ do
  ws <- loadWorkspacePath path
  case ws of
    Nothing ->
      let path' = takeDirectory path
      in if path == path'
         then E.throwM $ WorkspaceNotFoundException "could not locate bazel workspace"
         else findWorkspacePath path'
    Just ws' -> return ws'


loadWorkspacePath :: MonadIO io => FilePath -> io (Maybe WorkspacePath)
loadWorkspacePath path = liftIO $ do
  exists <- doesFileExist $ path ++ "/WORKSPACE"
  return (
    if exists then Just path
    else Nothing)

type BazelPath = String

newtype BazelNotFoundException
  = BazelNotFoundException { bazelNotFoundMessage :: String }
  deriving (Show, Typeable)

instance E.Exception BazelNotFoundException

resolveBazelPath :: MonadIO io => WorkspacePath -> io BazelPath
resolveBazelPath ws = liftIO $
  fromMaybe (E.throwM $ BazelNotFoundException "bazel runner not found for workspace")
  <$> ( runMaybeT $
        workspaceRootBazel <|>
        workspaceToolsBazel <|>
        systemBazel )
  where
    workspaceRootBazel = resolve $ ws ++ "/bazel"
    workspaceToolsBazel = resolve $ ws ++ "/tools/bazel"

    systemBazel :: MaybeT IO BazelPath
    systemBazel =
      fmap (splitOn ":") (MaybeT $ Env.lookupEnv "PATH")
      >>=  (asum . fmap (\s -> resolve $ s ++ "/bazel"))

    resolve :: String -> MaybeT IO BazelPath
    resolve s =
      let
        f :: String -> Bool -> Maybe BazelPath
        f s' True = Just $ s'
        f _ _    = Nothing
      in
        MaybeT $ (f s) <$> (doesFileExist s)
