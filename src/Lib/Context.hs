{-# LANGUAGE OverloadedStrings #-}
module Lib.Context
  ( Context(..)
  , Stack (..)
  , chroot
  , directory
  , fromSteps
  , indent
  , pushContext
  , validate
  ) where

import Control.Monad (when)
import qualified Data.Text as T
import qualified System.Directory as D

import qualified Lib.Spec as S

data Context = Context
  { step :: S.Step
  , children :: [Context]
  } deriving (Show)


newtype Stack  = Stack  { contexts :: [Context] }


chroot :: Stack -> Maybe T.Text
chroot (Stack []) = Nothing
chroot (Stack (Context { step = S.EnterContext _ (S.Chroot dir) } : _)) = Just dir
chroot (Stack (_ : rest)) = chroot $ Stack rest


directory :: Stack  -> T.Text
directory (Stack []) = "."
directory (Stack (Context { step = S.EnterContext _ (S.Directory dir) } : _)) = dir
directory (Stack (_ : rest)) = directory $ Stack rest


fromSteps :: [S.Step] -> IO Context
fromSteps steps = do
  cwd <- D.getCurrentDirectory
  pure Context
    { step = S.EnterContext "build" $ S.Directory $ T.pack cwd
    , children = collectChildren steps
    }
  where
    {- Collect children of current context -}
    collectChildren :: [S.Step] -> [Context]
    {- No child steps to be collected -}
    collectChildren [] = []
    {- Recursively collect children of enter context step -}
    collectChildren (s@(S.EnterContext name _) : remaining) = do
      let (c, remaining') = partitionContextSteps name remaining
      Context
        { step = s
        , children = collectChildren c
        } : collectChildren remaining'
    {- Anything else is just a regular child step -}
    collectChildren (s : remaining) = do
      Context
        { step = s
        , children = []
        } : collectChildren remaining
    {- Partition steps into child steps (same context) and remaining steps -}
    partitionContextSteps :: T.Text -> [S.Step] -> ([S.Step], [S.Step])
    partitionContextSteps name steps' = do
      let (c, remaining) = span (isChildStep name) steps'
      (c, drop 1 remaining)
    {- Check if step is a child step of given context -}
    isChildStep :: T.Text -> S.Step -> Bool
    isChildStep name (S.LeaveContext name') = name /= name'
    isChildStep _ _ = True


indent :: Stack -> String
indent (Stack []) = ""
indent (Stack (_ : rest)) = "  " ++ indent (Stack rest)


pushContext :: Stack -> Context -> Stack
pushContext stack ctx = stack { contexts = ctx : contexts stack }


validate :: Context -> IO ()
validate = validateWithStack $ Stack []


validateWithStack :: Stack -> Context -> IO ()
validateWithStack stack ctx@(Context { step = ctxStep@(S.EnterContext _ _) }) = do
  validateStep stack ctxStep
  let stack' = stack { contexts = ctx : contexts stack }
  validateChildren stack' $ children ctx
validateWithStack stack (Context { step = ctxStep }) =
  validateStep stack ctxStep


validateChildren :: Stack -> [Context] -> IO ()
validateChildren _ [] = pure ()
validateChildren stack (ctx : rest) = do
  validateWithStack stack ctx
  validateChildren stack rest


validateStep :: Stack -> S.Step -> IO ()
validateStep _ (S.DownloadFile {}) = pure ()
validateStep _ (S.EnterContext ctxName (S.Chroot "")) =
  fail $ "Empty chroot directory name in context: " <> T.unpack ctxName
validateStep _ (S.EnterContext ctxName (S.Directory "")) =
  fail $ "Empty directory name in context: " <> T.unpack ctxName
validateStep stack (S.EnterContext ctxName _) =
  let stackContexts = name <$> contexts stack
      name (Context { step = S.EnterContext name' _ }) = name'
      name _ = ""
   in when (ctxName `elem` stackContexts) $
    fail $ "Cannot re-enter context: " <> T.unpack ctxName
validateStep _ (S.LeaveContext ctxName) =
  fail $ "Unexpected leave context step: " <> T.unpack ctxName
validateStep _ (S.RunCommands _) = pure ()
validateStep stack (S.WriteFile filename _) =
  case T.unpack filename of
    '/':_ -> case chroot stack of
      Just _ -> pure ()
      Nothing -> fail $ "Absolute path " ++ T.unpack filename ++ " outside of chroot"
    _ -> pure ()
