{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Lib.Steps
  ( fromSpec
  , Step(..)
  , Steps(..)
  ) where

import Data.Text

import Lib.Context
import Lib.Spec


data Step
  = RunCommand { command :: [Text] }
  | RunCommands { commands :: [[Text]] }
  | WriteFile { file :: Text, content :: Text }
  | DownloadFile { file :: Text, url :: Text }
  | EnterContext { enter :: Context }
  | LeaveContext { leave :: Context }
  deriving (Show)


newtype Steps = Steps [Step]


fromSpec :: Spec -> IO Steps
fromSpec spec = stepsFromSpec (contexts spec) (steps spec) >>= verifyContextStack


stepsFromSpec :: [Context] -> [SpecStep] -> IO Steps
stepsFromSpec _ [] = pure $ Steps []
stepsFromSpec contexts (step:steps) = do
  step' <- stepFromSpec contexts step
  Steps steps' <- stepsFromSpec contexts steps
  pure $ Steps $ step':steps'


stepFromSpec :: [Context] -> SpecStep -> IO Step
stepFromSpec _ (Lib.Spec.RunCommand command) =
  pure $ Lib.Steps.RunCommand command
stepFromSpec _ (Lib.Spec.RunCommands commands) =
  pure $ Lib.Steps.RunCommands commands
stepFromSpec _ (Lib.Spec.WriteFile file content) =
  pure $ Lib.Steps.WriteFile file content
stepFromSpec _ (Lib.Spec.DownloadFile file url) =
  pure $ Lib.Steps.DownloadFile file url
stepFromSpec contexts (Lib.Spec.EnterContext name) =
  case Lib.Steps.EnterContext <$> contextFromSpec contexts name of
    Just step -> pure step
    Nothing -> missingContext name
stepFromSpec contexts (Lib.Spec.LeaveContext name) =
  case Lib.Steps.LeaveContext <$> contextFromSpec contexts name of
    Just step -> pure step
    Nothing -> missingContext name


contextFromSpec :: [Context] -> Text -> Maybe Context
contextFromSpec contexts name =
  Prelude.lookup name $ Prelude.map (\context ->
    (contextName context, context)) contexts


missingContext :: Text -> a
missingContext name = error $ "Context " ++ show name ++ " not found"


verifyContextStack :: Steps -> IO Steps
verifyContextStack (Steps steps) = do
  verifyContextStack' [] steps
  pure $ Steps steps
  where
    verifyContextStack' :: [Context] -> [Step] -> IO ()
    verifyContextStack' _ [] = pure ()
    verifyContextStack' contexts (step:steps) =
      case step of
        Lib.Steps.EnterContext enter ->
          verifyContextStack' (enter:contexts) steps
        Lib.Steps.LeaveContext leave ->
          case contexts of
            [] -> error $
              "Context stack is empty, cannot leave context " ++
                unpack (contextName leave)
            (context:contexts) ->
              if context == leave
                then verifyContextStack' contexts steps
                else error $
                  "Cannot leave context " ++ unpack (contextName leave) ++
                    " in context " ++ unpack (contextName context)
        _ -> verifyContextStack' contexts steps
