module Lib.Build
  ( buildImage
  ) where

import Control.Concurrent (myThreadId, throwTo)
import Control.Exception (bracket_)
import qualified Data.Text as T
import System.Posix.Signals

import Lib.Command
import Lib.Context as C
import Lib.Error
import Lib.File as F
import Lib.HTTP
import Lib.Spec as S


buildImage :: Spec -> IO ()
buildImage spec = do
  buildThreadId <- myThreadId
  let handleInterrupt = throwTo buildThreadId BuildInterrupted
      exitSignalSet =
        addSignal sigABRT $ addSignal sigCHLD $ addSignal sigQUIT $ addSignal sigTERM
        emptySignalSet
  _ <- installHandler sigINT (Catch handleInterrupt) $ Just exitSignalSet
  ctx <- fromSteps $ steps spec
  validate ctx
  buildWithStack (Stack []) ctx


buildWithStack :: Stack -> C.Context -> IO ()
buildWithStack stack ctx@(Context s@(EnterContext _ ctxStep ) c) =
  let stack' = pushContext stack ctx
      enterCtx = buildStep stack s
      leaveCtx = handleLeaveContext stack ctxStep
   in bracket_ enterCtx leaveCtx $ mapM_ (buildWithStack stack') c
buildWithStack stack (Context s []) =
  buildStep stack s
buildWithStack _ ctx@(Context _ _) =
  error $ "Unexpected context: " <> show ctx


buildStep :: Stack -> Step -> IO ()
buildStep stack (EnterContext _ ctx) =
  handleEnterContext stack ctx
buildStep stack (DownloadFile filename sha512sum downloadURL) =
  downloadFile stack (T.unpack filename) (T.unpack sha512sum) (T.unpack downloadURL)
buildStep stack (RunCommands (CommandStep cmd)) =
  handleCommands stack cmd
buildStep stack (RunCommands (CommandsStep cmd)) =
  handleCommands stack cmd
buildStep stack (WriteFile filename contents) =
  F.writeFile stack filename contents
buildStep _ (LeaveContext _) =
  error "Unexpected LeaveContext in buildStep"


handleCommands :: Stack -> CommandOrCommands -> IO ()
handleCommands stack (Command cmd) =
  executeCommand stack cmd
handleCommands stack (Commands cmds) =
  mapM_ (handleCommands stack . Command) cmds


handleEnterContext :: Stack -> S.Context -> IO ()
handleEnterContext stack (Chroot dir) =
  putStrLn $ indent stack <> "Entering chroot: " <> T.unpack dir
handleEnterContext stack (Directory dir) =
  putStrLn $ indent stack <> "Entering directory: " <> T.unpack dir
handleEnterContext stack (BracketCommands enterCmds _) =
  handleCommands stack enterCmds


handleLeaveContext :: Stack -> S.Context -> IO ()
handleLeaveContext stack (Chroot dir) =
  putStrLn $ indent stack <> "Leaving chroot: " <> T.unpack dir
handleLeaveContext stack (Directory dir) =
  putStrLn $ indent stack <> "Leaving directory: " <> T.unpack dir
handleLeaveContext stack (BracketCommands _ leaveCmds) =
  handleCommands stack leaveCmds
