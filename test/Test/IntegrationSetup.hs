module Test.IntegrationSetup
  ( bootstrapIntegration,
  )
where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, finally, try)
import System.Directory (createDirectoryIfMissing, doesFileExist, getHomeDirectory)
import System.Environment (getEnvironment, lookupEnv, setEnv)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath ((</>))
import System.IO (Handle, IOMode (AppendMode), hClose, hPutStrLn, openFile)
import System.Process

data ManagedProcess = ManagedProcess
  { mpHandle :: ProcessHandle
  , mpLogHandle :: Maybe Handle
  }

bootstrapIntegration :: IO (IO ())
bootstrapIntegration = do
  ensureVenv
  ensureAdaUp

  adaupNetwork <- maybe "devnet" id <$> lookupEnv "KUBER_TEST_ADAUP_NETWORK"
  enableRemoteClient <- remoteClientEnabled
  home <- getHomeDirectory
  let networkMagic = adaupNetworkMagic adaupNetwork
      socketPath = home </> ".cardano" </> adaupNetwork </> "node.socket"
      remoteUrl = "http://127.0.0.1:8081/"

  setEnv "NETWORK" networkMagic
  setEnv "CARDANO_NODE_SOCKET_PATH" socketPath
  setEnv "KUBER_REMOTE_URL" remoteUrl

  nodeProcess <- ensureNodeRunning adaupNetwork socketPath networkMagic
  serverProcess <-
    if enableRemoteClient
      then ensureServerRunning socketPath networkMagic
      else pure Nothing

  pure $ do
    stopManagedProcess serverProcess
    stopManagedProcess nodeProcess

ensureVenv :: IO ()
ensureVenv = do
  home <- getHomeDirectory
  let venvPython = home </> ".venv" </> "bin" </> "python"
  exists <- doesFileExist venvPython
  if exists
    then pure ()
    else runChecked "python3" ["-m", "venv", home </> ".venv"]

ensureAdaUp :: IO ()
ensureAdaUp = do
  home <- getHomeDirectory
  let pipPath = home </> ".venv" </> "bin" </> "pip"
  installed <- succeeds pipPath ["show", "adaup"]
  if installed
    then pure ()
    else runChecked pipPath ["install", "-e", "./adaup"]

ensureNodeRunning :: String -> FilePath -> String -> IO (Maybe ManagedProcess)
ensureNodeRunning adaupNetwork socketPath networkMagic = do
  ready <- nodeReady socketPath networkMagic
  if ready
    then do
      waitForNodeSetup adaupNetwork socketPath networkMagic
      pure Nothing
    else do
      home <- getHomeDirectory
      let cardanoPath = home </> ".venv" </> "bin" </> "cardano"
      processHandle <- startNodeProcess adaupNetwork cardanoPath ["node", adaupNetwork]
      waitForNodeSetup adaupNetwork socketPath networkMagic
      pure $ Just processHandle

ensureServerRunning :: FilePath -> String -> IO (Maybe ManagedProcess)
ensureServerRunning socketPath networkMagic = do
  ready <- serverReady
  if ready
    then pure Nothing
    else do
      processHandle <-
        startProcessWithEnv
          [ ("NETWORK", networkMagic),
            ("CARDANO_NODE_SOCKET_PATH", socketPath)
          ]
          "cabal"
          ["run", "exe:kuber-server"]
      waitFor "kuber-server" 120 serverReady
      pure $ Just processHandle

nodeReady :: FilePath -> String -> IO Bool
nodeReady socketPath networkMagic = do
  home <- getHomeDirectory
  let cliPath = home </> ".cardano" </> "bin" </> "cardano-cli"
  cliExists <- doesFileExist cliPath
  if cliExists
    then succeeds cliPath $ ["query", "tip", "--socket-path", socketPath] ++ networkCliArgs networkMagic
    else pure False

nodeSetupComplete :: String -> FilePath -> String -> IO Bool
nodeSetupComplete adaupNetwork socketPath networkMagic = do
  home <- getHomeDirectory
  let paymentAddrPath = home </> ".cardano" </> "keys" </> "payment.addr"
  keysReady <- case adaupNetwork of
    "devnet" -> doesFileExist paymentAddrPath
    _ -> pure True
  if not keysReady
    then pure False
    else nodeReady socketPath networkMagic

waitForNodeSetup :: String -> FilePath -> String -> IO ()
waitForNodeSetup adaupNetwork socketPath networkMagic = do
  waitFor "cardano node setup" 240 (nodeSetupComplete adaupNetwork socketPath networkMagic)
  -- adaup devnet keeps bootstrapping wallet material just after the socket comes up.
  threadDelay 5000000

serverReady :: IO Bool
serverReady = succeeds "curl" ["-fsS", "http://127.0.0.1:8081/api/v3/chain-point"]

networkCliArgs :: String -> [String]
networkCliArgs "mainnet" = ["--mainnet"]
networkCliArgs networkMagic = ["--testnet-magic", networkMagic]

adaupNetworkMagic :: String -> String
adaupNetworkMagic "mainnet" = "mainnet"
adaupNetworkMagic "preview" = "2"
adaupNetworkMagic "preprod" = "1"
adaupNetworkMagic "testnet" = "42"
adaupNetworkMagic "devnet" = "42"
adaupNetworkMagic other = other

remoteClientEnabled :: IO Bool
remoteClientEnabled = do
  value <- lookupEnv "KUBER_ENABLE_REMOTE_CLIENT"
  pure $ maybe True (`notElem` ["0", "false", "no", "off"]) value

waitFor :: String -> Int -> IO Bool -> IO ()
waitFor label attempts action = do
  success <- poll attempts
  if success
    then pure ()
    else error $ "Timed out waiting for " ++ label
  where
    poll remaining
      | remaining <= 0 = pure False
      | otherwise = do
          ready <- action
          if ready
            then pure True
            else threadDelay 1000000 >> poll (remaining - 1)

startProcess :: FilePath -> [String] -> IO ManagedProcess
startProcess cmd args = startProcessWithEnv [] cmd args

startNodeProcess :: String -> FilePath -> [String] -> IO ManagedProcess
startNodeProcess network cmd args = do
  logDir <- pure "test-reports"
  createDirectoryIfMissing True logDir
  let logPath = logDir </> ("cardano-node-" ++ network ++ ".log")
  logHandle <- openFile logPath AppendMode
  hPutStrLn logHandle $ "\n=== starting " ++ unwords (cmd : args) ++ " ==="
  baseEnv <- getEnvironment
  (_, _, _, handle) <-
    createProcess
      (proc cmd args)
        { env = Just baseEnv
        , std_out = UseHandle logHandle
        , std_err = UseHandle logHandle
        }
  pure $ ManagedProcess handle (Just logHandle)

startProcessWithEnv :: [(String, String)] -> FilePath -> [String] -> IO ManagedProcess
startProcessWithEnv envVars cmd args = do
  baseEnv <- getEnvironment
  (_, _, _, handle) <-
    createProcess
      (proc cmd args)
        { env = Just (mergeEnv envVars baseEnv),
          std_out = Inherit,
          std_err = Inherit
        }
  pure $ ManagedProcess handle Nothing

stopManagedProcess :: Maybe ManagedProcess -> IO ()
stopManagedProcess Nothing = pure ()
stopManagedProcess (Just (ManagedProcess handle logHandle)) = do
  status <- getProcessExitCode handle
  case status of
    Nothing -> terminateProcess handle `finally` waitForProcess handle >> closeManagedHandles
    Just _ -> closeManagedHandles
  where
    closeManagedHandles = maybe (pure ()) hClose logHandle

mergeEnv :: [(String, String)] -> [(String, String)] -> [(String, String)]
mergeEnv overrides base = overrides ++ filter (\(key, _) -> key `notElem` keys) base
  where
    keys = map fst overrides

succeeds :: FilePath -> [String] -> IO Bool
succeeds cmd args = do
  result <- try (readProcessWithExitCode cmd args "") :: IO (Either SomeException (ExitCode, String, String))
  pure $ case result of
    Right (ExitSuccess, _, _) -> True
    Right _ -> False
    Left _ -> False

runChecked :: FilePath -> [String] -> IO ()
runChecked cmd args = do
  result <- readProcessWithExitCode cmd args ""
  case result of
    (ExitSuccess, _, _) -> pure ()
    (_, stdoutText, stderrText) ->
      error $
        "Command failed: "
          ++ unwords (cmd : args)
          ++ "\nstdout:\n"
          ++ stdoutText
          ++ "\nstderr:\n"
          ++ stderrText
