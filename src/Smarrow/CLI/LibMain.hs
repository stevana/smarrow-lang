module Smarrow.CLI.LibMain where

import Data.ByteString (ByteString)
import Options.Applicative
import System.FilePath (takeDirectory, (<.>), (</>))

import Smarrow.AST
import Smarrow.Deploy.Codec (readShowCodec)
import Smarrow.Deploy.Config (SMId, displaySMId)
import Smarrow.Deploy.HttpClient (call_, newClient, spawn, upgrade)
import Smarrow.Environment
import Smarrow.Interpreter
import Smarrow.Parser (parseFile_, parseValue_)
import Smarrow.PrettyPrint
import Smarrow.Translate

------------------------------------------------------------------------

data Options = Options
  { oCommand :: Command
  }

data Command
  = Check   CheckOptions
  | Deploy  DeployOptions
  | Invoke  InvokeOptions

data CheckOptions = CheckOptions FilePath

data DeployOptions = DeployOptions SMId FilePath String

data InvokeOptions = InvokeOptions SMId String ByteString

------------------------------------------------------------------------

libMain :: IO ()
libMain = go . oCommand =<< execParser infoOpts
  where
    go :: Command -> IO ()
    go (Check  copts) = checkCmd  copts
    go (Deploy dopts) = deployCmd dopts
    go (Invoke iopts) = invokeCmd iopts

    checkCmd (CheckOptions fp) = do
      _expr <- parseFile_ fp
      putStrLn ("Succuessfully checked: " ++ fp)

    deployCmd (DeployOptions smid fp host) = do
      machine <- parseFile_ fp
      c <- newClient host readShowCodec
      case machineRefines machine of
        Nothing -> do
          let env       = extendEnvLangRecord defaultEnv
                            (ldTypes (machineLanguage machine))
                            (sdType (machineState machine))
              core      = translate env (machineFunction machine)
              initState = evalType (sdType (machineState machine))
          spawn c smid core initState (machineLanguage machine)
          putStrLn ("Deployed: " ++ displaySMId smid)
        Just refinee -> do
          oldMachine <- parseFile_ (takeDirectory fp </> machineNameString refinee <.> "smarr")
          let oldEnv  = extendEnvLangRecord defaultEnv (ldTypes (machineLanguage oldMachine))
                                                       (sdType (machineState oldMachine))
              newEnv  = extendEnvLangRecord defaultEnv (ldTypes (machineLanguage machine))
                                                       (sdType (machineState machine))
              oldCode = translate oldEnv (machineFunction oldMachine)
              newCode = translate newEnv (machineFunction machine)

              stateMigration = translate defaultEnv ReturnE -- XXX
          upgrade c smid oldCode newCode stateMigration (machineLanguage machine)
          putStrLn ("Upgraded: " ++ displaySMId smid)

    invokeCmd (InvokeOptions smid host input) = do
      c <- newClient host readShowCodec
      input' <- parseValue_ input
      output <- call_ c smid input'
      putStrLn (prettyValue output)

infoOpts :: ParserInfo Options
infoOpts = info (opts <**> helper)
  ( fullDesc
  -- <> progDesc "Command-line interface for the Smarrow language"
  <> header "smarrow - the command-line interface for the Smarrow environment" )

opts :: Parser Options
opts = Options <$> hsubparser
  (  command "check"  (info (Check  <$> checkOpts)  (progDesc "Check a state machine"))
  <> command "deploy" (info (Deploy <$> deployOpts) (progDesc "Deploy a state machine"))
  <> command "invoke" (info (Invoke <$> invokeOpts) (progDesc "Invoke a state machine"))
  )

checkOpts :: Parser CheckOptions
checkOpts = CheckOptions
  <$> strArgument
        (  metavar "FILE"
        <> help "The source file")

deployOpts :: Parser DeployOptions
deployOpts = DeployOptions
  <$> strArgument
        (  metavar "SMID"
        <> help "The state machine id" )
  <*> strArgument
        (  metavar "FILE"
        <> help "The source file" )
  <*> strOption
        (  long "host"
        <> short 'h'
        <> metavar "HOST"
        <> help "The host URI"
        <> value "http://localhost:8080" )

invokeOpts :: Parser InvokeOptions
invokeOpts = InvokeOptions
  <$> strArgument
        (  metavar "SMID"
        <> help "The state machine id" )
  <*> strOption
        (  long "host"
        <> short 'h'
        <> metavar "HOST"
        <> help "The host URI"
        <> value "http://localhost:8080" )
  <*> strArgument
        (  metavar "INPUT"
        <> help "The input for the state machine" )
