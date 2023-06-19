module Smarrow.CLI.LibMain where

import Data.ByteString (ByteString)
import Options.Applicative

import Smarrow.Deploy.Codec (readShowCodec)
import Smarrow.Deploy.Config (SMId, displaySMId)
import Smarrow.Deploy.HttpClient (call_, newClient, spawn, upgrade)
import Smarrow.Parser (parseFile_, parseValue_)
import Smarrow.PrettyPrint
import Smarrow.Translate
import Smarrow.AST.Value

------------------------------------------------------------------------

data Options = Options
  { oCommand :: Command
  }

data Command
  = Check   CheckOptions
  | Deploy  DeployOptions
  | Invoke  InvokeOptions
  | Upgrade UpgradeOptions

data CheckOptions = CheckOptions FilePath

data DeployOptions = DeployOptions SMId FilePath String

data InvokeOptions = InvokeOptions SMId String ByteString

data UpgradeOptions = UpgradeOptions SMId String FilePath FilePath FilePath

------------------------------------------------------------------------

libMain :: IO ()
libMain = go . oCommand =<< execParser infoOpts
  where
    go :: Command -> IO ()
    go (Check   copts) = checkCmd   copts
    go (Deploy  dopts) = deployCmd  dopts
    go (Invoke  iopts) = invokeCmd  iopts
    go (Upgrade uopts) = upgradeCmd uopts

    checkCmd (CheckOptions fp) = do
      _expr <- parseFile_ fp
      putStrLn ("Succuessfully checked: " ++ fp)

    deployCmd (DeployOptions smid fp host) = do
      expr <- parseFile_ fp
      let ccc = translate expr
      c <- newClient host readShowCodec
      let initState = IntV 0 -- XXX: make this part of the source code?
      spawn c smid ccc initState
      putStrLn ("Deployed: " ++ displaySMId smid)

    invokeCmd (InvokeOptions smid host input) = do
      c <- newClient host readShowCodec
      input' <- parseValue_ input
      output <- call_ c smid input'
      putStrLn (prettyValue output)

    upgradeCmd (UpgradeOptions smid host oldCode newCode stateMigration) = do
      c <- newClient host readShowCodec
      oldCode' <- translate <$> parseFile_ oldCode
      newCode' <- translate <$> parseFile_ newCode
      stateMigration' <- translate <$> parseFile_ stateMigration
      upgrade c smid oldCode' newCode' stateMigration'
      putStrLn ("Upgraded: " ++ displaySMId smid)

infoOpts :: ParserInfo Options
infoOpts = info (opts <**> helper)
  ( fullDesc
  -- <> progDesc "Command-line interface for the Smarrow language"
  <> header "smarrow - the command-line interface for the Smarrow environment" )

opts :: Parser Options
opts = Options <$> hsubparser
  (  command "check"   (info (Check   <$> checkOpts)   (progDesc "Check a state machine"))
  <> command "deploy"  (info (Deploy  <$> deployOpts)  (progDesc "Deploy a state machine"))
  <> command "invoke"  (info (Invoke  <$> invokeOpts)  (progDesc "Invoke a state machine"))
  <> command "upgrade" (info (Upgrade <$> upgradeOpts) (progDesc "Upgrade a state machine")) )

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

upgradeOpts :: Parser UpgradeOptions
upgradeOpts = UpgradeOptions
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
        (  metavar "FILE"
        <> help "The old source file" )
  <*> strArgument
        (  metavar "FILE"
        <> help "The new source file" )
  <*> strArgument
        (  metavar "FILE"
        <> help "The state migration source file" )

-- XXX: simpleVersioner
