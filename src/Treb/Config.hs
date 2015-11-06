{-|
Module:      Treb.Config
Description: Configuration parser and types for the Trebuchet server.
Copyright:   Travis Whitaker 2016
License:     MIT
Maintainer:  twhitak@its.jnj.com
Stability:   Provisional
Portability: POSIX
-}

module Treb.Config
    ( TrebCmd(..)
    , DBCmd(..)
    , RunConfig(..)
    , SSLConfig(..)
    , OAConfig(..)
    , PGConfig(..)
    , getTrebCmd
    , banner ) where

import Options.Applicative
import Text.PrettyPrint.ANSI.Leijen (vcat, text, hardline)
import Options.Applicative.Help.Chunk (paragraph, extractChunk)
import Data.Word

---- Types ----

data TrebCmd
    = RunCmd RunConfig
    | DBCmd DBCmd

data RunConfig = RunConfig
  { confDebugMode      :: Bool
  , confJobTemplateDir :: String
  , confDataBlockDir   :: String
  , confDomain         :: String
  , confPort           :: Int
  , confSSL            :: Maybe SSLConfig
  , confOA             :: Maybe OAConfig
  , confPG             :: PGConfig }

data SSLConfig = SSLConfig
  { confSSLPort          :: Int
  , confSSLCertPath      :: String
  , confSSLCertKeyPath   :: String
  , confSSLAllowInsecure :: Bool }

data OAConfig = OAConfig
  { confOAHost     :: String
  , confOAPort     :: Word16
  , confOADatabase :: String
  , confOAUsername :: String
  , confOAPassword :: String
  , confOADomain   :: String }

data PGConfig = PGConfig
  { confPGHost         :: String
  , confPGPort         :: Word16
  , confPGUsername     :: String
  , confPGPassword     :: String
  , confPGDatabase     :: String
  , confPGPoolMax      :: Int
  , confPGConnLifetime :: Int }

data DBCmd
    = DBInitCmd PGConfig
    | DBDropCmd PGConfig

---- Functions ----

-- | Read and parse the server configuration from command line arguments.
getTrebCmd :: IO TrebCmd
getTrebCmd = execParser $ trebCmd `withInfo` trebDesc

trebCmd :: Parser TrebCmd
trebCmd =
    subparser $
        ( command "db"  $ dbCmd  `withInfo` "Initialize or drop the Trebuchet database schema in PostgreSQL." )
     <> ( command "run" $ runCmd `withInfo` "Run Trebuchet server." )

dbCmd :: Parser TrebCmd
dbCmd = fmap DBCmd $
    subparser $
        ( command "init" $ (DBInitCmd <$> pgConfig) `withInfo` "Initialize the Trebuchet database schema in PostgreSQL."  )
     <> ( command "drop" $ (DBDropCmd <$> pgConfig) `withInfo` "Drop the Trebuchet database schema in PostgreSQL." )

-- | Parser of all options passed to Trebuchet.
runCmd :: Parser TrebCmd
runCmd = fmap RunCmd $
    RunConfig
        -- confDebugMode :: Bool
        <$> switch
            ( long "debug"
           <> help "Enable phony authentication and disable all interactions with OpenAtrium." )
        -- confJobTemplateDir :: FilePath
        <*> strOption
            ( long "job-templates"
           <> metavar "PATH"
           <> action "directory"
           <> help "Set the directory to search for job templates specifications." )
        -- confDataBlockDir :: FilePath
        <*> strOption
            ( long "datablocks"
           <> metavar "PATH"
           <> action "directory"
           <> help "Set the directory to search for protocol buffer datablock files." )
        -- confDomain :: String -- NOTE: Should attempt a reverse DNS lookup.
        <*> strOption
            ( long "domain"
           <> metavar "FQDN"
           <> help "Set the fully qualified domain name which clients are expected to access this HTTP server through." )
        -- confPort :: Int
        <*> option auto
            ( long "http-port"
           <> metavar "PORT"
           <> value 8080
           <> showDefault
           <> help "Set the HTTP port to listen on." )
        -- confSSL :: Maybe SSLConfig
        <*> optional sslConfig
        -- confOA :: Maybe OAConfig
        <*> optional oaConfig
        -- confPG :: PGConfig
        <*> pgConfig

-- | SSL/HTTPS configuration parser.
sslConfig :: Parser SSLConfig
sslConfig =
    SSLConfig
        -- confSSLPort :: Int
        <$> option auto
            ( long "ssl-port"
           <> metavar "PATH"
           <> value 443
           <> showDefault
           <> help "Set the port to listen for HTTPS requests on." )
        -- confSSLCertPath :: FilePath
        <*> strOption
            ( long "ssl-certificate"
           <> metavar "PATH"
           <> action "file"
           <> help "Set the path of the SSL certificate file for HTTPS." )
        -- confSSLCertKeyPath :: FilePath
        <*> strOption
            ( long "ssl-certificate-key"
           <> metavar "PATH"
           <> action "file"
           <> help "Set the path of the SSL certificate key file for HTTPS." )
        -- confSSLAllowInsecure :: Bool
        <*> switch
            ( long "allow-insecure-http"
           <> help "Accept HTTP connections with or without SSL. Otherwise, send an HTTP 200 response with a message describing the failure due to insecurity." )

-- | OpenAtrium configuration parser.
oaConfig :: Parser OAConfig
oaConfig =
    OAConfig
        -- confOAHost :: String
        <$> strOption
            ( long "oa-db-host"
           <> metavar "HOSTNAME"
           <> help "Set the hostname at which the OpenAtrium MySQL database server can be found." )
        -- confOAPort :: Int
        <*> option auto
            ( long "oa-db-port"
           <> metavar "PORT"
           <> value 3306
           <> showDefault
           <> help "Set the port of the OpenAtrium MySQL server." )
        -- confOADatabase :: String
        <*> strOption
            ( long "oa-db-schema"
           <> metavar "SCHEMA"
           <> help "Set the name of the database schema allocated for OpenAtrium on its MySQL database server." )
        -- confOAUsername :: String
        <*> strOption
            ( long "oa-db-username"
           <> metavar "USERNAME"
           <> help "Set the username for accessing the OpenAtrium MySQL database server." )
        -- confOAPassword :: String
        <*> strOption
            ( long "oa-db-password"
           <> metavar "PASSWORD"
           <> help "Set the password for accessing the OpenAtrium MySQL database server." )
        -- confOADomain :: String
        <*> strOption
            ( long "oa-cookie-domain"
           <> metavar "FQDN"
           <> help "Set the fully qualified domain name of the OpenAtrium web server. This value is necessary for calculating the name of session cookies." )

-- | PostgreSQL configuration parser.
pgConfig :: Parser PGConfig
pgConfig =
    PGConfig
        -- confPGHost :: String
        <$> strOption
            ( long "pg-host"
           <> metavar "HOSTNAME"
           <> value "localhost"
           <> showDefault
           <> help "Set the hostname of the PostgreSQL database server." )
        -- confPGPort :: Int
        <*> option auto
            ( long "pg-port"
           <> metavar "PORT"
           <> value 5432
           <> showDefault
           <> help "Set the port of the PostgreSQL database server." )
        -- confPGUsername :: String
        <*> strOption
            ( long "pg-username"
           <> metavar "USERNAME"
           <> help "Set the hostname of the PostgreSQL database server." )
        -- confPGPassword :: String
        <*> strOption
            ( long "pg-password"
           <> metavar "PASSWORD"
           <> help "Set the hostname of the PostgreSQL database server." )
        -- confPGDatabase :: String
        <*> strOption
            ( long "pg-schema"
           <> metavar "SCHEMA"
           <> value "trebuchet"
           <> showDefault
           <> help "Set the hostname of the PostgreSQL database server." )
        -- confPGPoolMax :: Int
        <*> option auto
            ( long "pg-pool-max"
           <> value 6
           <> showDefault
           <> metavar "COUNT"
           <> help "Set the hostname of the PostgreSQL database server." )
        -- confPGConnLifetime :: Int
        <*> option auto
            ( long "pg-conn-lifetime"
           <> value 30
           <> showDefault
           <> metavar "SECONDS"
           <> help "Set the amount of time to keep an inactive PostgreSQL connection alive." )

-- | Transform any option parser to one that accepts [-h|--help] to display a
--   comprehensive help message.
withHelp :: Parser a -> Parser a
withHelp = (abortOption
                ShowHelpText
                ( long "help"
               <> short 'h'
               <> help "Show this help message." ) *>)

-- | Set good defaults for command ParserInfo.
withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (withHelp opts) $ fullDesc <> headerDoc (Just $ vcat $ map text banner) <> progDescDoc (Just $ hardline <> (extractChunk $ paragraph desc))

-- | Stylish textual rendering of this server's name 'Trebuchet'.
banner :: [String]
banner =
    [ " _____         _           _       _   "
    , "|_   _|___ ___| |_ _ _ ___| |_ ___| |_ "
    , "  | | |  _| -_| . | | |  _|   | -_|  _|"
    , "  |_| |_| |___|___|___|___|_|_|___|_|  " ]

-- | High-level description of Trebuchet.
trebDesc :: String
trebDesc = concat
    [ "Trebuchet is an API server for the PKU stack. It serves a set of "
    , "\"datablocks\", enabling arbitrary visualization and analysis derived "
    , "from this data set." ]
