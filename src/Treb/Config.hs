module Treb.Config (withEnv) where

getPool :: TrebConfig -> EitherT String IO (H.Pool HP.Postgres)
getPool conf = do
  mapM_ (\(attr, msg) ->
    leftIf
      (isNothing $ attr conf)
      $ msg <> " for PostgreSQL database not given.")
    [ (confPGHost,         "Host")
    , (confPGPort,         "Port")
    , (confPGUsername,     "Username")
    , (confPGPassword,     "Password")
    , (confPGDatabase,     "Database name")
    , (confPGPoolMax,      "Maximum pool size")
    , (confPGConnLifetime, "Connection duration") ]

  pgPort         <- hoistEither $ readEither $ fromJust $ confPGPort conf
  pgPoolMax      <- hoistEither $ readEither $ fromJust $ confPGPoolMax conf
  pgConnLifetime <- hoistEither $ readEither $ fromJust $ confPGConnLifetime conf

  maybe
    (left "Invalid PostgreSQL pool settings.")
    (liftIO . uncurry H.acquirePool)
    $ (,) <$> (HP.ParamSettings <$> fmap BC.pack (confPGHost conf)
                                <*> pure pgPort
                                <*> fmap BC.pack (confPGUsername conf)
                                <*> fmap BC.pack (confPGPassword conf)
                                <*> fmap BC.pack (confPGDatabase conf))
          <*> (fromMaybe Nothing $ H.poolSettings <$> pure pgPoolMax
                                                  <*> pure pgConnLifetime)

getEnv :: EitherT String IO TrebEnv
getEnv = do
  -- Generate configuration from command line arguments
  conf <- processArgs defaultTrebConfig =<< liftIO getArgs

  -- Create a pool of connections to PostgreSQL
  pool <- getPool conf

  -- Check that SSL-related command line arguments are well formed
  leftIf
    (isJust (confSSLCertPath conf) `xor` isJust (confSSLCertKeyPath conf))
    $ "SSL requires both -c/--ssl-certificate and -k/--ssl-certificate-key to be set."
    
  -- Check that the job template directory exists
  let jobTemplateDir = confJobTemplateDir conf

  cwd <- liftIO getCurrentDirectory
  jobTemplateDirExists <- liftIO $ doesFileExist jobTemplateDir

  leftIf
    jobTemplateDirExists
    $ "Job template directory '" <> (cwd </> jobTemplateDir) <> "' not found."

  -- Create TVar for updating the job templates available to HTTP request handlers
  jobTemplatesTVar <- liftIO $ newTVarIO Nothing

  -- Begin watching job_templates directory and automatically update the internal job templates accordingly
  liftIO $ do
    putStrLn "Initializing event watchers for job templates directory."
  
    inotify <- initINotify
    addWatch inotify [Create, Delete, Modify, MoveIn, MoveOut] jobTemplateDir $ \ _ ->
      getJobTemplates jobTemplateDir
        >>= atomically . swapTVar jobTemplatesTVar . Just
        >> putStrLn "Job Templates Updated."

    putStrLn "> Done."

  -- Get the initial job templates
  jobTemplates <- liftIO $ putStrLn "Parsing job templates."
                        *> getJobTemplates jobTemplateDir
                        <* putStrLn "> Done."

  -- Connect to the Drupal/OpenAtrium MySQL database for authentication and authorization
  drupalMySQLConn <- unlessDebugMode conf $ do
    mapM_ (\(attr, msg) ->
      leftIf
        (isNothing $ attr conf)
        $ msg <> " for OpenAtrium database not given.")
      [ (confOAHost,     "Host")
      , (confOAPort,     "Port")
      , (confOADatabase, "Database name")
      , (confOAUsername, "Username")
      , (confOAPassword, "Password") ]

    liftIO $ putStrLn "Connecting to Drupal/OpenAtrium MySQL database."

    oaPort <- hoistEither $ readEither $ fromJust $ confOAPort conf
    ret <- liftIO $ MySQL.connect $
       MySQL.defaultConnectInfo
         { MySQL.connectHost     = fromJust $ confOAHost conf
         , MySQL.connectPort     = oaPort
         , MySQL.connectDatabase = fromJust $ confOADatabase conf
         , MySQL.connectUser     = fromJust $ confOAUsername conf
         , MySQL.connectPassword = fromJust $ confOAPassword conf }

    liftIO $ putStrLn "> Done."
    return ret

  -- Construct the Trebuchet environment
  return $ TrebEnv
    { trebEnvJobTemplates     = jobTemplates
    , trebEnvJobTemplatesTVar = jobTemplatesTVar
    , trebEnvDrupalMySQLConn  = drupalMySQLConn
    , trebEnvUsername         = Nothing
    , trebEnvConfig           = conf }
  where
    catchAny :: (SomeException -> IO a) -> IO a -> IO a
    catchAny = flip catch

    processArgs :: TrebConfig -> [String] -> EitherT String IO TrebConfig
    processArgs conf []                                                      = right conf
    processArgs conf (x  :xs) | x == "-d" || x == "--debug"                  = processArgs (conf { confDebugMode      = True })   xs
    processArgs conf (x:y:xs) | x == "-c" || x == "--ssl-certificate"        = processArgs (conf { confSSLCertPath    = Just y }) xs
    processArgs conf (x:y:xs) | x == "-k" || x == "--ssl-certificate-key"    = processArgs (conf { confSSLCertKeyPath = Just y }) xs
    processArgs conf (x:y:xs) | x == "-t" || x == "--job-template-directory" = processArgs (conf { confJobTemplateDir = y })      xs
    processArgs conf (x:y:xs) | x == "-p" || x == "--port"                   = either
                                                                                 left
                                                                                 (\p -> processArgs (conf { confPort  = p })      xs)
                                                                                 (readEither y)
    processArgs conf (x:y:xs) | x == "-H" || x == "--oa-host"                = processArgs (conf { confOAHost         = Just y }) xs
    processArgs conf (x:y:xs) | x == "-P" || x == "--oa-port"                = processArgs (conf { confOAPort         = Just y }) xs
    processArgs conf (x:y:xs) | x == "-D" || x == "--oa-database"            = processArgs (conf { confOADatabase     = Just y }) xs
    processArgs conf (x:y:xs) | x == "-U" || x == "--oa-username"            = processArgs (conf { confOAUsername     = Just y }) xs
    processArgs conf (x:y:xs) | x == "-P" || x == "--oa-password"            = processArgs (conf { confOAPassword     = Just y }) xs
    processArgs conf (x:y:xs) | x == "-C" || x == "--oa-cookie-domain"       = processArgs (conf { confOADomain       = Just y }) xs
    processArgs conf (x:y:xs) | x == "-h" || x == "--pg-host"                = processArgs (conf { confPGHost         = Just y }) xs
    processArgs conf (x:y:xs) | x == "-b" || x == "--pg-port"                = processArgs (conf { confPGPort         = Just y }) xs
    processArgs conf (x:y:xs) | x == "-u" || x == "--pg-username"            = processArgs (conf { confPGUsername     = Just y }) xs
    processArgs conf (x:y:xs) | x == "-w" || x == "--pg-password"            = processArgs (conf { confPGPassword     = Just y }) xs
    processArgs conf (x:y:xs) | x == "-s" || x == "--pg-database"            = processArgs (conf { confPGDatabase     = Just y }) xs
    processArgs conf (x:y:xs) | x == "-m" || x == "--pg-pool-max"            = processArgs (conf { confPGPoolMax      = Just y }) xs
    processArgs conf (x:y:xs) | x == "-l" || x == "--pg-conn-lifetime"       = processArgs (conf { confPGConnLifetime = Just y }) xs
    processArgs conf (x:_)                                                   = left $ "ERROR: Invalid command-line argument \'" <> x <> "\'."

