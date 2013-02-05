module Configuration.Db
( newConnection
) where

-------------------------------------------------------------------------------
import Configuration.File

import Control.Monad
import Database.HDBC
import Database.HDBC.MySQL

import Text.JSON
-------------------------------------------------------------------------------

readSqlConfig :: JSObject JSValue -> Result MySQLConnectInfo
readSqlConfig conf = do
  host <- conf ! "mysqlHost"
  user <- conf ! "mysqlUser"
  pass <- conf ! "mysqlPassword"
  name <- conf ! "mysqlDatabase"
  port <- conf ! "mysqlPort"
  sock <- conf ! "mysqlUnixSocket"
  return MySQLConnectInfo { mysqlHost = host, mysqlUser = user, mysqlPassword = pass,
                            mysqlDatabase = name, mysqlPort = read port,
                            mysqlUnixSocket = sock, mysqlGroup = Nothing }

newConnection :: IO Connection
newConnection = do
  config <- readConfig
  case readSqlConfig config of Ok conInf -> do  conn <- connectMySQL conInf
                                                setNamesDb conn "utf8" "utf8_unicode_ci"
                                                return conn
                               Error msg -> error ("Cannot parse config file: " ++ msg)

setNamesDb :: Connection -> String -> String -> IO ()
setNamesDb conn encoding collation = do
  run conn ("SET NAMES '" ++ encoding ++ "' COLLATE '" ++ collation ++ "'") []
  return ()
