module Configuration
( connection
) where

import Database.HDBC.MySQL
import Text.JSON

----------------------------------------------------------------------------------
-- A database connection read from the current directory


readSqlConfig :: JSObject JSValue -> Result MySQLConnectInfo
readSqlConfig conf = let (!) = flip valFromObj in do
  host <- conf ! "mysqlHost"
  user <- conf ! "mysqlUser"
  pass <- conf ! "mysqlPassword"
  name <- conf ! "mysqlDatabase"
  port <- conf ! "mysqlPort"
  sock <- conf ! "mysqlUnixSocket"
  return MySQLConnectInfo { mysqlHost = host, mysqlUser = user, mysqlPassword = pass,
                            mysqlDatabase = name, mysqlPort = read port,
                            mysqlUnixSocket = sock, mysqlGroup = Nothing }

readConfig :: IO (JSObject JSValue)
readConfig = do
  file <- readFile "../config.json"
  case decode file of Ok z    -> return z
                      Error _ -> error "Cannot read config.json in current directory."

connection :: IO Connection
connection = do
  config <- readConfig
  case readSqlConfig config of Ok conInf -> connectMySQL conInf
                               Error msg -> error ("Cannot parse config file: " ++ msg)