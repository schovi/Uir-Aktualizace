module Configuration.Config
( Config(..)
, loadConfig
) where

-- Db
import qualified  Configuration.Db as Db (newConnection)
import            Database.HDBC.MySQL (Connection)
-- File
import            Configuration.File

import Text.JSON
----------------------------------------------------------------------------------

instance Show Connection where
  show a = "MySQL Connection"

data Config = Config { dbConnection       :: Connection
                     } deriving (Show)

----------------------------------------------------------------------------------

loadConfig :: IO Config
loadConfig = do
  config <- readConfig
  dbConn <- Db.newConnection
  return $ Config dbConn