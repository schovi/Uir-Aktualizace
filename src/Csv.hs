module Csv
(
  parseCsv
) where

----------------------------------------------------------------------------------
import           Data.Spreadsheet
import           Control.Monad.Exception.Asynchronous.Lazy (result)
import           Data.String.Utils (replace)

----------------------------------------------------------------------------------
parseCsv :: String -> IO [[String]]
parseCsv dataString = return $ result $ fromString '"' ';' (replace "\\;" "" $ replace "\\\r\n" " " dataString)