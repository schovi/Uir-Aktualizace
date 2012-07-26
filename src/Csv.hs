module Csv
(
  parseCsv
) where

----------------------------------------------------------------------------------
import           Data.Spreadsheet
import           Data.Maybe
import           Data.List
import           Control.Monad.Exception.Asynchronous.Lazy (result)
import qualified Data.Map as M
import qualified Data.ByteString as B
import qualified Data.HashMap.Strict as H

maybeStr :: String -> Maybe String
maybeStr s | s == ""   = Nothing
           | otherwise = Just s

----------------------------------------------------------------------------------
-- Parsing from CSV file

parseCsv :: String -> IO [[String]]
parseCsv fileName = readFile fileName >>= return . result . fromString '"' ';'