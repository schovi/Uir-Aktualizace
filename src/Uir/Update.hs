{-# OPTIONS_GHC -XDoAndIfThenElse #-}

module Uir.Update
( update
) where

-------------------------------------------------------------------------------
-- Config
import            Configuration.Config (Config)
import qualified  Configuration.Config as Config
-- Db
import            Uir.Db (Connection, quickQuery', run, toSql, commit, rollback, deleteRow, insertRow, updateRow, getLastImportedVersion)

import            Uir.Helpers
import            Debugger
-------------------------------------------------------------------------------
import            Csv
import qualified  Data.Map as Map
import            Text.HandsomeSoup
import            Text.XML.HXT.Core
import            Text.Regex.Posix
import            Control.Monad
import qualified  HSH as HSH
import            Data.String.Utils (strip)
-------------------------------------------------------------------------------

bannedLinks = ["../uir/files/Verze_42/ADR00881.ZIP"]

update :: Config -> IO ()
update config = do
  let dbConn          = Config.dbConnection config
  availableVersions   <- getAvailableVersions "http://forms.mpsv.cz/uir/view.jsp?D=Verze_42"
  lastImportedVersion <- getLastImportedVersion dbConn
  debugStr $ show availableVersions
  let notImportedVersions = reverse . filter (notImported lastImportedVersion) $ availableVersions
  result <- foldM (\fold file -> if fold == True then processFile dbConn file else return False) True notImportedVersions
  return ()

processFile :: Connection -> String -> IO Bool
processFile dbConn file = do
  putStrLn $ "Downloading and importing " ++ file
  rawData <- HSH.run $ ("curl -s 'http://forms.mpsv.cz" ++ (drop 2 file) ++ "' | gzip -d -c | iconv -f CP1250 -t UTF-8") :: IO String
  --debugStr $ show rawData
  parsedData <- parseCsv rawData
  --debugStr $ show parsedData
  let (header:records) = parsedData
  valid <- validHeader dbConn header
  if valid
    then do
      debugStr "Processing records ..."
      result <- foldM folder True $ init records
      debugStr "Done processing records"
      if result then do
        debugStr "Processing OK"
        debugStr "Processing footer"
        debugStr $ show header
        processFooter dbConn header
        commit dbConn
        putStrLn "Done"
        return True
      else do
        rollback dbConn
        putStrLn "Fail"
        return False
    else
      return False
  where folder :: Bool -> Record -> IO Bool
        folder fold record = if fold == True then debug (show record) (processRecord dbConn record) else return False

notImported :: Int -> String -> Bool
notImported lastImportedVersion link =
  let versionNum = read . dropWhile (=='0') . last . concat $ (link =~ "([0-9]+)" :: [[String]]) :: Int
  in (versionNum > lastImportedVersion) && (not $ elem link bannedLinks)


getAvailableVersions :: String -> IO [String]
getAvailableVersions url = do
  doc <- fromUrl url
  runX $ doc >>> css "td a" ! "href"

returnWithText :: Bool -> String -> IO Bool
returnWithText bool message = do
  putStrLn message
  return bool

validHeader :: Connection -> Record -> IO Bool
validHeader conn header = do
  let applicationType     = header !! 1
      uirVersion          = header !! 2
      version1            = header !! 3
      version2            = header !! 4
      dataVersion         = read (header !! 5)::Int
      previousVersion     = dataVersion - 1
  case (applicationType, uirVersion) of
    ("UIR-ADR", "4")  -> do
                            putStrLn $ "Importing version " ++ version1 ++ "." ++ version2
                            currentVersionRecords <- quickQuery' conn "select * from `verze` where `ver_cislo`=?;" [toSql dataVersion]
                            if null currentVersionRecords
                            then do
                              previousVersionRecords <- quickQuery' conn "select * from `verze` where `ver_cislo`=?;" [toSql previousVersion]
                              if not $ null previousVersionRecords
                              then return True
                              else returnWithText False $ "Version " ++ show dataVersion ++ " couldn't be imported, there is missing version " ++ show previousVersion ++ " in database."
                            else returnWithText False $ "Version " ++ show dataVersion ++ " is already imported and closed."
    ("UIR-ADR", _)    -> returnWithText False $ "Change log version '" ++ uirVersion ++ "' is not supported."
    (_, "4")          -> returnWithText False $ "Change log is for '" ++ applicationType ++ "' application, not for UIR-ADR."
    (_, _)            -> returnWithText False "KaBooM"

processFooter :: Connection -> Record -> IO Integer
processFooter conn header = do
  let dataVersion         = read (header !! 5)::Int
      dataVersionZSJ      = header !! 6
  debugStr $ "dataVersion " ++ (show $ dataVersion)
  debugStr $ "dataVersionZSJ " ++ (show $ dataVersionZSJ)
  debugStr "Parsing date time in footer"
  let dataVersionClosed   = parseDateTime $ header !! 7
  debugStr $ "dataVersionClosed " ++ (show $ dataVersionClosed)
  debugStr "Parsing date time Done"
  res <- run conn "REPLACE INTO `verze` SET `ver_cislo`=?, `ver_zsj`=?, `cas_uzav`=?" [toSql dataVersion, toSql dataVersionZSJ, toSql dataVersionClosed]
  debugStr "Footer updated"
  return res

processRecord :: Connection -> [String] -> IO Bool
processRecord conn (tableIdStr:requestTypeStr:_:rawParams) =
  let params = extractParams rawParams
      requestType = read requestTypeStr::Int
      tableId = read tableIdStr::Int
      recordId :: Int
      recordId = read (params Map.! 1)
  in case requestType of
        0 -> do deleteRow conn tableId recordId
                return True
        1 -> do insertRow conn tableId params
                return True
        2 -> do updateRow conn tableId recordId params
                return True
        _ -> returnWithText False $ "Unknown command '" ++ requestTypeStr ++ "'"

extractParams :: [String] -> Params
extractParams params = Map.fromList $ map splitParam params
  where splitParam param = let (key,(_:val)) = span (/='=') param in (read key :: Int, strip val)