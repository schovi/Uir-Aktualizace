import            Csv
import            Database.HDBC (toSql, fromSql, run, runRaw, quickQuery', commit, SqlValue)
import            Database.HDBC.MySQL (Connection)
import            Data.List (intercalate)
import qualified  Data.Map as M
import qualified  Data.Array as A
import            Configuration (connection)
--import           Text.HandsomeSoup
--import           Text.XML.HXT.Core
import            Data.Time
import            Data.Time.LocalTime (LocalTime)
import            System.Locale



type Params = M.Map Int String
type Record = [String]


tables :: M.Map Int (String, A.Array Int String)
tables =
  M.fromList $ map (\(tableId, tableName, tableColumns) -> (tableId,(tableName,A.listArray (1, length tableColumns) tableColumns))) [
    (1,"okres",["okres_kod", "nazev", "zkratka", "stav", "vznik_dne", "vznik_info", "zanik_dne", "zanik_info", "nuts4", "kraj_kod"]),
    (2,"okres_h",["okres_kod", "plati_do", "nazev", "zkratka", "info", "nuts4", "kraj_kod"]),
    (3,"obec",["obec_kod", "okres_kod", "nazev", "zkratka", "stav", "vznik_dne", "vznik_info", "zanik_dne", "zanik_info", "nuts5", "pou_kod"]),
    (4,"obec_h",["obec_kod", "plati_do", "okres_kod", "nazev", "zkratka", "info", "nuts5", "pou_kod"]),
    (5,"cobce",["cobce_kod", "obec_kod", "nazev", "zkratka", "stav", "vznik_dne", "vznik_info", "zanik_dne", "zanik_info"]),
    (6,"cobce_h",["cobce_kod", "plati_do", "obec_kod", "nazev", "zkratka", "info"]),
    (7,"ulice",["ulice_kod", "obec_kod", "nazev", "zkratka", "stav", "vznik_dne", "vznik_info", "zanik_dne", "zanik_info"]),
    (8,"ulice_h",["ulice_kod", "plati_do", "obec_kod", "nazev", "zkratka", "info"]),
    (9,"objekt",["objekt_kod", "cobce_kod", "cisdom_typ", "cisdom_hod", "stav", "vznik_dne", "vznik_info", "zanik_dne", "zanik_info", "mcast_kod", "idob"]),
    (10,"objekt_h",["objekt_kod", "plati_do", "cobce_kod", "cisdom_typ", "cisdom_hod", "info", "mcast_kod"]),
    (11,"adresa",["adresa_kod", "objekt_kod", "ulice_kod", "cisor_hod", "cisor_pis", "psc", "stav", "vznik_dne", "vznik_info", "zanik_dne", "zanik_info","pcd", "x", "y"]),
    (12,"adresa_h",["adresa_kod", "plati_do", "objekt_kod", "ulice_kod", "cisor_hod", "cisor_pis", "psc", "info"]),
    (13,"posta",["psc", "nazev", "zkratka", "stav", "vznik_dne", "vznik_info", "zanik_dne", "zanik_info"]),
    (14,"posta_h",["psc", "plati_do", "nazev", "zkratka", "info"]),
    (15,"pobvod",[]),
    (16,"pobvod_h",[]),
    (17,"mcast",[]),
    (18,"mcast_h",[]),
    (19,"oblast",["oblast_kod", "nuts2", "nazev", "zkratka", "stav", "vznik_dne", "vznik_info", "zanik_dne", "zanik_info"]),
    (20,"oblast_h",["oblast_kod", "plati_do", "nuts2", "nazev", "zkratka", "info"]),
    (21,"kraj",["kraj_kod", "nuts3", "oblast_kod", "nazev", "zkratka", "stav", "vznik_dne", "vznik_info", "zanik_dne", "zanik_info"]),
    (22,"kraj_h",["kraj_kod", "plati_do", "nuts3", "oblast_kod", "nazev", "zkratka", "info"]),
    (23,"sobvod",[]),
    (24,"sobvod_h",[]),
    (25,"nobvod",[]),
    (26,"nobvod_h",[]),
    (27,"orp",[]),
    (28,"orp_h",[]),
    (29,"pou",[]),
    (30,"pou_h",[]),
    (54,"obec_d",[]),
    (55,"vazba",["vazba_id", "mcast_kod", "cobce_kod", "ulice_kod", "psc"]),
    (56,"cob_prev",[]),
    (999,"verze",[]),
    (1000,"zmen_zaz",[]), -- Vlastní ID
    (1001,"zmen_atr",[]), -- Vlastní ID
    (1002,"stav_db",[])  -- Vlastní ID
  ]

dateFields :: [String]
dateFields = ["vznik_dne", "zanik_dne", "plati_od", "plati_do"]

-- main cycle
process :: IO ()
process = do
  -- TODO Download z webu
  parsedData <- parseCsv "../data/update.utf8.txt"
  --parsedData <- parseCsv "../data/test.txt"
  conn <- connection
  let (header:records) = parsedData
  setNamesDb conn "utf8" "utf8_unicode_ci"
  valid <- validHeader conn header
  if valid
    then do
      mapM_ (processRecord conn) $ init records
      processFooter conn header
      commit conn
    else
      return ()

-- TODO stahování verzí
--downloadVersionsPage "http://forms.mpsv.cz/uir/view.jsp?D=Verze_42"
--downloadVersionsPage :: String ->
--downloadVersionsPage url = do
--  doc <- fromUrl url
--  let trs = doc <<< css "tr"
--      tds = trs <<< css "td"
--  tds

setNamesDb conn encoding collation = do
  run conn ("SET NAMES '" ++ encoding ++ "' COLLATE '" ++ collation ++ "'") []
  return ()

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
                            if null currentVersionRecords -- && (head currentVersion) !! 2 -- TODO check jestli je ta verze uzavřená ?
                            then do
                              previousVersionRecords <- quickQuery' conn "select * from `verze` where `ver_cislo`=?;" [toSql previousVersion]
                              if not $ null previousVersionRecords
                              then return True
                              else do
                                putStrLn $ "Version " ++ show dataVersion ++ " couldn't be imported, there is missing version " ++ show previousVersion ++ " in database."
                                return False
                            else do
                              putStrLn $ "Version " ++ show dataVersion ++ " is already imported and closed."
                              return False
    ("UIR-ADR", _)    -> do
                            putStrLn $ "Change log version " ++ uirVersion ++ " is not supported."
                            return False
    (_, "4")          -> do
                            putStrLn $ "Change log is for '" ++ applicationType ++ "' application, not for UIR-ADR."
                            return False

parseDateTime :: String -> LocalTime
parseDateTime dateTimeStr = readTime defaultTimeLocale "%d.%m.%Y %H:%M:%S" dateTimeStr

parseDate :: String -> Day
parseDate dateStr = readTime defaultTimeLocale "%d.%m.%Y" dateStr

processFooter :: Connection -> Record -> IO Integer
processFooter conn header = do
  let dataVersion         = read (header !! 5)::Int
      dataVersionZSJ      = read (header !! 6)::Int
      dataVersionClosed   = parseDateTime $ header !! 7
  run conn "REPLACE INTO `verze` SET `ver_cislo`=?, `ver_zsj`=?, `cas_uzav`=?" [toSql dataVersion, toSql dataVersionZSJ, toSql dataVersionClosed]

processRecord :: Connection -> [String] -> IO Bool
processRecord conn (tableIdStr:requestType:_:rawParams) =
  let params = extractParams rawParams
      tableId :: Int
      tableId = read tableIdStr
      recordId :: Int
      recordId = read (params M.! 1)
  in case read requestType::Int of
        0 -> do deleteRow conn tableId recordId
                return True
        1 -> do insertRow conn tableId params
                return True
        2 -> do updateRow conn tableId recordId params
                return True
        _ -> do putStrLn $ "Unknown command '" ++ requestType ++ "'"
                return False

insertRow :: Connection -> Int -> Params -> IO Integer
insertRow conn tableId params = do
  let columnNames = " (" ++ intercalate "," (getColumnNames tableId params) ++ ") "
      valueStatement = " (" ++ intercalate "," (replicate (length $ M.keys params) "?") ++ ") "
      sql = "INSERT INTO " ++ getTableName tableId ++ columnNames ++ "VALUES" ++ valueStatement ++ ";"
  run conn sql $ getColumnSqlValues tableId params

updateRow :: Connection -> Int -> Int -> Params -> IO Integer
updateRow conn tableId recordId params = do
  let primaryKey = getTablePrimaryKeyName tableId
      paramsWithoutPrimaryKey = M.delete 1 params
      setStatement = intercalate ", " $ map (++ " = ?") $ getColumnNames tableId paramsWithoutPrimaryKey
      whereCondition = primaryKey ++ " = ?"
      sql = "UPDATE " ++ (getTableName tableId) ++ " SET " ++ setStatement ++ " WHERE " ++ whereCondition ++ ";"
  run conn sql $ (getColumnSqlValues tableId paramsWithoutPrimaryKey) ++ [toSql recordId]

deleteRow :: Connection -> Int -> Int -> IO Integer
deleteRow conn tableId recordId = do
  let primaryKey = getTablePrimaryKeyName tableId
      sql = "DELETE FROM " ++ (getTableName tableId) ++ " WHERE " ++ primaryKey ++ " = ?;"
  run conn sql [toSql recordId]

extractParams :: [String] -> Params
extractParams params = M.fromList $ map splitParam params
  where splitParam param = let (key,(_:val)) = span (/='=') param in (read key :: Int,val)

getColumnNames :: Int -> Params -> [String]
getColumnNames tableId params = map extractColumn $ M.keys params
  where extractColumn columnId = (snd $ tables M.! tableId) A.! columnId

getAllTableColumnNames :: Int -> A.Array Int String
getAllTableColumnNames tableId = snd $ tables M.! tableId

getColumnSqlValues :: Int -> Params -> [SqlValue]
getColumnSqlValues tableId params = M.elems $ M.mapWithKey convertParam params
  where convertParam :: Int -> String -> SqlValue
        convertParam columnId value = convertType ((getAllTableColumnNames tableId) A.! columnId) value
        convertType :: String -> String -> SqlValue
        convertType columnName value = if columnName `elem` dateFields then toSql $ parseDate value else toSql value

getColumnValues :: Params -> [String]
getColumnValues = M.elems

getTableName :: Int -> String
getTableName tableId = fst $ tables M.! tableId

getTablePrimaryKeyName :: Int -> String
getTablePrimaryKeyName tableId = (snd $ tables M.! tableId) A.! 1