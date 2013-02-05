module Uir.Db
( tables
, dateFields
, insertRow
, updateRow
, deleteRow
, getColumnNames
, getAllTableColumnNames
, getColumnSqlValues
, getColumnValues
, getTableName
, getTablePrimaryKeyName
, getLastImportedVersion
, quickQuery'
, toSql
, fromSql
, commit
, rollback
, run
, Connection(..)
) where

-------------------------------------------------------------------------------
import            Database.HDBC (SqlValue(..), toSql, fromSql, run, quickQuery', commit, rollback, SqlValue)
import            Database.HDBC.MySQL (Connection)
import            Data.Map (Map)
import qualified  Data.Map as Map
import            Data.Array (Array)
import qualified  Data.Array as Array
import            Data.List as List
import            Debugger
-------------------------------------------------------------------------------
import Uir.Helpers
-------------------------------------------------------------------------------


tables :: Map Int (String, Array Int String)
tables =
  Map.fromList $ map (\(tableId, tableName, tableColumns) -> (tableId,(tableName,Array.listArray (1, length tableColumns) tableColumns))) [
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
    (15,"pobvod",["pobvod_kod", "obec_kod", "nazev", "zkratka", "stav", "vznik_dne", "vznik_info", "zanik_dne", "zanik_info"]),
    (16,"pobvod_h",["pobvod_kod", "plati_do", "obec_kod", "nazev", "zkratka", "info"]),
    (17,"mcast",["mcast_kod", "obec_kod", "pobvod_kod", "typ", "nazev", "zkratka", "stav", "vznik_dne", "vznik_info", "zanik_dne", "zanik_info", "sobvod_kod", "nobvod_kod"]),
    (18,"mcast_h",["mcast_kod", "plati_do", "obec_kod", "pobvod_kod", "typ", "nazev", "zkratka", "info", "sobvod_kod", "nobvod_kod"]),
    (19,"oblast",["oblast_kod", "nuts2", "nazev", "zkratka", "stav", "vznik_dne", "vznik_info", "zanik_dne", "zanik_info"]),
    (20,"oblast_h",["oblast_kod", "plati_do", "nuts2", "nazev", "zkratka", "info"]),
    (21,"kraj",["kraj_kod", "nuts3", "oblast_kod", "nazev", "zkratka", "stav", "vznik_dne", "vznik_info", "zanik_dne", "zanik_info"]),
    (22,"kraj_h",["kraj_kod", "plati_do", "nuts3", "oblast_kod", "nazev", "zkratka", "info"]),
    (23,"sobvod",["sobvod_kod", "cislo", "obec_kod", "nazev", "zkratka", "stav", "vznik_dne", "vznik_info", "vznik_dne", "vznik_info"]),
    (24,"sobvod_h",["sobvod_kod", "plati_do", "cislo", "obec_kod", "nazev", "zkratka", "info"]),
    (25,"nobvod",["nobvod_kod", "nuts4", "obec_kod", "nazev", "zkratka", "stav", "vznik_dne", "vznik_info", "vznik_dne", "vznik_info"]),
    (26,"nobvod_h",["nobvod_kod", "plati_do", "nuts4", "obec_kod", "nazev", "zkratka", "info"]),
    (27,"orp",["orp_kod", "kodorp_csu", "kraj_kod", "nazev", "zkratka", "stav", "vznik_dne", "vznik_info", "vznik_dne", "vznik_info"]),
    (28,"orp_h",["orp_kod", "plati_do", "kodorp_csu", "kraj_kod", "nazev", "zkratka", "info"]),
    (29,"pou",["pou_kod", "kodpou_csu", "orp_kod", "nazev", "zkratka", "stav", "vznik_dne", "vznik_info", "vznik_dne", "vznik_info"]),
    (30,"pou_h",["pou_kod", "plati_do", "kodpou_csu", "orp_kod", "nazev", "zkratka", "info"]),
    (54,"obec_d",["obec_kod", "adresa_kod", "jmeno", "telefon", "e_mail"]),
    (55,"vazba",["vazba_id", "mcast_kod", "cobce_kod", "ulice_kod", "psc"]),
    (56,"cob_prev",["kodcob", "plati_od", "plati_do", "cobce_kod", "mcast_kod"]),
    (999,"verze",[]),
    (1000,"zmen_zaz",[]), -- Vlastní ID
    (1001,"zmen_atr",[]), -- Vlastní ID
    (1002,"stav_db",[])  -- Vlastní ID
  ]

dateFields :: [String]
dateFields = ["vznik_dne", "zanik_dne", "plati_od", "plati_do"]

insertRow :: Connection -> Int -> Params -> IO Integer
insertRow conn tableId params = do
  let columnNames = " (" ++ List.intercalate "," (getColumnNames tableId params) ++ ") "
      valueStatement = " (" ++ List.intercalate "," (replicate (length $ Map.keys params) "?") ++ ") "
      sql = "INSERT INTO " ++ getTableName tableId ++ columnNames ++ "VALUES" ++ valueStatement ++ ";"
  debugStr $ "Inserting into table " ++ (show tableId) ++ " data " ++ (show params)
  res <- run conn sql $ getColumnSqlValues tableId params
  debugStr "Done inserting"
  return res

updateRow :: Connection -> Int -> Int -> Params -> IO Integer
updateRow conn tableId recordId params = do
  let primaryKey = getTablePrimaryKeyName tableId
      paramsWithoutPrimaryKey = Map.delete 1 params
      setStatement = List.intercalate ", " $ map (++ " = ?") $ getColumnNames tableId paramsWithoutPrimaryKey
      whereCondition = primaryKey ++ " = ?"
      sql = "UPDATE " ++ (getTableName tableId) ++ " SET " ++ setStatement ++ " WHERE " ++ whereCondition ++ ";"
  debugStr $ "Updating table " ++ (show tableId) ++ " recordId " ++ (show recordId) ++ " data " ++ (show params)
  res <- run conn sql $ (getColumnSqlValues tableId paramsWithoutPrimaryKey) ++ [toSql recordId]
  debugStr "Done updating"
  return res

deleteRow :: Connection -> Int -> Int -> IO Integer
deleteRow conn tableId recordId = do
  let primaryKey = getTablePrimaryKeyName tableId
      sql = "DELETE FROM " ++ (getTableName tableId) ++ " WHERE " ++ primaryKey ++ " = ?;"
  run conn sql [toSql recordId]

getColumnNames :: Int -> Params -> [String]
getColumnNames tableId params = map extractColumn $ Map.keys params
  where extractColumn columnId = (snd $ tables Map.! tableId) Array.! columnId

getAllTableColumnNames :: Int -> Array Int String
getAllTableColumnNames tableId = snd $ tables Map.! tableId

getColumnSqlValues :: Int -> Params -> [SqlValue]
getColumnSqlValues tableId params = Map.elems $ Map.mapWithKey convertParam params
  where convertParam :: Int -> String -> SqlValue
        convertParam columnId value = convertType ((getAllTableColumnNames tableId) Array.! columnId) value
        convertType :: String -> String -> SqlValue
        convertType columnName value = case value of
                                            []        -> SqlNull
                                            otherwise -> if columnName `elem` dateFields then toSql $ parseDate value else toSql value

getColumnValues :: Params -> [String]
getColumnValues = Map.elems

getTableName :: Int -> String
getTableName tableId = fst $ tables Map.! tableId

getTablePrimaryKeyName :: Int -> String
getTablePrimaryKeyName tableId = (snd $ tables Map.! tableId) Array.! 1

getLastImportedVersion :: Connection -> IO Int
getLastImportedVersion conn = do
  result <- quickQuery' conn "select ver_cislo from `verze` order by ver_cislo desc limit 1;" []
  return . fromSql . head . head $ result