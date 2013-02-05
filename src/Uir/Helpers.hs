module Uir.Helpers
( parseDateTime
, parseDate
, Params
, Record
) where

-------------------------------------------------------------------------------
import            Data.Map (Map)
import            Data.Time
import            Data.Time.LocalTime (LocalTime)
import            System.Locale

-------------------------------------------------------------------------------

type Params = Map Int String
type Record = [String]


parseDateTime :: String -> LocalTime
parseDateTime dateTimeStr = readTime defaultTimeLocale "%d.%m.%Y-%H:%M:%S" dateTimeStr

parseDate :: String -> Day
parseDate dateStr = readTime defaultTimeLocale "%d.%m.%Y" dateStr
