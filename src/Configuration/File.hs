{-# OPTIONS_GHC -XNoMonomorphismRestriction #-}

module Configuration.File
( readConfig
, (!)
) where

-------------------------------------------------------------------------------
import Text.JSON
-------------------------------------------------------------------------------
(!) :: (JSObject JSValue) -> String -> Result String
(!) = flip valFromObj

readConfig :: IO (JSObject JSValue)
readConfig = do
  file <- readFile "config/config.json"
  case decode file of Ok z    -> return z
                      Error _ -> error "Cannot read config.json in current directory."