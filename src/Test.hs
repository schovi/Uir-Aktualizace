module Test () where

-----------------------------------------------------------------------------
import            Configuration.Config (Config(..))
import qualified  Configuration.Config as Config

import            Uir.Update (update)

import            Debugger
-----------------------------------------------------------------------------
--import System.Process
--import System.Process.Pipe
--import HSH
-----------------------------------------------------------------------------

test :: IO ()
test = do
  config  <- Config.loadConfig
  update config
  return ()


--main :: IO ()
--main = do
--  result <- run $ "curl -s http://forms.mpsv.cz/uir/files/Verze_42/ADR00446.ZIP | gzip -d -c | iconv -f CP1250 -t UTF-8" :: IO String
--  putStrLn result
--  return ()

--test :: IO ()
--test = do
--  zipFile           <- readProcess "curl" ["-s", "http://forms.mpsv.cz/uir/files/Verze_42/ADR00446.ZIP"] []
--  uncompressedFile  <- readProcess "gzip" ["-d", "-c"] zipFile
--  file              <- readProcess "iconv" ["-f CP1250", "-t UTF8"] uncompressedFile
--  putStrLn uncompressedFile
--  return ()

--test :: IO ()
--test = do
--  result <- pipeString  [ ("curl", ["-s", "http://forms.mpsv.cz/uir/files/Verze_42/ADR00446.ZIP"])
--                        , ("gzip", ["-d", "-c"])
--                        , ("iconv", ["-f CP1250", "-t UTF8"])
--                        ] []
--  putStrLn result
--  return ()