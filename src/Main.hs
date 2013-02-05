module Main
(
  main
) where

-------------------------------------------------------------------------------
import            Configuration.Config (Config(..))
import qualified  Configuration.Config as Config

import            Uir.Update (update)

import            Debugger
-------------------------------------------------------------------------------
import System.Environment (getArgs)
import System.IO (BufferMode(..), hSetBuffering, stdout)
-------------------------------------------------------------------------------

protectedRun :: [String] -> Config -> IO ()
protectedRun args config = do
  case args of
    ["update"] -> update config
    otherwise  -> putStrLn "Usage: `uir update`"

protect worker = worker

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  args    <- getArgs
  config  <- Config.loadConfig
  protect $ protectedRun args config