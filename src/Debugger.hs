module Debugger
(
  debugStr,
  debug,
  dividor
) where

--import Debug.Trace
import System.IO.Unsafe (unsafePerformIO)


debugStr :: String -> IO ()
--debugStr something = return ()
debugStr = putStrLn

debug :: String -> a -> a
--debug something expr = expr
debug string expr = unsafePerformIO $ do
  putStrLn string
  return expr

dividor = replicate 80 '-' ++ "\n"
