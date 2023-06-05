module Smarrow.Utils where

import Data.List (intercalate)
import Debug.Trace

------------------------------------------------------------------------

tracing :: Bool
tracing = False

debug :: String -> [(String, String)] -> a -> a
debug ctx kvs e
  | tracing   = trace ("[" ++ ctx ++ "] " ++
                       intercalate ", " [ k ++ " = " ++ v | (k, v) <- kvs ]) e
  | otherwise = e
