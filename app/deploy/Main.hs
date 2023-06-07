module Main where

import Smarrow.Deploy.EventLoop

------------------------------------------------------------------------

pORT :: Int
pORT = 8080

main :: IO ()
main = do
  putStrLn ("Starting smarrow event loop on port " ++ show pORT)
  eventLoop pORT
