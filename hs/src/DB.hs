{-# LANGUAGE OverloadedStrings #-}

module DB
  ( connect
  , withConn
  , withTransaction
  ) where

import           Control.Exception
import qualified Data.ByteString.Char8      as C
import qualified Database.PostgreSQL.Simple as PSQL

connect :: String -> IO PSQL.Connection
connect dbName = PSQL.connectPostgreSQL $ C.pack $ "dbname='" ++ dbName ++ "'"

withConn :: String -> (PSQL.Connection -> IO a) -> IO a
withConn dbName = bracket (connect dbName) PSQL.close

withTransaction :: String -> (PSQL.Connection -> IO a) -> IO a
withTransaction dbName f = withConn dbName (\conn -> PSQL.withTransaction conn (f conn))
