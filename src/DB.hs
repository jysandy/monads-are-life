{-# LANGUAGE OverloadedStrings #-}

module DB
  ( connect
  , withConn
  , withTransaction
  )
where

import           Control.Exception
import qualified Database.PostgreSQL.Simple    as PSQL

connect :: IO PSQL.Connection
connect = PSQL.connectPostgreSQL "dbname='monads_are_life'"

withConn :: (PSQL.Connection -> IO a) -> IO a
withConn = bracket connect PSQL.close

withTransaction :: (PSQL.Connection -> IO a) -> IO a
withTransaction f = withConn $ (\conn -> PSQL.withTransaction conn (f conn))
