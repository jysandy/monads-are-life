{-# LANGUAGE OverloadedStrings #-}

module Monad
  ( fetchAll
  , create
  , delete
  , fetchByID
  )
where

import           Prelude                 hiding ( Monad )
import           Control.Exception
import qualified Database.PostgreSQL.Simple    as PSQL
import qualified Data.Text                     as Text
import           Data.Text                      ( Text )

data Monad = Monad {id :: Integer,
                    name :: Text,
                    description :: Text,
                    rating :: Int}
                    deriving (Eq, Show)

connect :: IO PSQL.Connection
connect = PSQL.connectPostgreSQL "dbname='monads_are_life'"

withConn :: (PSQL.Connection -> IO a) -> IO a
withConn = bracket connect PSQL.close

withTransaction :: (PSQL.Connection -> IO a) -> IO a
withTransaction f = withConn $ (\conn -> PSQL.withTransaction conn (f conn))

fetchAll :: IO [Monad]
fetchAll = do
  rows <- withTransaction
    $ \tx -> PSQL.query_ tx "select id, name, description, rating from monads"
  return $ map monadFromTuple rows
 where
  monadFromTuple (id, name, description, rating) =
    Monad id name description rating

create :: Text -> Text -> Int -> IO Monad
create name description rating = do
  [PSQL.Only id] <- withTransaction $ \tx -> PSQL.query
    tx
    "insert into monads (name, description, rating) values (?,?,?) RETURNING id"
    (name, description, rating)
  return $ Monad id name description rating

delete :: Integer -> IO Monad
delete id = do
  [(name, description, rating)] <- withTransaction $ \tx -> PSQL.query
    tx
    "delete from monads where id = ? RETURNING name, description, rating"
    (PSQL.Only id)
  return $ Monad id name description rating

fetchByID :: Integer -> IO (Maybe Monad)
fetchByID id = do
  rows <- withTransaction $ \tx -> PSQL.query
    tx
    "select name, description, rating from monads where id = ?"
    (PSQL.Only id)
  case rows of
    [] -> return Nothing
    [(name, description, rating)] ->
      return . Just $ Monad id name description rating
