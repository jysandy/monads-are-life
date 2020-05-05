{-# LANGUAGE OverloadedStrings #-}

module Monad
  ( fetchAll
  )
where

import           Prelude                 hiding ( Monad )
import qualified Database.PostgreSQL.Simple    as PSQL
import qualified Data.Text                     as Text
import           Data.Text                      ( Text )

data Monad = Monad {id :: Integer,
                    name :: Text,
                    description :: Text,
                    rating :: Int}
                    deriving (Eq, Show)

showText :: Show a => a -> Text
showText = Text.pack . show

connect :: IO PSQL.Connection
connect = PSQL.connectPostgreSQL "dbname='monads_are_life'"

fetchAll :: IO [Monad]
fetchAll = do
  conn <- connect
  rows <- PSQL.withTransaction conn
    $ PSQL.query_ conn "select id, name, description, rating from monads"
  PSQL.close conn
  return $ map monadFromTuple rows
 where
  monadFromTuple (id, name, description, rating) =
    Monad id name description rating

create :: Text -> Text -> Int -> IO Monad
create name description rating = do
  conn           <- connect
  [PSQL.Only id] <- PSQL.withTransaction conn $ PSQL.query
    conn
    "insert into monads (name, description, rating) values (?,?,?) RETURNING id"
    (name, description, rating)
  return $ Monad id name description rating
