{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Monad
  ( fetchAll
  , create
  , delete
  , fetchByID
  , Monad(..)
  )
where

import           Prelude                 hiding ( Monad
                                                , id
                                                )
import qualified DB
import qualified Database.PostgreSQL.Simple    as PSQL
import qualified Data.Text                     as Text
import           Data.Text                      ( Text )
import           GHC.Generics
import           Data.Aeson

data Monad = Monad {id :: Integer,
                    name :: Text,
                    description :: Text,
                    rating :: Int}
                    deriving (Eq, Show, Ord, Generic)

instance ToJSON Monad

fetchAll :: IO [Monad]
fetchAll = do
  rows <- DB.withTransaction
    $ \tx -> PSQL.query_ tx "select id, name, description, rating from monads"
  return $ map monadFromTuple rows
 where
  monadFromTuple (id, name, description, rating) =
    Monad id name description rating

create :: Text -> Text -> Int -> IO Monad
create name description rating = do
  [PSQL.Only id] <- DB.withTransaction $ \tx -> PSQL.query
    tx
    "insert into monads (name, description, rating) values (?,?,?) RETURNING id"
    (name, description, rating)
  return $ Monad id name description rating

delete :: Integer -> IO (Maybe Monad)
delete id = do
  rows <- DB.withTransaction $ \tx -> PSQL.query
    tx
    "delete from monads where id = ? RETURNING name, description, rating"
    (PSQL.Only id)
  case rows of
    [] -> return Nothing
    [(name, description, rating)] ->
      return . Just $ Monad id name description rating

fetchByID :: Integer -> IO (Maybe Monad)
fetchByID id = do
  rows <- DB.withTransaction $ \tx -> PSQL.query
    tx
    "select name, description, rating from monads where id = ?"
    (PSQL.Only id)
  case rows of
    [] -> return Nothing
    [(name, description, rating)] ->
      return . Just $ Monad id name description rating
