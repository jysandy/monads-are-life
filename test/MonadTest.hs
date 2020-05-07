{-# LANGUAGE OverloadedStrings #-}

module MonadTest
  ( monadTests
  )
where

import           Test.Tasty
import           Test.Tasty.HUnit
import           Prelude                 hiding ( Monad )
import qualified Database.PostgreSQL.Simple    as PSQL
import qualified DB
import qualified Monad
import qualified Data.Set                      as Set

monadTests =
  testGroup "Testing monad CRUD functions"
    $ [ testCase "create" $ do
        clearDB
        createdMonad <- Monad.create "name" "description" 3
        Monad.name createdMonad @?= "name"
        Monad.description createdMonad @?= "description"
        Monad.rating createdMonad @?= 3

        [(id, name, rating, description)] <- DB.withTransaction $ \tx ->
          PSQL.query_ tx "select id, name, description, rating from monads"
        assertBool
          "The returned monad should be the same as the monad in the DB"
          (createdMonad == Monad.Monad id name rating description)
      , after AllFinish "create" $ testCase "fetchAll" $ do
        clearDB
        monad1    <- Monad.create "name" "description" 3
        monad2    <- Monad.create "name2" "description2" 4
        allMonads <- Monad.fetchAll
        Set.fromList allMonads @?= Set.fromList [monad1, monad2]
      , after AllFinish "fetchAll"
      $ testCase "fetchByID when the data exists"
      $ do
          clearDB
          monad1               <- Monad.create "name" "description" 3
          monad2               <- Monad.create "name2" "description2" 4
          (Just fetchedMonad1) <- Monad.fetchByID (Monad.id monad1)
          fetchedMonad1 @?= monad1
          (Just fetchedMonad2) <- Monad.fetchByID (Monad.id monad2)
          fetchedMonad2 @?= monad2
      , after AllFinish "fetchByID when the data exists"
      $ testCase "fetchByID when the data is absent"
      $ do
          clearDB
          fetchedMonad <- Monad.fetchByID 0
          fetchedMonad @?= Nothing
      , after AllFinish "fetchByID when the data is absent"
      $ testCase "delete when the data is present"
      $ do
          clearDB
          createdMonad        <- Monad.create "name" "description" 3
          (Just deletedMonad) <- Monad.delete (Monad.id createdMonad)
          deletedMonad @?= createdMonad
          maybeAMonad <- Monad.fetchByID (Monad.id createdMonad)
          maybeAMonad @?= Nothing
      , after AllFinish "delete when the data is present"
      $ testCase "delete when the data is absent"
      $ do
          clearDB
          maybeDeletedMonad <- Monad.delete 0
          maybeDeletedMonad @?= Nothing
      ]

clearDB :: IO ()
clearDB = do
  conn <- PSQL.connectPostgreSQL "dbname='monads_are_life'"
  PSQL.execute_ conn "truncate monads"
  PSQL.close conn
