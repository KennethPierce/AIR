-- Necessary:
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}

-- Incidental:
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Control.Monad
import Data.Hashable
import Data.List
import Data.Text (Text)
import Data.Traversable (for)
import Data.Typeable
import Haxl.Core
import System.Random
import Lib

import qualified Data.Text as Text

main :: IO ()
main = selfPlaysIO

main1 :: IO ()
main1 = do
  let stateStore = stateSet UserState{} stateEmpty
  env0 <- initEnv stateStore ()
  names <- runHaxl env0 getAllUsernames
  print names

-- Data source API.

getAllUsernames :: Haxl [Name]
getAllUsernames = do
  userIds <- getAllUserIds  
  for userIds $ \userId -> do
    aliasId <- getAliasTricky userId
    getUsernameById aliasId

getAliasTricky :: Id -> Haxl Id
getAliasTricky userId = do
  userId1 <- getAlias userId
  userId2  <- getAlias userId1
  let newId = if mod userId 2 == 0 then userId1 else userId2
  pure newId


getAlias :: Id -> Haxl Id
getAlias userId = dataFetch (GetAlias userId)

getAllUserIds :: Haxl [Id]
getAllUserIds = dataFetch GetAllIds

getUsernameById :: Id -> Haxl Name
getUsernameById userId = dataFetch (GetNameById userId)

-- Aliases.

type Haxl = GenHaxl ()
type Id = Int
type Name = Text

-- Data source implementation.

data UserReq a where
  GetAllIds   :: UserReq [Id]
  GetAlias    :: Id -> UserReq Id
  GetNameById :: Id -> UserReq Name
  deriving (Typeable)

deriving instance Eq (UserReq a)
instance Hashable (UserReq a) where
   hashWithSalt s GetAllIds       = hashWithSalt s (0::Int)
   hashWithSalt s (GetNameById a) = hashWithSalt s (1::Int, a)
   hashWithSalt s (GetAlias a) = hashWithSalt s (2::Int, a)

deriving instance Show (UserReq a)
instance ShowP UserReq where showp = show

instance StateKey UserReq where
  data State UserReq = UserState {}

instance DataSourceName UserReq where
  dataSourceName _ = "UserDataSource"

instance DataSource u UserReq where
--  fetch :: State req -> Flags -> u -> [BlockedFetch req] -> PerformFetch
  fetch _state _flags _userEnv blockedFetches = SyncFetch $  do
    let
      allIdVars :: [ResultVar [Id]]
      allIdVars = [r | BlockedFetch GetAllIds r <- blockedFetches]
      doAllIdVars :: IO ()
      doAllIdVars = do
        --allIds <- sql "select id from ids"
        let allIds = [1..5]
        putStrLn ("allIdVars " ++ (show allIds))
        mapM_ (\r -> putSuccess r allIds) allIdVars

    unless (null allIdVars) doAllIdVars
  
    let
      params :: [Id]
      aliases :: [ResultVar Id]
      (params,aliases) = unzip 
        [(userId, r) | BlockedFetch (GetAlias userId) r <- blockedFetches]

      doAliases :: IO ()
      doAliases = do 
        --aliasIds <- sql ("alias ids " ++ show (params))
        let aliasIds = [i+101 | i <- params]
        putStrLn ("alias " ++ (show aliasIds))
        mapM_ (uncurry putSuccess) (zip aliases aliasIds)
    unless (null aliases)  doAliases

    let
      ids :: [Id]
      vars :: [ResultVar Name]
      (ids, vars) = unzip
        [(userId, r) | BlockedFetch (GetNameById userId) r <- blockedFetches]

      idStrings :: [String]
      idStrings = map show ids
      
      doGetNameById :: IO ()
      doGetNameById = do
        let names = ["Jim_" | _ <- idStrings]
        putStrLn ("getnamebyid " ++ (show ids))
        mapM_ (uncurry putSuccess) (zip vars names)
    unless (null ids) doGetNameById

