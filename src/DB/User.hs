{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module DB.User where

import           Control.Category (id)
import           Control.Lens
import           Data.ByteString
import           Data.Proxy
import           Data.Tagged
import           Data.Text
import qualified Data.Time as Time
import           Data.Time (Day, LocalTime)
import           Data.Int
import           Tisch
import           User
import           Prelude hiding (id)
--------------------------------------------------------------------------------
-- User
--------------------------------------------------------------------------------


data TUser

data instance Table TUser = TUser
type instance Database TUser = DB
type instance SchemaName TUser = "public"
type instance TableName TUser = "users"

type instance Columns TUser =
  '[ 'Column "id" 'W  'R UserId UserId
   , 'Column "username" 'W  'R Username Username
   , 'Column "email" 'W  'R PGText Text
   ]

mkUserForInsert :: User -> HsI TUser
mkUserForInsert usr = mkHsI TUser
  (Tagged @"id" (userId usr))
  (Tagged @"username" (userUsername usr))
  (Tagged @"email" (userEmail usr))


--insertUser :: IO ()
--insertUser = do
--  pg <- connect' "host=\'localhost\' user=\'postgres\' port=5432 password=\'\' dbname=\'iam\'"
--  runInsert1 pg TUser $ mkUserForInsert sampleUser
