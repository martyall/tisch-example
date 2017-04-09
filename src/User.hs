{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}

module User where

import Control.Lens
import Data.ByteString (ByteString)
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import Data.Proxy
import Data.String (IsString)
import Data.Tagged
import Data.Text
import Data.UUID (UUID, fromString)
import GHC.Generics (Generic)
import Tisch
import Types

--------------------------------------------------------------------------------
-- | User
--------------------------------------------------------------------------------

-- | UserId
newtype UserId = UserId {getUserId  :: UUID}
  deriving (Eq, Show, QueryRunnerColumnDefault PGUuid)

instance Sample UserId where
  sample _ =
    let (Just uuid) = fromString "550e8400-e29b-41d4-a716-446655440000"
    in UserId uuid

instance Wrapped UserId where
  type Unwrapped UserId = UUID
  _Wrapped' = iso getUserId UserId

instance PgTyped UserId where
  type PgType UserId = PGUuid

-- | Username
newtype Username = Username {getUsername :: Text}
  deriving (Eq, Show, QueryRunnerColumnDefault PGText, IsString)

instance Sample Username where
  sample _ = "Bob"

instance Wrapped Username where
  type Unwrapped Username = Text
  _Wrapped' = iso getUsername Username

instance PgTyped Username where
  type PgType Username = PGText

-- | User
data User =
  User { userId :: UserId
       , userUsername :: Username
       , userEmail :: Text
       }

instance Sample User where
  sample _ = User (sample $ Proxy @ UserId) (sample $ Proxy @ Username) ("bob@gmail.com")

-- | User Schema
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

instance HasTable User where
  type TableOf User = TUser
  toPG usr = mkHsI TUser
    (Tagged @"id" (userId usr))
    (Tagged @"username" (userUsername usr))
    (Tagged @"email" (userEmail usr))


--------------------------------------------------------------------------------
-- | UserAuth
--------------------------------------------------------------------------------

-- | UserAuth
data UserAuth =
  UserAuth { userAuthUserId :: UserId
           , userAuthPassword :: ByteString
           }

instance Sample UserAuth where
  sample _ = UserAuth (sample $ Proxy @ UserId) "MYSECRETPASSWORD"

-- | UserAuth Schema
data TUserAuth

data instance Table TUserAuth = TUserAuth
type instance Database TUserAuth = TUserAuth
type instance SchemaName TUserAuth = "public"
type instance TableName TUserAuth = "user_auth"

type instance Columns TUserAuth =
  '[ 'Column "user_id" 'W 'R UserId UserId
   , 'Column "password" 'W 'R ByteString ByteString
   ]

instance HasTable UserAuth where
  type TableOf UserAuth = TUserAuth
  toPG auth = mkHsI TUserAuth
    (Tagged @"user_id" (userAuthUserId auth))
    (Tagged @"password" (userAuthPassword auth))
