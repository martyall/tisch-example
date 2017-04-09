{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Types where

import Control.Monad.IO.Class
import qualified Control.Monad.Catch as Cx
import qualified Opaleye as O
import Control.Monad.Reader.Class
import qualified Data.Profunctor.Product.Default as PP
import Data.Int (Int64)
import Data.Proxy
import Tisch

data DB

pDB :: Proxy DB
pDB = Proxy

class Sample a where
  sample :: Proxy a -> a

class HasTable k where
  type TableOf k :: *
  toPG :: k -> HsI (TableOf k)
