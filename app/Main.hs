{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import User
import Data.Proxy
import Types
import Tisch

main :: IO ()
main = do
  pg <- connect' "host=\'localhost\' user=\'postgres\' port=5432 password=\'\' dbname=\'iam\'"
  let usr = sample $ Proxy @User
  runInsert1 pg TUser $ toPG usr
