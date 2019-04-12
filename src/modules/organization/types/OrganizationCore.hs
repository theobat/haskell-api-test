{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}

module Modules.Organization.Types.OrganizationCore  where

import Protolude hiding (Enum)

import qualified Data.Aeson as Aeson

import           Data.Data
import           GHC.Generics
import qualified Data.ByteString               as B
import           Data.Generics.Twins            ( gzipWithT
                                                )
import qualified Data.Generics as G
import           GraphQL.Parser
import           GraphQL.AST
import           Text.RawString.QQ


newtype T = T {
  id :: Text
}