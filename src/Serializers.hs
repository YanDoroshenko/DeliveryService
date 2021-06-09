{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
module Serializers where

import Model

import GHC.Generics
import Data.Aeson

instance FromJSON RateDef
instance ToJSON RateDef

instance FromJSON Request

instance ToJSON Response

deriving instance Generic RateDef

deriving instance Generic Request

deriving instance Generic Response
