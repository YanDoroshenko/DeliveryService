{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
module Serializers where

import Model

import GHC.Generics
import Data.Aeson

deriving instance Generic RateDef

instance FromJSON RateDef
instance ToJSON RateDef
