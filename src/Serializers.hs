{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
module Serializers where

import Model

import GHC.Generics
import Data.Aeson

instance FromJSON PostalCodeOverrideRate
instance ToJSON PostalCodeOverrideRate

instance FromJSON Request

instance ToJSON Response

instance FromJSON RateDef
instance ToJSON RateDef

deriving instance Generic PostalCodeOverrideRate

deriving instance Generic RateDef

deriving instance Generic Request

deriving instance Generic Response
