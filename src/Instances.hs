{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
module Instances where

import Model

import GHC.Generics
import Data.Aeson

instance FromJSON Rate
instance ToJSON Rate

instance FromJSON Request
instance ToJSON Request

instance ToJSON Response

instance FromJSON RateDef
instance ToJSON RateDef

deriving instance Generic Rate

deriving instance Generic RateDef

deriving instance Generic Request

deriving instance Generic Response
