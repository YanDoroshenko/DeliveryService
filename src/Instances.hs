{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
module Instances where

import Model
import Utils (orElse)

import GHC.Generics
import Control.Applicative (empty)
import Control.Monad
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Maybe
import Data.Text (Text)

instance FromJSON Rate where
  parseJSON (Object v) =
    foldr (\l r -> l `combineMaybe` r) (return $ parseBaseDistanceRate v) [parse "postalCode" parsePostalCodeRate v, parse "locationId" parseLocationRate v, parse "stateCode" parseStateRate v] >>= Prelude.id
  parseJSON _ = empty


parse key f v = ((\p -> (\_ -> f v) <$> p) <$> ((v .:? key) :: Parser (Maybe String)))

combineMaybe :: Monad m => m (Maybe a) -> m a -> m a
combineMaybe l r =
  l >>= (\x -> case x of
           Just x_ -> return x_
           _ -> r)

maybeRate :: Object -> Text -> (Object -> Parser Rate) -> Parser Rate
maybeRate v key f =
  (v .:? key :: Parser (Maybe String)) >>= (\_ -> f v)

withId x y = y <$> (x .: "id")

withRate x y = y <*> (x .: "rate")

parsePostalCodeRate :: Object -> Parser Rate
parsePostalCodeRate v = withRate v $ (withId v PostalCodeOverrideRate) <*> v .: "postalCode"

parseLocationRate :: Object -> Parser Rate
parseLocationRate v = withRate v $ (withId v LocationOverrideRate) <*> v .: "locationId"

parseBaseDistanceRate :: Object -> Parser Rate
parseBaseDistanceRate v = withRate v $ (withId v BaseDistanceRate) <*> v .:? "distanceFrom" <*> v .:? "distanceTo"

parseStateRate :: Object -> Parser Rate
parseStateRate v = withRate v $ (withId v StateOverrideRate) <*> v .: "stateCode"

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
