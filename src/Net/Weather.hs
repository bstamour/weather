{-# LANGUAGE OverloadedStrings #-}

module Net.Weather
       ( Observation(..)
       , APIKey
       , getConditions
       ) where

import Control.Monad
import Control.Applicative

import Data.Text (Text)
import Data.Aeson
import Data.ByteString.Lazy.Char8 (pack)
import Network.HTTP (getResponseBody, simpleHTTP, getRequest)

import qualified Data.HashMap.Strict as H

-- | Observation data.
data Observation =
  Observation { obsTime        :: String  -- ^ The time the observation was taken.
              , obsWeather     :: String  -- ^ Description of the weather.
              , obsTemp        :: Float   -- ^ Temperature (F).
              , obsRelHumidity :: String  -- ^ Relative humidity (%).
              , obsWind        :: String  -- ^ Wind condition.
              , obsFeelsLike   :: String  -- ^ What it feels like.
              } deriving (Show)

instance FromJSON Observation where
  parseJSON (Object v) = Observation
                         <$> v .: "observation_time"
                         <*> v .: "weather"
                         <*> v .: "temp_f"
                         <*> v .: "relative_humidity"
                         <*> v .: "wind_string"
                         <*> v .: "feelslike_string"
  parseJSON _          = mzero

-- | API key. Obtain yours at http://wunderground.com.
type APIKey = String

-- | Get the current weather conditions of a city.
getConditions :: APIKey -> String -> String -> IO (Maybe Observation)
getConditions key city state = do
  obj <- evalJSONRequest $ conditionsQuery key city state
  return $ obj >>= getProperty "current_observation"

-- Fetch a property from a JSON object.
getProperty :: FromJSON a => Text -> Value -> Maybe a
getProperty property (Object v) = do
  val <- H.lookup property v
  case fromJSON val of
   Error _   -> Nothing
   Success x -> Just x
getProperty _ _                 = Nothing

-- Evaluate a JSON request and return the parsed object.
evalJSONRequest :: FromJSON a => String -> IO (Maybe a)
evalJSONRequest request = do
  body <- getResponseBody <=< simpleHTTP $ getRequest request
  return . decode $ pack body

-- The query used to get weather conditions.
conditionsQuery :: String -> String -> String -> String
conditionsQuery key city state =
  "http://api.wunderground.com/api/" ++ key
  ++ "/conditions/q/" ++ state
  ++ "/" ++ city ++ ".json"
