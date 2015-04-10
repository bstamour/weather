# weather
Haskell library for working with the Weather Underground JSON API.

Currently only supports querying for conditions for US-based cities.

Example usage:

```haskell
{-# LANGUAGE RecordWildCards #-}

module Main where

import Web.Weather

mykey :: APIKey
mykey   = "top-secret"

mycity, mystate :: String
mycity  = "Detroit"
mystate = "MI"

main :: IO ()
main = do
  resp <- getConditions mykey mycity mystate
  case resp of
   Nothing -> putStrLn "No data for that city/state"
   Just (Observation{..}) -> do
     putStrLn $ "Observation time: " ++ obsTime
     putStrLn $ "Weather conditions: " ++ obsWeather
     putStrLn $ "Temp: " ++ show obsTemp
     putStrLn $ "Rel humidity: " ++ show obsRelHumidity
     putStrLn $ "Wind: " ++ obsWind
     putStrLn $ "Feels like: " ++ obsFeelsLike
```