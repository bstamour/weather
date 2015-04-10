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

Output:

```
Observation time: Last Updated on April 10, 2:09 PM EDT
Weather conditions: Partly Cloudy
Temp: 52.9
Rel humidity: "60%"
Wind: From the West at 4.7 MPH
Feels like: 52.9 F (11.6 C)
```
