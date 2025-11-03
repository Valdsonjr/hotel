{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Colog.Core (Severity (..), unLogAction)
import Data.Text (Text)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import System.Log (Record (..), withHotelLogger, (.=), mkHttpConfig)

main :: IO ()
main = do
  manager <- newManager defaultManagerSettings
  let cfg = mkHttpConfig
        [ "service.name" .= ("hotel.tests" :: Text),
          "env" .= ("test" :: Text)
        ]
        ""
        10
        5
        manager
  withHotelLogger cfg $ \logger -> do
          
    unLogAction logger $
      Record
        { severity = Just Debug,
          time = Nothing,
          traceId = Nothing,
          spanId = Nothing,
          message = "Olá mundo",
          eventName = Just "greeting",
          attributes = []
        }
    unLogAction logger $
      Record
        { severity = Just Info,
          time = Nothing,
          traceId = Nothing,
          spanId = Nothing,
          message = "Olá mundo",
          eventName = Just "greeting",
          attributes = []
        }
    unLogAction logger $
      Record
        { severity = Just Warning,
          time = Nothing,
          traceId = Nothing,
          spanId = Nothing,
          message = "Olá mundo",
          eventName = Just "greeting",
          attributes = []
        }
    unLogAction logger $
      Record
        { severity = Just Error,
          time = Nothing,
          traceId = Nothing,
          spanId = Nothing,
          message = "Olá mundo",
          eventName = Just "greeting",
          attributes = []
        }
