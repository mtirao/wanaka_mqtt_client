{-# LANGUAGE OverloadedStrings #-}


import Web.Scotty
import Network.Wai.Middleware.RequestLogger (logStdoutDev, logStdout)
import Data.Text (Text, unpack, pack)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Aeson (Value, fromJSON, encode)
import Data.UUID.V4 (nextRandom)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as S8
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import Data.Time.Clock.POSIX (getPOSIXTime, utcTimeToPOSIXSeconds)
import Data.Time.Clock (getCurrentTime)
import Network.MQTT.Client
import Network.MQTT.Topic (unTopic)
import Network.URI (parseURI)
import Network.HTTP.Client
import MessageModel (MessageModel(..), MessageType(..))
import Control.Monad.Trans.Class (MonadTrans(lift))
import GHC.Int

bsLazyToText :: BL.ByteString -> [Char]
bsLazyToText = TL.unpack . TLE.decodeUtf8 

makeRequest :: MessageModel -> IO ()
makeRequest msgModel = do
    manager <- newManager defaultManagerSettings
    initialRequest <- parseRequest "http://localhost:3000/api/wanaka/message"
    let request = initialRequest {
            method = "POST",
            requestBody = RequestBodyLBS (encode msgModel),
            requestHeaders = [("Content-Type", "application/json"), ("Authorization", "Bearer eyJhbGciOiJIUzI1NiJ9.eyJleHAiOjE3NTc2MDYyMjAsInVzZXIiOiIzYmQyMzlkNC0wM2QxLTQ3Y2UtYWUyMC1kYjE1YjIzOTgwMTEifQ.NMx9rryCuxFzDKNxBymI4fVYNJKSwHiM3IEdcm4c2a4")]
        }
    response <- httpLbs request manager
    putStrLn $ "Response status: " ++ show (responseStatus response)



msgReceived :: MQTTClient -> Topic -> BL.ByteString -> [Property] -> IO ()
msgReceived _ topic message property = do
    let topicName = unTopic topic  -- Convert ByteString to String
    currentTime <- getCurrentTime
    let posixTime = round (utcTimeToPOSIXSeconds currentTime) :: Int64
    uuid <- nextRandom
    case topicName of
      "alert" -> makeRequest (MessageModel (decodeUtf8 $ BL.toStrict message) posixTime Alert (Just uuid))
      "info" -> makeRequest (MessageModel (decodeUtf8 $ BL.toStrict message) posixTime Info (Just uuid))
      "error" -> makeRequest (MessageModel (decodeUtf8 $ BL.toStrict message) posixTime Fail (Just uuid))
      "warn" -> makeRequest (MessageModel (decodeUtf8 $ BL.toStrict message) posixTime Warn (Just uuid))


  --  makeRequest
  --  putStrLn $ "Message: " ++ bsLazyToText message

main :: IO ()
main = do
  let (Just uri) = parseURI "mqtt://192.168.0.56"
  mc <- connectURI mqttConfig{_msgCB=SimpleCallback msgReceived} uri
  print =<< subscribe mc [("alert", subOptions), ("error", subOptions), ("info", subOptions), ("warn", subOptions)] []
  waitForClient mc   -- wait for the the client to disconnect


