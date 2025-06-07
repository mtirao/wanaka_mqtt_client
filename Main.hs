{-# LANGUAGE OverloadedStrings #-}


import Web.Scotty
import Network.Wai.Middleware.RequestLogger (logStdoutDev, logStdout)
import Data.Text (Text, unpack, pack)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Aeson (Value)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as S8
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as C
import Network.MQTT.Client
import Network.URI (parseURI)
import Network.HTTP.Client



bsLazyToText :: BL.ByteString -> [Char]
bsLazyToText = TL.unpack . TLE.decodeUtf8 

makeRequest :: IO ()
makeRequest = do
    manager <- newManager defaultManagerSettings
    let request = "http://localhost:8080/api/profile"
    response <- httpLbs request manager
    putStrLn $ "Response status: " ++ show (responseStatus response)


msgReceived :: MQTTClient -> Topic -> BL.ByteString -> [Property] -> IO ()
msgReceived _ topic message property = do
    makeRequest
    putStrLn $ "Message: " ++ bsLazyToText message

main :: IO ()
main = do
  let (Just uri) = parseURI "mqtt://192.168.0.56"
  mc <- connectURI mqttConfig{_msgCB=SimpleCallback msgReceived} uri
  print =<< subscribe mc [("tmp/topic1", subOptions), ("tmp/topic2", subOptions)] []
  waitForClient mc   -- wait for the the client to disconnect

 -- where
 --   msgReceived _ t m p = print (t,m,p)

