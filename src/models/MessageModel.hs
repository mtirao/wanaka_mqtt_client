{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE RecordWildCards       #-}
{-# language DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}


module MessageModel where

import Data.Text 
import Data.Aeson
import GHC.Int
import Data.UUID (UUID)
import GHC.Generics (Generic)

data MessageType = Info | Fail | Warn | Alert
    deriving (Eq, Enum, Bounded, Generic)
    deriving stock (Read, Show)


instance FromJSON MessageType where
    parseJSON = withText "MessageType" $ \t -> case t of
        "Info"  -> pure Info
        "Error" -> pure Fail
        "Warn"  -> pure Warn
        "Alert" -> pure Alert
        _       -> fail $ "Unknown MessageType: " ++ show t

instance ToJSON MessageType where
    toJSON Info  = String "Info"
    toJSON Fail = String "Error"
    toJSON Warn  = String "Warn"
    toJSON Alert = String "Alert"

-- Message Request
data MessageModel = MessageModel
    { messageContent :: Text
    ,  messageDate :: Int64
    ,  messageType :: MessageType
    , messageId :: Maybe UUID
    } deriving (Show)

instance FromJSON MessageModel where
    parseJSON (Object v) = MessageModel <$>
        v .: "messageContent" <*>
        v .: "messageDate" <*>
        v .: "messageType" <*>
        v .:? "messageId"

instance ToJSON MessageModel where
    toJSON (MessageModel {..}) = object
        [ "messageId" .= messageId
        , "messageContent" .= messageContent
        , "messageDate" .= messageDate
        , "messageType" .= messageType
        ]

fromMessageType :: MessageType -> Text     
fromMessageType Info  = "info"
fromMessageType Fail = "error"
fromMessageType Warn  = "warn"
fromMessageType Alert = "alert"

toMessageType :: Text -> Maybe MessageType
toMessageType "info"  = Just Info
toMessageType "error" = Just Fail
toMessageType "warn"  = Just Warn
toMessageType "alert" = Just Alert
toMessageType _       = Nothing