{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Matrix event data type
module Network.Matrix.Events
  ( MessageTextType (..),
    MessageText (..),
    RoomMessage (..),
    RoomMember (..),
    Membership (..),
    Event (..),
    EventID (..),
    eventType,
  )
where

import Control.Applicative ((<|>))
import Control.Monad (mzero)
import Data.Aeson (FromJSON (..), Object, ToJSON (..), Value (Object, String), object, (.:), (.:?), (.=), genericParseJSON, genericToJSON, withObject)
import Data.Aeson.Casing (aesonPrefix, snakeCase)
import Data.Aeson.Casing.Internal (dropFPrefix, applyFirst)
import Data.Char (toLower)
import Data.Aeson.Types (Pair, Parser)
import Data.Text (Text)
import GHC.Generics (Generic)
import qualified Data.Aeson as Aeson

data MessageTextType
  = TextType
  | EmoteType
  | NoticeType
  deriving (Eq, Show)

instance FromJSON MessageTextType where
  parseJSON (String name) = case name of
    "m.text" -> pure TextType
    "m.emote" -> pure EmoteType
    "m.notice" -> pure NoticeType
    _ -> mzero
  parseJSON _ = mzero

instance ToJSON MessageTextType where
  toJSON mt = String $ case mt of
    TextType -> "m.text"
    EmoteType -> "m.emote"
    NoticeType -> "m.notice"

data MessageText = MessageText
  { mtBody :: Text,
    mtType :: MessageTextType,
    mtFormat :: Maybe Text,
    mtFormattedBody :: Maybe Text
  }
  deriving (Show, Eq)

instance FromJSON MessageText where
  parseJSON (Object v) =
    MessageText
      <$> v .: "body"
        <*> v .: "msgtype"
        <*> v .:? "format"
        <*> v .:? "formatted_body"
  parseJSON _ = mzero

instance ToJSON MessageText where
  toJSON = object . messageTextAttr

messageTextAttr :: MessageText -> [Pair]
messageTextAttr msg =
  ["body" .= mtBody msg, "msgtype" .= mtType msg] <> format <> formattedBody
  where
    omitNull k vM = maybe [] (\v -> [k .= v]) vM
    format = omitNull "format" $ mtFormat msg
    formattedBody = omitNull "formatted_body" $ mtFormattedBody msg

data ImageInfo = ImageInfo
    { iiH :: Maybe Int
    -- ^  The intended display height of the image in pixels. This may differ from the intrinsic dimensions of the image file.
    , iiW :: Maybe Int
    -- ^  The intended display width of the image in pixels. This may differ from the intrinsic dimensions of the image file.
    , iiMimetype :: Maybe Text
    -- ^ The mimetype of the image, e.g. image/jpeg.
    , iiSize :: Maybe Int
    -- ^  Size of the image in bytes.
    }
  deriving (Generic, Show, Eq)

instance FromJSON ImageInfo where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON ImageInfo where
    toJSON = genericToJSON aesonOptions

data MessageImage = MessageImage
  { miBody :: Text,
    miFile :: Maybe Text,
    miFilename :: Maybe Text,
    miFormat :: Maybe Text,
    miFormattedBody :: Maybe Text,
    miInfo :: Maybe ImageInfo
  }
  deriving (Generic, Show, Eq)

instance FromJSON MessageImage where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON MessageImage where
    toJSON = object . messageImageAttr

messageImageAttr :: MessageImage -> [Pair]
messageImageAttr msg =
  ["body" .= miBody msg, "msgtype" .= ("m.image" :: Text)] <> format <> formattedBody
  where
    omitNull k vM = maybe [] (\v -> [k .= v]) vM
    format = omitNull "format" $ miFormat msg
    formattedBody = omitNull "formatted_body" $ miFormattedBody msg

data RoomMessage
  = RoomMessageText MessageText
  | RoomMessageImage MessageImage
  deriving (Show, Eq)

roomMessageAttr :: RoomMessage -> [Pair]
roomMessageAttr rm = case rm of
  RoomMessageText mt -> messageTextAttr mt
  RoomMessageImage mi -> messageImageAttr mi

instance ToJSON RoomMessage where
  toJSON msg = case msg of
    RoomMessageText mt -> toJSON mt
    RoomMessageImage mi -> toJSON mi

instance FromJSON RoomMessage where
  parseJSON x = flip (withObject "RoomMessage") x $ \o -> do
    eventType <- (o .: "msgtype") :: Parser Text
    case eventType of
        "m.image" -> RoomMessageImage <$> parseJSON x
        _ -> RoomMessageText <$> parseJSON x

data RelatedMessage = RelatedMessage
  { rmMessage :: RoomMessage,
    rmRelatedTo :: EventID
  }
  deriving (Show, Eq)

aesonOptions :: Aeson.Options
aesonOptions = (aesonPrefix snakeCase) {Aeson.omitNothingFields = True, Aeson.constructorTagModifier = snakeCase . dropFPrefix . applyFirst toLower}

data Membership
    = MembershipInvite
    | MembershipJoin
    | MembershipKnock
    | MembershipLeave
    | MembershipBan
  deriving (Generic, Show, Eq)

instance FromJSON Membership where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON Membership where
    toJSON = genericToJSON aesonOptions

newtype RoomMember = RoomMember
    { rmMembership :: Membership
    }
  deriving (Generic, Show, Eq)

instance FromJSON RoomMember where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON RoomMember where
    toJSON = genericToJSON aesonOptions

data Event
  = EventRoomMessage RoomMessage
  | -- | A reply defined by the parent event id and the reply message
    EventRoomReply EventID RoomMessage
  | -- | An edit defined by the original message and the new message
    EventRoomEdit (EventID, RoomMessage) RoomMessage
  | EventRoomMember RoomMember
  | EventUnknown Object
  deriving (Eq, Show)

instance ToJSON Event where
  toJSON event = case event of
    EventRoomMessage msg -> toJSON msg
    EventRoomReply eventID msg ->
      let replyAttr =
            [ "m.relates_to"
                .= object
                  [ "m.in_reply_to" .= toJSON eventID
                  ]
            ]
       in object $ replyAttr <> roomMessageAttr msg
    EventRoomEdit (EventID eventID, msg) newMsg ->
      let editAttr =
            [ "m.relates_to"
                .= object
                  [ "rel_type" .= ("m.replace" :: Text),
                    "event_id" .= eventID
                  ],
              "m.new_content" .= object (roomMessageAttr newMsg)
            ]
       in object $ editAttr <> roomMessageAttr msg
    EventRoomMember member -> toJSON member
    EventUnknown v -> Object v

instance FromJSON Event where
  parseJSON (Object content) =
    parseRelated <|> parseMessage <|> parseMember <|> pure (EventUnknown content)
    where
      parseMessage = EventRoomMessage <$> parseJSON (Object content)
      parseMember = EventRoomMember <$> parseJSON (Object content)
      parseRelated = do
        relateM <- content .: "m.relates_to"
        case relateM of
          Object relate -> parseReply relate <|> parseReplace relate
          _ -> mzero
      parseReply relate =
        EventRoomReply <$> relate .: "m.in_reply_to" <*> parseJSON (Object content)
      parseReplace relate = do
        rel_type <- relate .: "rel_type"
        if rel_type == ("m.replace" :: Text)
          then do
            ev <- EventID <$> relate .: "event_id"
            msg <- parseJSON (Object content)
            EventRoomEdit (ev, msg) <$> content .: "m.new_content"
          else mzero
  parseJSON _ = mzero

eventType :: Event -> Text
eventType event = case event of
  EventRoomMessage _ -> "m.room.message"
  EventRoomReply _ _ -> "m.room.message"
  EventRoomEdit _ _ -> "m.room.message"
  EventRoomMember _ -> "m.room.member"
  EventUnknown _ -> error $ "Event is not implemented: " <> show event

newtype EventID = EventID {unEventID :: Text} deriving (Show, Eq, Ord)

instance FromJSON EventID where
  parseJSON (Object v) = EventID <$> v .: "event_id"
  parseJSON _ = mzero

instance ToJSON EventID where
  toJSON (EventID v) = object ["event_id" .= v]
