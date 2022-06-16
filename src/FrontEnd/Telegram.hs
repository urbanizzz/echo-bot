{-# LANGUAGE OverloadedStrings #-}

module FrontEnd.Telegram
  ( run
  , Handle
  , textFromMessage
  , messageFromText
  )
where

import qualified Data.Text as T
import qualified EchoBot
import qualified Logger
import Control.Monad (forever)
import Data.Aeson ((.=),(.:),(.:?),(.!=))
import qualified Data.Aeson as A
import qualified Data.Map.Strict as M
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Text.Read (readMaybe)
import qualified Network.HTTP.Simple as Simple
import Network.HTTP.Client.Internal ( Request(method,  responseTimeout), ResponseTimeout(ResponseTimeoutNone) )
import Data.ByteString.Char8 (ByteString)

type UpdateId = Int
type MessageId = Int
type UserId = Int
type ChatId = Int
type Handle = EchoBot.Handle IO Message
type HandleMap = M.Map UserId Handle

data Message 
  = PlainText T.Text
  | Sticker A.Object
  | Picture A.Array

data Update = Update UpdateId MessageId UserId ChatId Message
instance A.FromJSON Update where
  parseJSON = A.withObject "FromJSON Update" $ \o -> Update
    <$> (o .: "update_id")
    <*> (o .: "message" >>= (.: "message_id"))
    <*> (o .: "message" >>= (.: "from") >>= (.: "id"))
    <*> (o .: "message" >>= (.: "chat") >>= (.: "id"))
    <*> (PlainText <$> (o .: "message" >>= \o1 -> o1 .:? "text" .!= ""))
-- instance A.ToJSON Message where
--  toJSON (Message _ chat_id txt) = object [ "chat_id" .= chat_id, "text" .= txt ]

data Updates = Updates [Update]
instance A.FromJSON Updates where
  parseJSON = A.withObject "FromJSON Updates" $ \o -> Updates
    <$> (o .: "result")

textFromMessage :: Message -> Maybe T.Text
textFromMessage (PlainText x) = Just x
textFromMessage (Sticker _) = Nothing
textFromMessage (Picture _) = Nothing

messageFromText :: T.Text -> Message
messageFromText = PlainText

botURL :: String
botURL = "https://api.telegram.org/bot949284451:AAGK8fCgIhv2KLcmT8Mz_bf-3hAl0Ccp7pA/"

tgTimeout ::String 
tgTimeout = "60"

getUpdates :: Handle -> UpdateId -> IO [Update]
getUpdates h lastUpdateId = do
  rawJSON <- getUpdatesAsJSON lastUpdateId
  let eitherResult = A.eitherDecodeStrict' rawJSON :: Either String Updates
  result <- eitherToUpdate h eitherResult
  return result

eitherToUpdate :: Handle -> Either String Updates -> IO [Update]
eitherToUpdate h eitherResult = case eitherResult of
  Left err -> do
    Logger.logWarning (EchoBot.hLogHandle h) $ (T.pack err)
    pure []
  Right (Updates updates) -> pure updates

getUpdatesAsJSON :: UpdateId -> IO ByteString
getUpdatesAsJSON lastUpdateId = do
  let requestObject = A.object ["offset" .= show lastUpdateId, "timeout" .= tgTimeout] 
  initialRequest <- Simple.parseRequest $ botURL ++ "getUpdates"
  let requestWithBody = Simple.setRequestBodyJSON requestObject initialRequest
  let request = requestWithBody
        { method = "POST"
        , responseTimeout = ResponseTimeoutNone
        }
  res <- Simple.httpBS request
  return $ Simple.getResponseBody res

sendMessage :: Handle -> UserId -> ChatId -> Message -> IO ()
sendMessage = undefined

insertNewUser :: UserId -> Handle -> HandleMap -> HandleMap
insertNewUser = M.insert

proceedUpdate :: Handle -> IORef HandleMap -> Update -> IO ()
-- proceedUpdate newUserHandle handleMapRef (Update lastUpdateId messageId userId chatId message) = do
proceedUpdate newUserHandle handleMapRef (Update _ _ userId chatId message) = do
  handleMap <- readIORef handleMapRef
  modifyIORef' handleMapRef $ 
    if userId `M.notMember` handleMap
      then insertNewUser userId newUserHandle
      else id
  handleMap' <- readIORef handleMapRef
  let handle = handleMap' M.! userId
  response <- EchoBot.respond handle (EchoBot.MessageEvent message)
  mapM_ (handleResponse handle userId chatId) response

run :: Handle -> IO ()
run newUserHandle = do
  handleMapRef <- newIORef (M.empty :: HandleMap)
  lastUpdateIdRef <- newIORef (0 :: Int)
  forever $ do
    lastUpdateId <- readIORef lastUpdateIdRef
    updates <- getUpdates newUserHandle lastUpdateId
    mapM_ (proceedUpdate newUserHandle handleMapRef) updates

type RepetitionCount = Int

getNumber :: Handle -> UserId -> ChatId -> IO T.Text
getNumber = undefined

handleResponse :: Handle -> UserId -> ChatId -> EchoBot.Response Message -> IO ()
handleResponse h userId chatId (EchoBot.MessageResponse x) = sendMessage h userId chatId x
handleResponse h userId chatId (EchoBot.MenuResponse title options) = do
  sendMessage h userId chatId $ PlainText title
  rawText <- getNumber h userId chatId
  let number = getNumberFromRawText rawText
  let maybeEvent = M.lookup number $ M.fromList options
  _ <- maybe (pure []) (EchoBot.respond h) maybeEvent
  pure ()

getNumberFromRawText :: T.Text -> RepetitionCount
getNumberFromRawText text = case (readMaybe . T.unpack $ text) of
  Just x -> x
  _      -> 0

