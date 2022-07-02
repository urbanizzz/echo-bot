{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module FrontEnd.Telegram
  ( run
  , Handle
  , textFromMessage
  , messageFromText
  )
where

import qualified Data.Text as T
import qualified EchoBot
import Logger ((.<))
import qualified Logger
import Control.Monad (forever)
import Data.Aeson ((.=),(.:),(.:?),(.!=))
import Data.Aeson.QQ
import qualified Data.Aeson as A
import qualified Data.Map.Strict as M
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Text.Read (readMaybe)
import qualified Network.HTTP.Simple as Simple
import Network.HTTP.Client.Internal ( Request(method,  responseTimeout), ResponseTimeout(ResponseTimeoutNone) )
import Data.ByteString.Char8 (ByteString)
import qualified Network.HTTP.Client as LowLevel
import Data.Text.Encoding (decodeUtf8)
import Data.ByteString.Lazy (toStrict)

type Handle = EchoBot.Handle IO Message
type HandleMap = M.Map UserId Handle

type UpdateId = Int
type MessageId = Int
type UserId = Int
type ChatId = Int
type Message = T.Text
type RepetitionCount = Int

data Update = Update
  { updateId :: UpdateId
  , messageId :: MessageId
  , userId :: UserId
  , chatId :: ChatId
  , message :: Message
  } deriving Show
instance A.FromJSON Update where
  parseJSON = A.withObject "FromJSON Update" $ \o -> Update
    <$> (o .: "update_id")
    <*> (o .: "message" >>= (.: "message_id"))
    <*> (o .: "message" >>= (.: "from") >>= (.: "id"))
    <*> (o .: "message" >>= (.: "chat") >>= (.: "id"))
    <*> (o .: "message" >>= \o1 -> o1 .:? "text" .!= "")

data Updates = Updates
  { unUpdates :: [Update]
  }
instance A.FromJSON Updates where
  parseJSON = A.withObject "FromJSON Updates" $ \o -> Updates
    <$> (o .: "result")

tgTimeout ::String 
tgTimeout = "60"

textFromMessage :: Message -> Maybe T.Text
textFromMessage = Just

messageFromText :: T.Text -> Message
messageFromText = id

getUpdates :: Handle -> UpdateId -> IO [Update]
getUpdates h lastUpdateId = do
  Logger.logDebug (EchoBot.hLogHandle h) $ "From Telegram.getUpdates: calling getUpdates"
  let requestObject = A.object
        [ "offset" .= show lastUpdateId
        , "timeout" .= tgTimeout
        ] 
  rawJSON <- useMethod h "getUpdates" requestObject
  let eitherResult = A.eitherDecodeStrict' rawJSON :: Either String Updates
  result <- either logErr (pure . unUpdates) eitherResult
  pure result
  where
    logErr err = do
      Logger.logWarning (EchoBot.hLogHandle h) $ "From Telegram.getUpdates: " .< (T.pack err)
      pure []

insertNewUser :: UserId -> Handle -> HandleMap -> HandleMap
insertNewUser = M.insert

proceedUpdate :: Handle -> IORef HandleMap -> Update -> IO UpdateId
proceedUpdate newUserHandle handleMapRef update = do
  handleMap <- readIORef handleMapRef
  modifyIORef' handleMapRef $ 
    if (userId update) `M.notMember` handleMap
      then insertNewUser (userId update) newUserHandle
      else id
  handleMap' <- readIORef handleMapRef
  let handle = handleMap' M.! (userId update)
  response <- EchoBot.respond handle (EchoBot.MessageEvent (message update))
  mapM_ (handleResponse handle update) response
  pure $ updateId update

run :: Handle -> IO ()
run newUserHandle = do
  handleMapRef <- newIORef (M.empty :: HandleMap)
  lastUpdateIdRef <- newIORef (0 :: Int)
  forever $ do
    lastUpdateId <- readIORef lastUpdateIdRef
    Logger.logDebug (EchoBot.hLogHandle newUserHandle) $ "From Telegram.run: current lastUpdateId is " .< (T.pack . show $ lastUpdateId)
    updates <- getUpdates newUserHandle lastUpdateId
    updateIds <- mapM (proceedUpdate newUserHandle handleMapRef) updates
    let newLastUpdateId = if null updateIds
                          then lastUpdateId
                          else 1 + (last updateIds)
    modifyIORef' lastUpdateIdRef (\_ -> newLastUpdateId)

useMethod :: Handle -> String -> A.Value -> IO ByteString
useMethod h methodName requestObject = do
  let url = mconcat
            [ EchoBot.confBotURL . EchoBot.hConfig $ h
            , methodName
            ]
  initialRequest <- Simple.parseRequest url
  let requestWithBody = Simple.setRequestBodyJSON requestObject initialRequest
  let request = requestWithBody
        { method = "POST"
        , responseTimeout = ResponseTimeoutNone
        }
  Logger.logDebug (EchoBot.hLogHandle h) $ "From Telegram.useMethod: requestBody is " .< (showBody . LowLevel.requestBody $ request)
  result <- Simple.httpBS request
  pure $ Simple.getResponseBody result


showBody :: LowLevel.RequestBody -> T.Text
showBody (LowLevel.RequestBodyLBS bs) = decodeUtf8 . toStrict $ bs
showBody (LowLevel.RequestBodyBS bs) = decodeUtf8 bs
showBody _ = "no RequestBody"

copyMessage :: Handle -> Update -> IO ()
copyMessage h update = do
  Logger.logDebug (EchoBot.hLogHandle h) $ "From Telegram.copyMessage: calling copyMessage"
  let requestObject = A.object
        [ "chat_id" .= chatId update
        , "from_chat_id" .= chatId update
        , "message_id" .= messageId update
        ] 
  _ <- useMethod h "copyMessage" requestObject
  pure ()

sendMessage :: Handle -> Update -> Message -> IO ()
sendMessage h update msg = do
  Logger.logDebug (EchoBot.hLogHandle h) $ "From Telegram.sendMessage: calling sendMessage"
  let requestObject = A.object
        [ "chat_id" .= chatId update
        , "text" .= msg
        ]
  _ <- useMethod h "sendMessage" requestObject
  pure ()

deleteMessage :: Handle -> Update -> IO ()
deleteMessage h update = do
  Logger.logDebug (EchoBot.hLogHandle h) $ "From Telegram.deleteMessage: calling deleteMessage"
  let requestObject = A.object
        [ "chat_id" .= chatId update
        , "message_id" .= messageId update
        ]
  _ <- useMethod h "deleteMessage" requestObject
  pure ()

keySet :: A.Value
keySet = [aesonQQ| {"one_time_keyboard":true,"keyboard":[[{"text":"1"},{"text":"2"},{"text":"3"},{"text":"4"},{"text":"5"}]]} |]

getNumber :: Handle -> Update -> Message -> IO T.Text
getNumber h update title = do
  Logger.logDebug (EchoBot.hLogHandle h) $ "From Telegram.getNumber: calling getNumber"
  let requestObject = A.object
        [ "chat_id" .= chatId update
        , "text" .= title
        , "reply_markup" .= keySet
        ]
  _ <- useMethod h "sendMessage" requestObject
  updates <- getUpdates h (1 + updateId update)
  Logger.logDebug (EchoBot.hLogHandle h) $ "From Telegram.getNumber: last update is " .< (T.pack . show $ updates)
  if null updates
    then pure "0"
    else do
      _ <- deleteMessage h . last $ updates
      pure . message . last $ updates

handleResponse :: Handle -> Update -> EchoBot.Response Message -> IO ()
handleResponse h update (EchoBot.MessageResponse x) = if (message update == x) 
  then copyMessage h update
  else sendMessage h update x
handleResponse h update (EchoBot.MenuResponse title options) = do
  rawText <- getNumber h update title
  let number = getNumberFromRawText rawText
  let maybeEvent = M.lookup number $ M.fromList options
  _ <- maybe (pure []) (EchoBot.respond h) maybeEvent
  pure ()

getNumberFromRawText :: T.Text -> RepetitionCount
getNumberFromRawText text = case (readMaybe . T.unpack $ text) of
  Just x -> x
  _      -> 0

